mod expression;

use std::{
    iter::Peekable,
    num::{ParseFloatError, ParseIntError},
};

use expression::{
    ArrayExpression, BinaryExpression, IfExpression, LambdaExpression, LetExpression,
    UnaryExpression,
};
use nix_lexer::{Lexer, Token, TokenDiscriminants};
use thiserror::Error;

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum Expression<'a> {
    Bool(bool),
    Int(i64),
    Float(WrappedFloat),
    Add(Box<(Expression<'a>, Expression<'a>)>),
    Let(Box<LetExpression<'a>>),
    Ident(&'a str),
    If(Box<IfExpression<'a>>),
    Binary(Box<BinaryExpression<'a>>),
    Unary(Box<UnaryExpression<'a>>),
    Array(ArrayExpression<'a>),
    Comment,
    Lambda(Box<LambdaExpression<'a>>),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,
    #[error("Unexpected token {0:?}")]
    UnexpectedTopLevelToken(TokenDiscriminants),
    #[error("Unexpected token {0:?} expected one of {1:?}")]
    UnexpectedToken(TokenDiscriminants, Vec<TokenDiscriminants>),
    #[error("Invalid integer: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("Invalid float: {0}")]
    ParseFloat(#[from] ParseFloatError),
}

pub fn parse_expression<'a>(stream: Lexer<'a, Token<'a>>) -> Result<Expression<'a>, Error> {
    parse_expression_inner(&mut stream.peekable(), u8::MAX)
}

fn parse_expression_inner<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    min_prec: u8,
) -> Result<Expression<'a>, Error> {
    let mut left = if let Some(expr) = UnaryExpression::parse(stream)? {
        Expression::Unary(Box::new(expr))
    } else if let Some(lambda) = LambdaExpression::parse(stream)? {
        Expression::Lambda(Box::new(lambda))
    } else {
        parse_primary(stream)?
    };

    loop {
        let Some((operator, right, postfix)) = BinaryExpression::parse(stream, min_prec)? else {
            break;
        };

        left = Expression::Binary(Box::new(BinaryExpression {
            operator,
            left,
            right,
            postfix,
        }));
    }

    Ok(left)
}

fn parse_primary<'a>(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Expression<'a>, Error> {
    let token = stream.next().ok_or(Error::UnexpectedEndOfFile)?.unwrap();

    match token {
        Token::Bool(v) => Ok(Expression::Bool(v)),
        Token::Let => Ok(Expression::Let(Box::new(LetExpression::parse(stream)?))),
        Token::If => Ok(Expression::If(Box::new(IfExpression::parse(stream)?))),
        Token::Int(v) => Ok(Expression::Int(v.parse()?)),
        Token::Float(v) => Ok(Expression::Float(WrappedFloat(v.parse()?))),
        Token::Ident(v) => Ok(Expression::Ident(v)),
        Token::BracketOpen => {
            let node = parse_expression_inner(stream, u8::MAX)?;
            expect_next_token_or_error(stream, TokenDiscriminants::BracketClose)?;
            Ok(node)
        }
        Token::InterpolationStart => {
            let node = parse_expression_inner(stream, u8::MAX)?;
            expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            Ok(node)
        }
        Token::SquareBracketOpen => Ok(Expression::Array(ArrayExpression::parse(stream)?)),
        Token::BraceOpen => todo!("bo"),
        Token::BraceClose => todo!("bc"),
        Token::Colon => todo!("colon"),
        Token::Inherit => todo!("inherit"),
        Token::Rec => todo!("rec"),
        Token::Question => todo!("quest"),
        Token::Dollar => todo!("dollar"),
        Token::String(_string_tokens) => todo!("str"),
        Token::MultilineString(_string_tokens) => todo!("multi"),
        Token::Throw => todo!("throw"),
        Token::At => todo!("at"),
        Token::Asterisk => todo!("aster"),
        Token::Path(_) => todo!("path"),
        Token::BlockComment | Token::InlineComment => Ok(Expression::Comment),
        v => Err(Error::UnexpectedTopLevelToken(v.into())),
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct WrappedFloat(f64);

impl Eq for WrappedFloat {}

impl PartialOrd for WrappedFloat {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WrappedFloat {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum BindingName<'a> {
    Lazy(Expression<'a>),
    Value(&'a str),
}

fn parse_binding_name<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
) -> Result<Vec<BindingName<'a>>, Error> {
    let mut name = Vec::new();

    loop {
        match stream.next() {
            Some(Ok(Token::Ident(ident))) => name.push(BindingName::Value(ident)),
            Some(Ok(Token::InterpolationStart)) => {
                name.push(BindingName::Lazy(parse_expression_inner(stream, u8::MAX)?));
                expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            }
            Some(Ok(Token::Equals)) => return Ok(name),
            Some(Ok(token)) => {
                return Err(Error::UnexpectedToken(
                    token.into(),
                    vec![
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::InterpolationStart,
                        TokenDiscriminants::Equals,
                    ],
                ));
            }
            Some(Err(())) => panic!(),
            None => return Err(Error::UnexpectedEndOfFile),
        }
    }
}

fn expect_next_token_or_error<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    kind: TokenDiscriminants,
) -> Result<(), Error> {
    match stream.next() {
        Some(Ok(v)) if TokenDiscriminants::from(&v) == kind => Ok(()),
        Some(Ok(v)) => Err(Error::UnexpectedToken(v.into(), vec![kind])),
        Some(Err(())) => panic!(),
        None => Err(Error::UnexpectedEndOfFile),
    }
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use insta::{assert_debug_snapshot, glob};
    use nix_lexer::Logos as _;

    use crate::parse_expression;

    #[test]
    fn fixtures() {
        glob!("../fixtures", "*.nix", |path| {
            let input = read_to_string(path).expect("failed to read fixture");
            let lex = nix_lexer::Token::lexer(&input);
            let actual = parse_expression(lex).unwrap();
            assert_debug_snapshot!("fixture", actual, &input);
        });
    }
}
