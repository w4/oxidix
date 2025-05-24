use std::{
    collections::{BTreeMap, HashMap},
    iter::Peekable,
};

use nix_lexer::{Lexer, Logos, Token, TokenDiscriminants};
use thiserror::Error;

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum Expression<'a> {
    Bool(bool),
    Int(&'a str),
    Add(Box<(Expression<'a>, Expression<'a>)>),
    Let(Box<LetExpression<'a>>),
    Ident(&'a str),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,
    #[error("Unexpected token {0:?} expected one of {1:?}")]
    UnexpectedToken(TokenDiscriminants, Vec<TokenDiscriminants>),
}

pub fn parse_expression<'a>(stream: Lexer<'a, Token<'a>>) -> Result<Expression<'a>, Error> {
    parse_expression_inner(&mut stream.peekable())
}

pub fn parse_expression_inner<'a, 'b: 'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
) -> Result<Expression<'a>, Error> {
    let token = stream.next().ok_or(Error::UnexpectedEndOfFile)?.unwrap();

    match token {
        Token::Bool(v) => Ok(Expression::Bool(v)),
        Token::Let => Ok(Expression::Let(Box::new(parse_let_expression(stream)?))),
        Token::In => todo!("in"),
        Token::If => todo!("if"),
        Token::Else => todo!("else"),
        Token::Then => todo!("then"),
        Token::Int(v) => Ok(Expression::Int(v)),
        Token::Float(_) => todo!("float"),
        Token::Ident(v) => Ok(Expression::Ident(v)),
        Token::Dot => todo!("dot"),
        Token::Equals => todo!("equals"),
        Token::DoubleEquals => todo!("double equals"),
        Token::Ge => todo!("ge"),
        Token::Le => todo!("le"),
        Token::Lt => todo!("lt"),
        Token::Gt => todo!("gt"),
        Token::Or => todo!("or"),
        Token::And => todo!("and"),
        Token::Semicolon => todo!("semi"),
        Token::Plus => todo!("plus"),
        Token::Bang => todo!("bang"),
        Token::Neg => todo!("neg"),
        Token::SlashSlash => todo!("ss"),
        Token::BraceOpen => todo!("bo"),
        Token::BraceClose => todo!("bc"),
        Token::BracketOpen => todo!("bracket open"),
        Token::BracketClose => todo!("bracket close"),
        Token::SquareBracketOpen => todo!("sqb open"),
        Token::SquareBracketClose => todo!("sqb close"),
        Token::BlockComment => todo!("bl comm"),
        Token::InlineComment => todo!("in comm"),
        Token::Colon => todo!("colon"),
        Token::Inherit => todo!("inherit"),
        Token::Rec => todo!("rec"),
        Token::Question => todo!("quest"),
        Token::Dollar => todo!("dollar"),
        Token::InterpolationStart => todo!("interp start"),
        Token::String(string_tokens) => todo!("str"),
        Token::MultilineString(string_tokens) => todo!("multi"),
        Token::Comma => todo!("comma"),
        Token::Slash => todo!("slash"),
        Token::Throw => todo!("throw"),
        Token::At => todo!("at"),
        Token::Asterisk => todo!("aster"),
        Token::Path(_) => todo!("path"),
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum BindingName<'a> {
    Lazy(Expression<'a>),
    Value(&'a str),
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LetExpression<'a> {
    pub bindings: BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
    pub inner: Expression<'a>,
}

fn parse_let_expression<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
) -> Result<LetExpression<'a>, Error> {
    let mut bindings = BTreeMap::new();

    let inner = loop {
        match stream.peek() {
            Some(Ok(Token::In)) => {
                stream.next();
                break parse_expression_inner(stream)?;
            }
            Some(Ok(Token::Ident(_))) => {
                let name = parse_binding_name(stream)?;
                let value = parse_expression_inner(stream)?;
                expect_next_token_or_error(stream, TokenDiscriminants::Semicolon)?;
                bindings.insert(name, value);
            }
            Some(Ok(token)) => {
                return Err(Error::UnexpectedToken(
                    token.into(),
                    vec![
                        TokenDiscriminants::In,
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::Inherit,
                    ],
                ));
            }
            Some(Err(())) => panic!(),
            None => return Err(Error::UnexpectedEndOfFile),
        }
    };

    Ok(LetExpression { bindings, inner })
}

fn parse_binding_name<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
) -> Result<Vec<BindingName<'a>>, Error> {
    let mut name = Vec::new();

    loop {
        match stream.next() {
            Some(Ok(Token::Ident(ident))) => name.push(BindingName::Value(ident)),
            Some(Ok(Token::InterpolationStart)) => {
                name.push(BindingName::Lazy(parse_expression_inner(stream)?));
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
    use nix_lexer::Logos as _;

    use crate::parse_expression;

    #[test]
    fn test() {
        let lex = nix_lexer::Token::lexer("let x = 5; in x");
        let actual = parse_expression(lex).unwrap();
        panic!("{actual:#?}");
    }
}
