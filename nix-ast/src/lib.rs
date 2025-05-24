mod error;
mod expression;

use std::{iter::Peekable, path::Path};

use ariadne::Source;
pub use error::{Error, SpannedError};
use error::{HandleStreamError, WithSpan};
use expression::{
    ArrayExpression, AttrsetExpression, BinaryExpression, IfExpression, LambdaExpression,
    LetExpression, UnaryExpression,
};
use nix_lexer::{Lexer, SpannedIter, Token, TokenDiscriminants};

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
    Attrset(AttrsetExpression<'a>),
}

pub fn parse_expression<'a>(stream: Lexer<'a, Token<'a>>) -> Result<Expression<'a>, SpannedError> {
    parse_expression_inner(&mut stream.spanned().peekable(), u8::MAX)
}

fn parse_expression_inner<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    min_prec: u8,
) -> Result<Expression<'a>, SpannedError> {
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

fn parse_primary<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
) -> Result<Expression<'a>, SpannedError> {
    let (token, span) = stream.next().span_error()?;

    match token {
        Token::Bool(v) => Ok(Expression::Bool(v)),
        Token::Let => Ok(Expression::Let(Box::new(LetExpression::parse(stream)?))),
        Token::If => Ok(Expression::If(Box::new(IfExpression::parse(stream)?))),
        Token::Int(v) => Ok(Expression::Int(
            v.parse().map_err(Error::ParseInt).with_span(span)?,
        )),
        Token::Float(v) => Ok(Expression::Float(WrappedFloat(
            v.parse().map_err(Error::ParseFloat).with_span(span)?,
        ))),
        Token::Ident(v) => Ok(Expression::Ident(v)),
        Token::BracketOpen => wrapped_interpolated(stream, TokenDiscriminants::BracketClose),
        Token::InterpolationStart => wrapped_interpolated(stream, TokenDiscriminants::BraceClose),
        Token::SquareBracketOpen => Ok(Expression::Array(ArrayExpression::parse(stream)?)),
        Token::BraceOpen => Ok(Expression::Attrset(AttrsetExpression::parse(stream)?)),
        Token::Rec => Ok(Expression::Attrset(AttrsetExpression {
            recursive: true,
            ..AttrsetExpression::parse(stream)?
        })),
        Token::Dollar => todo!("dollar"),
        Token::String(_string_tokens) => todo!("str"),
        Token::MultilineString(_string_tokens) => todo!("multi"),
        Token::Throw => todo!("throw"),
        Token::Path(_) => todo!("path"),
        Token::BlockComment | Token::InlineComment => Ok(Expression::Comment),
        v => Err(Error::UnexpectedTopLevelToken(v.into()).with_span(span)),
    }
}

fn wrapped_interpolated<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    end: TokenDiscriminants,
) -> Result<Expression<'a>, SpannedError> {
    let node = parse_expression_inner(stream, u8::MAX)?;
    expect_next_token_or_error(stream, end)?;
    Ok(node)
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
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
) -> Result<Vec<BindingName<'a>>, SpannedError> {
    let mut name = Vec::new();

    loop {
        match stream.next().span_error()? {
            (Token::Ident(ident), _) => name.push(BindingName::Value(ident)),
            (Token::InterpolationStart, _) => {
                name.push(BindingName::Lazy(parse_expression_inner(stream, u8::MAX)?));
                expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            }
            (Token::Equals, _) => return Ok(name),
            (token, span) => {
                return Err(Error::UnexpectedToken(
                    token.into(),
                    vec![
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::InterpolationStart,
                        TokenDiscriminants::Equals,
                    ],
                )
                .with_span(span));
            }
        }
    }
}

fn expect_next_token_or_error<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    kind: TokenDiscriminants,
) -> Result<(), SpannedError> {
    match stream.next().span_error()? {
        (v, _) if TokenDiscriminants::from(&v) == kind => Ok(()),
        (v, span) => Err(Error::UnexpectedToken(v.into(), vec![kind]).with_span(span)),
    }
}

pub fn format_error(error: SpannedError, path: &Path) -> String {
    use ariadne::{Label, Report, ReportKind};

    let mut out = Vec::new();

    Report::build(ReportKind::Error, error.span.clone())
        .with_message("Syntax error")
        .with_label(Label::new(error.span).with_message(error.error))
        .finish()
        .write_for_stdout(
            Source::from(std::fs::read_to_string(path).unwrap()),
            &mut out,
        )
        .unwrap();

    String::from_utf8(out).unwrap()
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use insta::{assert_debug_snapshot, assert_snapshot, glob};
    use nix_lexer::Logos as _;

    use crate::{format_error, parse_expression};

    #[test]
    fn fixtures() {
        glob!("../fixtures", "*.nix", |path| {
            let input = read_to_string(path).expect("failed to read fixture");
            let lex = nix_lexer::Token::lexer(&input);
            match parse_expression(lex) {
                Ok(v) => assert_debug_snapshot!("fixture", v, &input),
                Err(e) => {
                    assert_snapshot!("fixture", strip_ansi_codes(&format_error(e, path)), &input)
                }
            }
        });
    }

    fn strip_ansi_codes(input: &str) -> String {
        let mut output = String::new();
        let mut chars = input.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\x1b' {
                if chars.peek() == Some(&'[') {
                    chars.next(); // Skip '['

                    // Skip all characters until a letter (end of ANSI sequence)
                    while let Some(&next) = chars.peek() {
                        if next.is_ascii_alphabetic() {
                            chars.next(); // consume the letter
                            break;
                        } else {
                            chars.next(); // skip numeric or separator
                        }
                    }
                }
            } else {
                output.push(c);
            }
        }

        output
    }
}
