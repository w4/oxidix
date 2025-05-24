use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{Error, Expression, expect_next_token_or_error, parse_expression_inner};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LambdaExpression<'a> {
    pub parameters: LambdaParameters<'a>,
    pub body: Expression<'a>,
}

impl<'a> LambdaExpression<'a> {
    pub fn parse(
        orig_stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    ) -> Result<Option<LambdaExpression<'a>>, Error> {
        // clone the stream so we can optimistically parse
        let mut stream = orig_stream.clone();

        let Some(parameters) = LambdaParameters::parse(&mut stream).transpose() else {
            return Ok(None);
        };

        let Some(Token::Colon) = stream.next().transpose().unwrap() else {
            return Ok(None);
        };

        // if we've confirmed we're in a lambda we can return any syntax errors from parsing parameters now
        let parameters = parameters?;

        let body = parse_expression_inner(&mut stream, u8::MAX)?;

        // since we've passed parsing, fast forward the stream that was passed to us
        *orig_stream = stream;

        Ok(Some(LambdaExpression { parameters, body }))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum LambdaParameters<'a> {
    Set(LambdaParametersSet<'a>),
    Named(&'a str),
}

impl<'a> LambdaParameters<'a> {
    fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Option<Self>, Error> {
        match stream.next() {
            Some(Ok(Token::BraceOpen)) => {
                Ok(LambdaParametersSet::parse(stream)?.map(LambdaParameters::Set))
            }
            Some(Ok(Token::Ident(ident))) => Ok(Some(LambdaParameters::Named(ident))),
            _ => Ok(None),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LambdaParametersSet<'a> {
    pub inner: BTreeMap<&'a str, LambdaParameter<'a>>,
    pub extra_ignored: bool,
    pub bound: Option<&'a str>,
}

impl<'a> LambdaParametersSet<'a> {
    fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Option<Self>, Error> {
        let mut inner = BTreeMap::new();
        let mut extra_ignored = false;

        loop {
            let ident = match stream.next() {
                Some(Ok(Token::Ident(ident))) => ident,
                Some(Ok(Token::BraceClose)) => break,
                Some(Ok(Token::DotDotDot)) => {
                    extra_ignored = true;
                    expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
                    break;
                }
                Some(Ok(v)) => {
                    return Err(Error::UnexpectedToken(
                        v.into(),
                        vec![
                            TokenDiscriminants::Ident,
                            TokenDiscriminants::BraceClose,
                            TokenDiscriminants::DotDotDot,
                        ],
                    ));
                }
                Some(Err(())) => panic!(),
                None => return Err(Error::UnexpectedEndOfFile),
            };

            let default = if matches!(stream.peek(), Some(Ok(Token::Question))) {
                let _token = stream.next();
                let default = parse_expression_inner(stream, u8::MAX)?;
                Some(default)
            } else {
                None
            };

            if inner.insert(ident, LambdaParameter { default }).is_some() {
                return Err(Error::DuplicateFunctionArgument(ident.to_string()));
            }

            match stream.next() {
                Some(Ok(Token::BraceClose)) => break,
                Some(Ok(Token::Comma)) => {}
                Some(Ok(v)) => {
                    return Err(Error::UnexpectedToken(
                        v.into(),
                        vec![
                            TokenDiscriminants::Question,
                            TokenDiscriminants::Comma,
                            TokenDiscriminants::BraceClose,
                        ],
                    ));
                }
                Some(Err(())) => panic!(),
                None => return Err(Error::UnexpectedEndOfFile),
            }
        }

        let bound = if matches!(stream.peek(), Some(Ok(Token::At))) {
            stream.next();

            match stream.next() {
                Some(Ok(Token::Ident(ident))) => Some(ident),
                Some(Ok(v)) => {
                    return Err(Error::UnexpectedToken(
                        v.into(),
                        vec![TokenDiscriminants::Ident],
                    ));
                }
                Some(Err(())) => panic!(),
                None => return Err(Error::UnexpectedEndOfFile),
            }
        } else {
            None
        };

        Ok(Some(LambdaParametersSet {
            inner,
            extra_ignored,
            bound,
        }))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LambdaParameter<'a> {
    pub default: Option<Expression<'a>>,
}
