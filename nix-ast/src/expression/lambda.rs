use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{BindingName, Error, Expression, expect_next_token_or_error, parse_expression_inner};

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
                let Some(set) = LambdaParametersSet::parse(stream)? else {
                    return Ok(None);
                };
                expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
                Ok(Some(LambdaParameters::Set(set)))
            }
            Some(Ok(Token::Ident(ident))) => Ok(Some(LambdaParameters::Named(ident))),
            _ => Ok(None),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LambdaParametersSet<'a> {
    pub inner: BTreeMap<Vec<BindingName<'a>>, LambdaParameter<'a>>,
}

impl<'a> LambdaParametersSet<'a> {
    fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Option<Self>, Error> {
        let mut inner = BTreeMap::new();

        loop {
            if stream.peek().map(Result::as_ref).transpose().unwrap() == Some(&Token::BraceClose) {
                break;
            }

            let ident = parse_binding_name(stream)?;

            let default = if matches!(stream.peek(), Some(Ok(Token::Question))) {
                let _token = stream.next();
                let default = parse_expression_inner(stream, u8::MAX)?;
                Some(default)
            } else {
                None
            };

            inner.insert(ident, LambdaParameter { default });

            match stream.peek() {
                Some(Ok(Token::BraceClose)) => break,
                Some(Ok(Token::Comma)) => {
                    stream.next();
                }
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

        Ok(Some(LambdaParametersSet { inner }))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LambdaParameter<'a> {
    pub default: Option<Expression<'a>>,
}

fn parse_binding_name<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
) -> Result<Vec<BindingName<'a>>, Error> {
    let mut name = Vec::new();

    loop {
        match stream.peek() {
            Some(Ok(Token::Ident(ident))) => {
                name.push(BindingName::Value(ident));
                stream.next();
            }
            Some(Ok(Token::InterpolationStart)) => {
                stream.next();
                name.push(BindingName::Lazy(parse_expression_inner(stream, u8::MAX)?));
                expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            }
            Some(Ok(Token::Comma | Token::Question | Token::BraceClose)) => return Ok(name),
            Some(Ok(token)) => {
                return Err(Error::UnexpectedToken(
                    token.into(),
                    vec![
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::Comma,
                        TokenDiscriminants::Question,
                        TokenDiscriminants::BraceClose,
                    ],
                ));
            }
            Some(Err(())) => panic!(),
            None => return Err(Error::UnexpectedEndOfFile),
        }
    }
}
