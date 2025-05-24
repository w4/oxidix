use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{
    BindingName, Error, Expression, expect_next_token_or_error, parse_binding_name,
    parse_expression_inner,
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct AttrsetExpression<'a> {
    pub recursive: bool,
    pub bindings: BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
}

impl<'a> AttrsetExpression<'a> {
    pub fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Self, Error> {
        let mut bindings = BTreeMap::new();

        loop {
            match stream.peek() {
                Some(Ok(Token::BraceClose)) | None => break,
                Some(Ok(Token::Inherit)) => parse_inherits(stream, &mut bindings)?,
                Some(Ok(Token::Ident(_) | Token::String(_) | Token::InterpolationStart)) => {
                    parse_key_value(stream, &mut bindings)?
                }
                Some(Ok(v)) => {
                    return Err(Error::UnexpectedToken(
                        v.into(),
                        vec![TokenDiscriminants::BraceClose, TokenDiscriminants::Ident],
                    ));
                }
                Some(Err(())) => panic!(),
            }
        }

        expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;

        Ok(AttrsetExpression {
            recursive: false,
            bindings,
        })
    }
}

pub fn parse_key_value<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), Error> {
    let key = parse_binding_name(stream)?;
    let value = parse_expression_inner(stream, u8::MAX)?;
    expect_next_token_or_error(stream, TokenDiscriminants::Semicolon)?;

    if bindings.insert(key.clone(), value).is_some() {
        return Err(Error::AttributeAlreadyDefined(format!("{key:?}")));
    }

    Ok(())
}

pub fn parse_inherits<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), Error> {
    expect_next_token_or_error(stream, TokenDiscriminants::Inherit)?;

    match stream.peek() {
        Some(Ok(Token::Ident(_))) => parse_inherits_no_from(stream, bindings)?,
        Some(Ok(Token::BracketOpen)) => parse_inherits_with_from(stream, bindings)?,
        Some(Ok(Token::Semicolon)) => {}
        Some(Ok(v)) => {
            return Err(Error::UnexpectedToken(
                v.into(),
                vec![TokenDiscriminants::Ident, TokenDiscriminants::BracketOpen],
            ));
        }
        Some(Err(())) => panic!(),
        None => return Err(Error::UnexpectedEndOfFile),
    };

    Ok(())
}

pub fn parse_inherits_no_from<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), Error> {
    loop {
        match stream.next() {
            Some(Ok(Token::Ident(v))) => {
                if bindings
                    .insert(vec![BindingName::Value(v)], Expression::Ident(v))
                    .is_some()
                {
                    return Err(Error::AttributeAlreadyDefined(v.to_string()));
                }
            }
            Some(Ok(Token::Semicolon)) => break,
            Some(Ok(v)) => {
                return Err(Error::UnexpectedToken(
                    v.into(),
                    vec![TokenDiscriminants::Ident, TokenDiscriminants::Semicolon],
                ));
            }
            Some(Err(())) => panic!(),
            None => return Err(Error::UnexpectedEndOfFile),
        }
    }

    Ok(())
}

pub fn parse_inherits_with_from<'a>(
    stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), Error> {
    expect_next_token_or_error(stream, TokenDiscriminants::BracketOpen)?;

    let binding = parse_expression_inner(stream, u8::MAX)?;

    expect_next_token_or_error(stream, TokenDiscriminants::BracketClose)?;

    loop {
        match stream.next() {
            Some(Ok(Token::Ident(v))) => {
                if bindings
                    .insert(vec![BindingName::Value(v)], binding.clone())
                    .is_some()
                {
                    return Err(Error::AttributeAlreadyDefined(v.to_string()));
                }
            }
            Some(Ok(Token::Semicolon)) => break,
            Some(Ok(v)) => {
                return Err(Error::UnexpectedToken(
                    v.into(),
                    vec![TokenDiscriminants::Ident, TokenDiscriminants::Semicolon],
                ));
            }
            Some(Err(())) => panic!(),
            None => return Err(Error::UnexpectedEndOfFile),
        }
    }

    Ok(())
}
