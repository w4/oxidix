use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{Span, SpannedIter, Token, TokenDiscriminants};

use crate::{
    BindingName, Error, Expression, HandleStreamError, SpannedError, expect_next_token_or_error,
    parse_binding_name, parse_expression_inner,
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AttrsetExpression<'a> {
    pub recursive: bool,
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub bindings: BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
}

impl<'a> AttrsetExpression<'a> {
    pub fn parse(stream: &mut Peekable<SpannedIter<'a, Token<'a>>>) -> Result<Self, SpannedError> {
        let mut bindings = BTreeMap::new();

        loop {
            match stream.peek().span_error()? {
                (Token::BraceClose, _) => break,
                (Token::Inherit, _) => parse_inherits(stream, &mut bindings)?,
                (Token::Ident(_) | Token::String(_) | Token::InterpolationStart, span) => {
                    parse_key_value(stream, &mut bindings, span)?
                }
                (v, span) => {
                    return Err(Error::UnexpectedToken(
                        v.into(),
                        vec![TokenDiscriminants::BraceClose, TokenDiscriminants::Ident],
                    )
                    .with_span(span));
                }
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
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
    span: Span,
) -> Result<(), SpannedError> {
    let key = parse_binding_name(stream)?;
    let value = parse_expression_inner(stream, u8::MAX)?;
    expect_next_token_or_error(stream, TokenDiscriminants::Semicolon)?;

    if bindings.insert(key.clone(), value).is_some() {
        return Err(Error::AttributeAlreadyDefined(format!("{key:?}")).with_span(span));
    }

    Ok(())
}

pub fn parse_inherits<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), SpannedError> {
    expect_next_token_or_error(stream, TokenDiscriminants::Inherit)?;

    match stream.peek().span_error()? {
        (Token::Ident(_), _) => parse_inherits_no_from(stream, bindings)?,
        (Token::BracketOpen, _) => parse_inherits_with_from(stream, bindings)?,
        (Token::Semicolon, _) => {}
        (v, span) => {
            return Err(Error::UnexpectedToken(
                v.into(),
                vec![TokenDiscriminants::Ident, TokenDiscriminants::BracketOpen],
            )
            .with_span(span));
        }
    };

    Ok(())
}

pub fn parse_inherits_no_from<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), SpannedError> {
    loop {
        match stream.next().span_error()? {
            (Token::Ident(v), span) => {
                if bindings
                    .insert(vec![BindingName::Value(v)], Expression::Ident(v))
                    .is_some()
                {
                    return Err(Error::AttributeAlreadyDefined(v.to_string()).with_span(span));
                }
            }
            (Token::Semicolon, _) => break,
            (v, span) => {
                return Err(Error::UnexpectedToken(
                    v.into(),
                    vec![TokenDiscriminants::Ident, TokenDiscriminants::Semicolon],
                )
                .with_span(span));
            }
        }
    }

    Ok(())
}

pub fn parse_inherits_with_from<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    bindings: &mut BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
) -> Result<(), SpannedError> {
    expect_next_token_or_error(stream, TokenDiscriminants::BracketOpen)?;

    let binding = parse_expression_inner(stream, u8::MAX)?;

    expect_next_token_or_error(stream, TokenDiscriminants::BracketClose)?;

    loop {
        match stream.next().span_error()? {
            (Token::Ident(v), span) => {
                if bindings
                    .insert(vec![BindingName::Value(v)], binding.clone())
                    .is_some()
                {
                    return Err(Error::AttributeAlreadyDefined(v.to_string()).with_span(span));
                }
            }
            (Token::Semicolon, _) => break,
            (v, span) => {
                return Err(Error::UnexpectedToken(
                    v.into(),
                    vec![TokenDiscriminants::Ident, TokenDiscriminants::Semicolon],
                )
                .with_span(span));
            }
        }
    }

    Ok(())
}
