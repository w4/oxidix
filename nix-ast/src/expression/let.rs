use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{SpannedIter, Token, TokenDiscriminants};

use crate::{
    BindingName, Error, Expression, HandleStreamError, SpannedError, expect_next_token_or_error,
    parse_binding_name, parse_expression_inner,
};

use super::attrset::parse_inherits;

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LetExpression<'a> {
    pub bindings: BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub inner: Expression<'a>,
}

impl<'a> LetExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    ) -> Result<LetExpression<'a>, SpannedError> {
        let mut bindings = BTreeMap::new();

        let inner = loop {
            match stream.peek().span_error()? {
                (Token::In, _) => {
                    stream.next();
                    break parse_expression_inner(stream, u8::MAX)?;
                }
                (Token::Ident(_), _) => {
                    let name = parse_binding_name(stream)?;
                    let value = parse_expression_inner(stream, u8::MAX)?;
                    expect_next_token_or_error(stream, TokenDiscriminants::Semicolon)?;
                    bindings.insert(name, value);
                }
                (Token::Inherit, _) => {
                    parse_inherits(stream, &mut bindings)?;
                }
                (token, span) => {
                    return Err(Error::UnexpectedToken(
                        token.into(),
                        vec![
                            TokenDiscriminants::In,
                            TokenDiscriminants::Ident,
                            TokenDiscriminants::Inherit,
                        ],
                    )
                    .with_span(span));
                }
            }
        };

        Ok(LetExpression { bindings, inner })
    }
}
