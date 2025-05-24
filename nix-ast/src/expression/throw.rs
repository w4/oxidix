use std::iter::Peekable;

use nix_lexer::{SpannedIter, Token};

use crate::{Expression, SpannedError, parse_primary};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ThrowExpression<'a> {
    pub expression: Expression<'a>,
}

impl<'a> ThrowExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    ) -> Result<ThrowExpression<'a>, SpannedError> {
        Ok(Self {
            expression: parse_primary(stream)?,
        })
    }
}
