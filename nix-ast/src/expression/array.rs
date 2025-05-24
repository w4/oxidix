use std::iter::Peekable;

use nix_lexer::{SpannedIter, Token, TokenDiscriminants};

use crate::{Expression, HandleStreamError, SpannedError, parse_primary};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct ArrayExpression<'a> {
    pub values: Vec<Expression<'a>>,
}

impl<'a> ArrayExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    ) -> Result<ArrayExpression<'a>, SpannedError> {
        let mut values = Vec::new();

        loop {
            let (token, _) = stream.peek().span_error()?;

            if TokenDiscriminants::from(token) == TokenDiscriminants::SquareBracketClose {
                break;
            }

            values.push(parse_primary(stream)?);
        }

        Ok(ArrayExpression { values })
    }
}
