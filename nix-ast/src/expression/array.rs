use std::iter::Peekable;

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{Error, Expression, parse_primary};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct ArrayExpression<'a> {
    pub values: Vec<Expression<'a>>,
}

impl<'a> ArrayExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<Lexer<'a, Token<'a>>>,
    ) -> Result<ArrayExpression<'a>, Error> {
        let mut values = Vec::new();

        loop {
            let token = stream
                .peek()
                .ok_or(Error::UnexpectedEndOfFile)?
                .as_ref()
                .unwrap();

            if TokenDiscriminants::from(token) == TokenDiscriminants::SquareBracketClose {
                break;
            }

            values.push(parse_primary(stream)?);
        }

        Ok(ArrayExpression { values })
    }
}
