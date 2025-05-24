use std::iter::Peekable;

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{Error, Expression, expect_next_token_or_error, parse_expression_inner};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct IfExpression<'a> {
    pub condition: Expression<'a>,
    pub then: Expression<'a>,
    pub r#else: Expression<'a>,
}

impl<'a> IfExpression<'a> {
    pub fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<IfExpression<'a>, Error> {
        let condition = parse_expression_inner(stream, u8::MAX)?;
        expect_next_token_or_error(stream, TokenDiscriminants::Then)?;
        let then = parse_expression_inner(stream, u8::MAX)?;
        expect_next_token_or_error(stream, TokenDiscriminants::Else)?;
        let r#else = parse_expression_inner(stream, u8::MAX)?;

        Ok(IfExpression {
            condition,
            then,
            r#else,
        })
    }
}
