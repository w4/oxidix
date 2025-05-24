use std::iter::Peekable;

use nix_lexer::{Lexer, Token};

use crate::{Error, Expression, parse_expression_inner};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct UnaryExpression<'a> {
    pub operator: UnaryOperator,
    pub expression: Expression<'a>,
}

impl<'a> UnaryExpression<'a> {
    pub fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<Option<Self>, Error> {
        let token = stream
            .peek()
            .ok_or(Error::UnexpectedEndOfFile)?
            .as_ref()
            .unwrap();

        let Some(operator) = UnaryOperator::parse(token) else {
            return Ok(None);
        };

        stream.next();

        let expression = parse_expression_inner(stream, operator.precedence())?;

        Ok(Some(UnaryExpression {
            operator,
            expression,
        }))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum UnaryOperator {
    ArithmeticNegation,
    LogicalNot,
}

impl UnaryOperator {
    pub fn parse(token: &Token<'_>) -> Option<Self> {
        match token {
            Token::Bang => Some(Self::LogicalNot),
            Token::Neg => Some(Self::ArithmeticNegation),
            _ => None,
        }
    }

    pub fn precedence(self) -> u8 {
        match self {
            Self::ArithmeticNegation => 3,
            Self::LogicalNot => 8,
        }
    }
}
