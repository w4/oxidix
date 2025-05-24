use std::iter::Peekable;

use nix_lexer::{SpannedIter, Token};

use crate::{Error, Expression, HandleStreamError, SpannedError, parse_expression_inner};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct UnaryExpression<'a> {
    pub operator: UnaryOperator,
    pub expression: Expression<'a>,
}

impl<'a> UnaryExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    ) -> Result<Option<Self>, SpannedError> {
        let (token, _) = stream
            .peek()
            .ok_or(Error::UnexpectedEndOfFile.with_span(0..0))?
            .span_error()?;

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
