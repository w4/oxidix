use std::iter::Peekable;

use nix_lexer::{SpannedIter, Token, TokenDiscriminants};

use crate::{
    Error, Expression, HandleStreamError, SpannedError, expect_next_token_or_error,
    parse_expression_inner,
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
    pub postfix: Option<Expression<'a>>,
}

impl<'a> BinaryExpression<'a> {
    pub fn parse(
        stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
        min_prec: u8,
    ) -> Result<Option<(BinaryOperator, Expression<'a>, Option<Expression<'a>>)>, SpannedError>
    {
        let (token, _) = match stream.peek() {
            Some(token) => token.span_error()?,
            None => return Ok(None),
        };

        let Some(operator) = BinaryOperator::parse(token) else {
            return Ok(None);
        };

        let prec = operator.precedence();

        if prec > min_prec {
            return Ok(None);
        }

        // don't chomp the token for juxtaposition-based function application
        if operator != BinaryOperator::FunctionApplication {
            stream.next();
        }

        let next_prec = match operator.associativity() {
            Some(BinaryAssociativity::Left) => prec.saturating_sub(1),
            Some(BinaryAssociativity::Right) => prec,
            None => u8::MAX,
        };

        let right = match operator {
            BinaryOperator::AttributeSelection => parse_binary_expression(stream, next_prec)?,
            _ => parse_expression_inner(stream, next_prec)?,
        };

        let postfix = match operator {
            BinaryOperator::AttributeSelection => {
                if let Some((Ok(Token::TextOr), _)) = stream.peek() {
                    stream.next();
                    Some(parse_expression_inner(stream, next_prec)?)
                } else {
                    None
                }
            }
            _ => None,
        };

        Ok(Some((operator, right, postfix)))
    }
}

fn parse_binary_expression<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    next_prec: u8,
) -> Result<Expression<'a>, SpannedError> {
    match stream.next().span_error()? {
        (Token::Ident(v), _) => Ok(Expression::Ident(v)),
        (Token::InterpolationStart, _) => {
            let expr = parse_expression_inner(stream, next_prec)?;
            expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            Ok(expr)
        }
        (Token::String(_), _) => todo!(),
        (v, span) => Err(Error::UnexpectedToken(
            v.into(),
            vec![
                TokenDiscriminants::Ident,
                TokenDiscriminants::InterpolationStart,
                TokenDiscriminants::String,
                TokenDiscriminants::Or,
            ],
        )
        .with_span(span)),
    }
}

// https://nix.dev/manual/nix/2.29/language/operators.html
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum BinaryOperator {
    AttributeSelection,
    FunctionApplication,
    HasAttribute,
    ListConcatenation,
    Multiplication,
    Division,
    Subtraction,
    Addition,
    Update,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    LogicalAnd,
    LogicalOr,
    LogicalImplication,
    LeftAssocPipeOperator,
    RightAssocPipeOperator,
}

impl BinaryOperator {
    pub fn precedence(self) -> u8 {
        match self {
            Self::AttributeSelection => 1,
            Self::FunctionApplication => 2,
            // 3 = unary arithmetic negation
            Self::HasAttribute => 4,
            Self::ListConcatenation => 5,
            Self::Multiplication | Self::Division => 6,
            Self::Subtraction | Self::Addition => 7,
            // 8 = unary logical NOT
            Self::Update => 9,
            Self::Lt | Self::Le | Self::Gt | Self::Ge | Self::Eq | Self::Ne => 10,
            Self::LogicalAnd => 12,
            Self::LogicalOr => 13,
            Self::LogicalImplication => 14,
            Self::LeftAssocPipeOperator | Self::RightAssocPipeOperator => 15,
        }
    }

    pub fn parse(token: &Token<'_>) -> Option<Self> {
        match token {
            Token::Dot => Some(Self::AttributeSelection),
            Token::Question => Some(Self::HasAttribute),
            Token::PlusPlus => Some(Self::ListConcatenation),
            Token::Asterisk => Some(Self::Multiplication),
            Token::Slash => Some(Self::Division),
            Token::Neg => Some(Self::Subtraction),
            Token::Plus => Some(Self::Addition),
            Token::SlashSlash => Some(Self::Update),
            Token::Lt => Some(Self::Lt),
            Token::Le => Some(Self::Le),
            Token::Gt => Some(Self::Gt),
            Token::Ge => Some(Self::Ge),
            Token::DoubleEquals => Some(Self::Eq),
            Token::BangEquals => Some(Self::Ne),
            Token::And => Some(Self::LogicalAnd),
            Token::Or => Some(Self::LogicalOr),
            Token::Arrow => Some(Self::LogicalImplication),
            Token::PipeRight => Some(Self::LeftAssocPipeOperator),
            Token::PipeLeft => Some(Self::RightAssocPipeOperator),
            Token::If
            | Token::Then
            | Token::Else
            | Token::Let
            | Token::In
            | Token::Semicolon
            | Token::BraceClose
            | Token::BracketClose
            | Token::SquareBracketClose
            | Token::Comma => None,
            _ => Some(Self::FunctionApplication),
        }
    }

    pub fn associativity(self) -> Option<BinaryAssociativity> {
        match self {
            Self::AttributeSelection
            | Self::HasAttribute
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::Eq
            | Self::Ne => None,
            Self::FunctionApplication
            | Self::Multiplication
            | Self::Division
            | Self::Subtraction
            | Self::Addition
            | Self::LogicalAnd
            | Self::LogicalOr
            | Self::LeftAssocPipeOperator => Some(BinaryAssociativity::Left),
            Self::ListConcatenation
            | Self::Update
            | Self::LogicalImplication
            | Self::RightAssocPipeOperator => Some(BinaryAssociativity::Right),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum BinaryAssociativity {
    Left,
    Right,
}
