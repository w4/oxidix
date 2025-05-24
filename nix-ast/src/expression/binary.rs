//! Binary expression parsing and representation.
//!
//! This module handles all binary operations in Nix, including arithmetic,
//! logical, comparison, and Nix-specific operations like attribute selection
//! and function application. It implements proper operator precedence and
//! associativity according to the Nix language specification.

use std::iter::Peekable;

use nix_lexer::{SpannedIter, Token, TokenDiscriminants};

use crate::{
    Error, Expression, HandleStreamError, SpannedError, expect_next_token_or_error,
    parse_expression_inner,
};

/// Represents a binary operation between two expressions.
///
/// Binary expressions form the backbone of most Nix computations, from simple
/// arithmetic to complex attribute manipulations. This struct handles all
/// binary operations with proper precedence and associativity.
///
/// # Examples
///
/// ```rust
/// // Arithmetic: 1 + 2
/// // Comparison: x == y
/// // Attribute selection: obj.attr
/// // Function application: func arg
/// ```
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct BinaryExpression<'a> {
    /// The binary operator being applied
    pub operator: BinaryOperator,
    /// The left operand expression
    pub left: Expression<'a>,
    /// The right operand expression
    pub right: Expression<'a>,
    /// Optional postfix expression (used for `or` in attribute selection)
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

/// Binary operators in the Nix language.
///
/// This enum represents all binary operators supported by Nix, ordered by
/// their precedence (lowest to highest). The precedence follows the official
/// Nix language specification.
///
/// # Precedence Order (lowest to highest)
///
/// 1. Attribute selection (`.`)
/// 2. Function application (juxtaposition)
/// 3. Has attribute (`?`)
/// 4. List concatenation (`++`)
/// 5. Multiplication (`*`), Division (`/`)
/// 6. Addition (`+`), Subtraction (`-`)
/// 7. Update (`//`)
/// 8. Comparisons (`<`, `<=`, `>`, `>=`, `==`, `!=`)
/// 9. Logical AND (`&&`)
/// 10. Logical OR (`||`)
/// 11. Logical implication (`->`)
/// 12. Pipe operators (`|>`, `<|`)
///
/// # Reference
///
/// See: https://nix.dev/manual/nix/2.29/language/operators.html
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum BinaryOperator {
    /// Attribute selection operator (`.`) - `obj.attr`
    AttributeSelection,

    /// Function application (juxtaposition) - `func arg`
    FunctionApplication,

    /// Has attribute operator (`?`) - `obj ? attr`
    HasAttribute,

    /// List concatenation operator (`++`) - `list1 ++ list2`
    ListConcatenation,

    /// Multiplication operator (`*`) - `a * b`
    Multiplication,

    /// Division operator (`/`) - `a / b`
    Division,

    /// Subtraction operator (`-`) - `a - b`
    Subtraction,

    /// Addition operator (`+`) - `a + b`
    Addition,

    /// Update operator (`//`) - `set1 // set2`
    Update,

    /// Less than operator (`<`) - `a < b`
    Lt,

    /// Less than or equal operator (`<=`) - `a <= b`
    Le,

    /// Greater than operator (`>`) - `a > b`
    Gt,

    /// Greater than or equal operator (`>=`) - `a >= b`
    Ge,

    /// Equality operator (`==`) - `a == b`
    Eq,

    /// Inequality operator (`!=`) - `a != b`
    Ne,

    /// Logical AND operator (`&&`) - `a && b`
    LogicalAnd,

    /// Logical OR operator (`||`) - `a || b`
    LogicalOr,

    /// Logical implication operator (`->`) - `a -> b`
    LogicalImplication,

    /// Left-associative pipe operator (`|>`) - `a |> b`
    LeftAssocPipeOperator,

    /// Right-associative pipe operator (`<|`) - `a <| b`
    RightAssocPipeOperator,
}

impl BinaryOperator {
    /// Returns the precedence level of this operator.
    ///
    /// Lower numbers indicate higher precedence (tighter binding).
    /// This follows the Nix language specification for operator precedence.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use nix_ast::expression::BinaryOperator;
    ///
    /// assert!(BinaryOperator::Multiplication.precedence() <
    ///         BinaryOperator::Addition.precedence());
    /// ```
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

    /// Attempts to parse a binary operator from a token.
    ///
    /// This method examines a token and determines if it represents a binary
    /// operator. Some tokens (like keywords and delimiters) cannot be binary
    /// operators, while others default to function application.
    ///
    /// # Arguments
    ///
    /// * `token` - The token to examine
    ///
    /// # Returns
    ///
    /// Returns `Some(BinaryOperator)` if the token represents a binary operator,
    /// or `None` if it cannot be used as a binary operator.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use nix_ast::expression::BinaryOperator;
    /// use nix_lexer::Token;
    ///
    /// assert_eq!(BinaryOperator::parse(&Token::Plus), Some(BinaryOperator::Addition));
    /// assert_eq!(BinaryOperator::parse(&Token::Let), None);
    /// ```
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

    /// Returns the associativity of this operator.
    ///
    /// Associativity determines how operators of the same precedence are
    /// grouped. Left-associative operators group from left to right,
    /// right-associative operators group from right to left, and some
    /// operators (like comparisons) are non-associative.
    ///
    /// # Returns
    ///
    /// - `Some(BinaryAssociativity::Left)` for left-associative operators
    /// - `Some(BinaryAssociativity::Right)` for right-associative operators  
    /// - `None` for non-associative operators
    ///
    /// # Examples
    ///
    /// ```rust
    /// use nix_ast::expression::{BinaryOperator, BinaryAssociativity};
    ///
    /// // Addition is left-associative: a + b + c = (a + b) + c
    /// assert_eq!(BinaryOperator::Addition.associativity(),
    ///           Some(BinaryAssociativity::Left));
    ///
    /// // Update is right-associative: a // b // c = a // (b // c)
    /// assert_eq!(BinaryOperator::Update.associativity(),
    ///           Some(BinaryAssociativity::Right));
    ///
    /// // Equality is non-associative: a == b == c is not allowed
    /// assert_eq!(BinaryOperator::Eq.associativity(), None);
    /// ```
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

/// Associativity of binary operators.
///
/// This enum determines how operators of the same precedence level are
/// grouped when they appear in sequence. For example, with left associativity,
/// `a + b + c` is parsed as `(a + b) + c`, while with right associativity,
/// `a // b // c` is parsed as `a // (b // c)`.
///
/// Some operators (like comparison operators) are non-associative, meaning
/// expressions like `a == b == c` are not allowed and will result in a
/// parse error.
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum BinaryAssociativity {
    /// Left-to-right associativity: `a op b op c` = `(a op b) op c`
    Left,
    /// Right-to-left associativity: `a op b op c` = `a op (b op c)`
    Right,
}
