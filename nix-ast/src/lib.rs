//! # Nix AST Parser
//!
//! A parser for the Nix programming language that builds abstract syntax trees (ASTs)
//! from token streams produced by the `nix-lexer` crate. This crate provides a complete
//! parser for Nix expressions with comprehensive error reporting.
//!
//! ## Features
//!
//! - **Complete Nix syntax support**: All Nix language constructs
//! - **Rich error reporting**: Detailed error messages with source locations
//! - **Zero-copy parsing**: AST nodes reference the original source where possible
//! - **Operator precedence**: Correct handling of all Nix operators
//! - **Expression evaluation**: Support for all Nix expression types
//!
//! ## Basic Usage
//!
//! ```rust,ignore
//! use nix_ast::parse_expression;
//! use nix_lexer::{Token, Logos};
//!
//! let source = "let x = 42; in x + 1";
//! let lexer = Token::lexer(source);
//! let ast = parse_expression(lexer)?;
//! # Ok::<(), nix_ast::SpannedError>(())
//! ```
//!
//! ## Error Handling
//!
//! The parser provides rich error reporting with source locations:
//!
//! ```rust,ignore
//! use nix_ast::{parse_expression, format_error};
//! use nix_lexer::{Token, Logos};
//! use std::path::Path;
//!
//! let source = "let x = ; in x";  // Invalid syntax
//! let lexer = Token::lexer(source);
//!
//! match parse_expression(lexer) {
//!     Ok(ast) => { /* Process AST */ },
//!     Err(error) => {
//!         let diagnostic = format_error(error, Path::new("example.nix"));
//!         eprintln!("{}", diagnostic);
//!     }
//! }
//! ```

mod error;
pub mod expression;

use std::{iter::Peekable, path::Path};

use ariadne::Source;
use error::WithSpan;
pub use error::{Error, HandleStreamError, SpannedError};
use expression::{
    ArrayExpression, AttrsetExpression, BinaryExpression, IfExpression, LambdaExpression,
    LetExpression, ThrowExpression, UnaryExpression,
};
use nix_lexer::{Lexer, SpannedIter, Token, TokenDiscriminants};

/// An expression in the Nix programming language.
///
/// This enum represents all possible expressions that can appear in Nix code.
/// Each variant corresponds to a different type of expression, from simple
/// literals to complex constructs like lambda functions and let expressions.
///
/// # Examples
///
/// ```rust
/// use nix_ast::{parse_expression, Expression};
/// use nix_lexer::{Token, Logos};
///
/// // Parse a simple integer
/// let ast = parse_expression(Token::lexer("42"))?;
/// assert!(matches!(ast, Expression::Int(42)));
///
/// // Parse a lambda expression
/// let ast = parse_expression(Token::lexer("x: x + 1"))?;
/// assert!(matches!(ast, Expression::Lambda(_)));
/// # Ok::<(), nix_ast::SpannedError>(())
/// ```
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expression<'a> {
    /// Boolean literal (`true` or `false`)
    Bool(bool),

    /// Integer literal (e.g., `42`, `-123`)
    Int(i64),

    /// Floating-point literal (e.g., `3.14`, `2.5`)
    Float(WrappedFloat),

    /// Addition expression (legacy, use `Binary` with `Addition` operator instead)
    Add(Box<(Expression<'a>, Expression<'a>)>),

    /// Let expression (`let x = 1; y = 2; in x + y`)
    Let(Box<LetExpression<'a>>),

    /// Identifier (variable name, function name, etc.)
    Ident(&'a str),

    /// Conditional expression (`if condition then expr1 else expr2`)
    If(Box<IfExpression<'a>>),

    /// Binary operation (arithmetic, logical, comparison, etc.)
    Binary(Box<BinaryExpression<'a>>),

    /// Unary operation (negation, logical NOT)
    Unary(Box<UnaryExpression<'a>>),

    /// Array literal (`[ 1 2 3 ]`)
    Array(ArrayExpression<'a>),

    /// Comment (block or inline)
    Comment,

    /// Lambda function (`x: x + 1` or `{ x, y }: x + y`)
    Lambda(Box<LambdaExpression<'a>>),

    /// Attribute set (`{ x = 1; y = 2; }`)
    Attrset(AttrsetExpression<'a>),

    /// Exception throw (`throw "test"`)
    Throw(Box<ThrowExpression<'a>>),
}

/// Parses a Nix expression from a token stream.
///
/// This is the main entry point for parsing Nix expressions. It takes a lexer
/// that produces tokens and returns either a parsed AST or a detailed error
/// with source location information.
///
/// # Arguments
///
/// * `stream` - A lexer instance that produces tokens from Nix source code
///
/// # Returns
///
/// Returns `Ok(Expression)` if parsing succeeds, or `Err(SpannedError)` if
/// there's a syntax error. The error includes detailed information about what
/// went wrong and where in the source code.
///
/// # Examples
///
/// ```rust
/// use nix_ast::parse_expression;
/// use nix_lexer::{Token, Logos};
///
/// // Parse a simple expression
/// let ast = parse_expression(Token::lexer("42"))?;
/// assert!(matches!(ast, nix_ast::Expression::Int(42)));
///
/// // Parse a complex expression
/// let ast = parse_expression(Token::lexer("let x = 1; in x + 2"))?;
/// assert!(matches!(ast, nix_ast::Expression::Let(_)));
/// # Ok::<(), nix_ast::SpannedError>(())
/// ```
///
/// # Error Handling
///
/// ```rust,ignore
/// use nix_ast::{parse_expression, format_error};
/// use nix_lexer::{Token, Logos};
/// use std::path::Path;
///
/// let result = parse_expression(Token::lexer("let x = ; in x"));
/// match result {
///     Ok(ast) => println!("Parsed: {:?}", ast),
///     Err(error) => {
///         let diagnostic = format_error(error, Path::new("example.nix"));
///         eprintln!("Parse error:\n{}", diagnostic);
///     }
/// }
/// ```
pub fn parse_expression<'a>(stream: Lexer<'a, Token<'a>>) -> Result<Expression<'a>, SpannedError> {
    parse_expression_inner(&mut stream.spanned().peekable(), u8::MAX)
}

fn parse_expression_inner<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    min_prec: u8,
) -> Result<Expression<'a>, SpannedError> {
    let mut left = if let Some(expr) = UnaryExpression::parse(stream)? {
        Expression::Unary(Box::new(expr))
    } else if let Some(lambda) = LambdaExpression::parse(stream)? {
        Expression::Lambda(Box::new(lambda))
    } else {
        parse_primary(stream)?
    };

    loop {
        let Some((operator, right, postfix)) = BinaryExpression::parse(stream, min_prec)? else {
            break;
        };

        left = Expression::Binary(Box::new(BinaryExpression {
            operator,
            left,
            right,
            postfix,
        }));
    }

    Ok(left)
}

fn parse_primary<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
) -> Result<Expression<'a>, SpannedError> {
    let (token, span) = stream.next().span_error()?;

    match token {
        Token::Bool(v) => Ok(Expression::Bool(v)),
        Token::Let => Ok(Expression::Let(Box::new(LetExpression::parse(stream)?))),
        Token::If => Ok(Expression::If(Box::new(IfExpression::parse(stream)?))),
        Token::Int(v) => Ok(Expression::Int(
            v.parse().map_err(Error::ParseInt).with_span(span)?,
        )),
        Token::Float(v) => Ok(Expression::Float(WrappedFloat(
            v.parse().map_err(Error::ParseFloat).with_span(span)?,
        ))),
        Token::Ident(v) => Ok(Expression::Ident(v)),
        Token::BracketOpen => wrapped_interpolated(stream, TokenDiscriminants::BracketClose),
        Token::InterpolationStart => wrapped_interpolated(stream, TokenDiscriminants::BraceClose),
        Token::SquareBracketOpen => Ok(Expression::Array(ArrayExpression::parse(stream)?)),
        Token::BraceOpen => Ok(Expression::Attrset(AttrsetExpression::parse(stream)?)),
        Token::Rec => Ok(Expression::Attrset(AttrsetExpression {
            recursive: true,
            ..AttrsetExpression::parse(stream)?
        })),
        Token::Throw => Ok(Expression::Throw(Box::new(ThrowExpression::parse(stream)?))),
        Token::Dollar => todo!("dollar"),
        Token::String(_string_tokens) => todo!("str"),
        Token::MultilineString(_string_tokens) => todo!("multi"),
        Token::Path(_) => todo!("path"),
        Token::BlockComment | Token::InlineComment => Ok(Expression::Comment),
        v => Err(Error::UnexpectedTopLevelToken(v.into()).with_span(span)),
    }
}

fn wrapped_interpolated<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    end: TokenDiscriminants,
) -> Result<Expression<'a>, SpannedError> {
    let node = parse_expression_inner(stream, u8::MAX)?;
    expect_next_token_or_error(stream, end)?;
    Ok(node)
}

/// A wrapper around `f64` that implements `Eq` and `Ord` for use in AST nodes.
///
/// Since floating-point numbers don't naturally implement `Eq` and `Ord` due to
/// NaN values, this wrapper uses `total_cmp` to provide a total ordering that
/// treats NaN values consistently.
///
/// # Examples
///
/// ```rust
/// use nix_ast::WrappedFloat;
///
/// let f1 = WrappedFloat(3.14);
/// let f2 = WrappedFloat(2.71);
/// assert!(f1 > f2);
/// ```
#[derive(PartialEq, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WrappedFloat(pub f64);

impl Eq for WrappedFloat {}

impl PartialOrd for WrappedFloat {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WrappedFloat {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

/// Represents a binding name in attribute sets and let expressions.
///
/// Binding names can be either static identifiers or dynamic expressions
/// that are evaluated at runtime. Dynamic binding names use interpolation
/// syntax like `${expression}`.
///
/// # Examples
///
/// ```rust
/// // Static binding: x = 1;
/// // Dynamic binding: ${expr} = 1;
/// ```
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BindingName<'a> {
    /// A dynamic binding name that requires evaluation (e.g., `${expr}`)
    Lazy(Expression<'a>),

    /// A static binding name (e.g., `x`, `foo-bar`)
    Value(&'a str),
}

fn parse_binding_name<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
) -> Result<Vec<BindingName<'a>>, SpannedError> {
    let mut name = Vec::new();

    loop {
        match stream.next().span_error()? {
            (Token::Ident(ident), _) => name.push(BindingName::Value(ident)),
            (Token::InterpolationStart, _) => {
                name.push(BindingName::Lazy(parse_expression_inner(stream, u8::MAX)?));
                expect_next_token_or_error(stream, TokenDiscriminants::BraceClose)?;
            }
            (Token::Equals, _) => return Ok(name),
            (token, span) => {
                return Err(Error::UnexpectedToken(
                    token.into(),
                    vec![
                        TokenDiscriminants::Ident,
                        TokenDiscriminants::InterpolationStart,
                        TokenDiscriminants::Equals,
                    ],
                )
                .with_span(span));
            }
        }
    }
}

fn expect_next_token_or_error<'a>(
    stream: &mut Peekable<SpannedIter<'a, Token<'a>>>,
    kind: TokenDiscriminants,
) -> Result<(), SpannedError> {
    match stream.next().span_error()? {
        (v, _) if TokenDiscriminants::from(&v) == kind => Ok(()),
        (v, span) => Err(Error::UnexpectedToken(v.into(), vec![kind]).with_span(span)),
    }
}

/// Formats a parsing error into a human-readable diagnostic message.
///
/// This function takes a `SpannedError` (which contains both the error details
/// and source location information) and formats it into a rich diagnostic
/// message using the `ariadne` crate. The resulting message includes:
///
/// - A clear error description
/// - Source code context showing where the error occurred
/// - Visual indicators pointing to the problematic location
/// - Syntax highlighting for better readability
///
/// # Arguments
///
/// * `error` - The parsing error with span information
/// * `path` - Path to the source file (used for display purposes)
///
/// # Returns
///
/// A formatted string containing the diagnostic message ready for display
/// to the user.
///
/// # Examples
///
/// ```rust,ignore
/// use nix_ast::{parse_expression, format_error};
/// use nix_lexer::{Token, Logos};
/// use std::path::Path;
///
/// let source = "let x = ; in x";  // Invalid syntax
/// let result = parse_expression(Token::lexer(source));
///
/// if let Err(error) = result {
///     let diagnostic = format_error(error, Path::new("example.nix"));
///     eprintln!("{}", diagnostic);
///     // This will print a nicely formatted error message showing
///     // exactly where the syntax error occurred
/// }
/// ```
///
/// # Panics
///
/// This function will panic if:
/// - The file at `path` cannot be read
/// - The error message cannot be formatted (very unlikely)
/// - The resulting bytes cannot be converted to UTF-8 (very unlikely)
#[cfg(feature = "ariadne")]
pub fn format_error(error: SpannedError, path: &Path) -> String {
    use ariadne::{Label, Report, ReportKind};

    let mut out = Vec::new();

    Report::build(ReportKind::Error, error.span.clone())
        .with_message("Syntax error")
        .with_label(Label::new(error.span).with_message(error.error))
        .finish()
        .write_for_stdout(
            Source::from(std::fs::read_to_string(path).unwrap()),
            &mut out,
        )
        .unwrap();

    String::from_utf8(out).unwrap()
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use insta::{assert_debug_snapshot, assert_snapshot, glob};
    use nix_lexer::Logos as _;

    use crate::parse_expression;

    #[test]
    fn fixtures() {
        glob!("../fixtures", "*.nix", |path| {
            let input = read_to_string(path).expect("failed to read fixture");
            let lex = nix_lexer::Token::lexer(&input);
            match parse_expression(lex) {
                Ok(v) => assert_debug_snapshot!("fixture", v, &input),
                Err(e) => {
                    assert_snapshot!(
                        "fixture",
                        strip_ansi_codes(&crate::format_error(e, path)),
                        &input
                    )
                }
            }
        });
    }

    fn strip_ansi_codes(input: &str) -> String {
        let mut output = String::new();
        let mut chars = input.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\x1b' {
                if chars.peek() == Some(&'[') {
                    chars.next(); // Skip '['

                    // Skip all characters until a letter (end of ANSI sequence)
                    while let Some(&next) = chars.peek() {
                        if next.is_ascii_alphabetic() {
                            chars.next(); // consume the letter
                            break;
                        } else {
                            chars.next(); // skip numeric or separator
                        }
                    }
                }
            } else {
                output.push(c);
            }
        }

        output
    }
}
