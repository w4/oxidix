//! Error types and utilities for the Nix AST parser.
//!
//! This module provides comprehensive error handling for parsing Nix expressions,
//! including detailed error messages and source location tracking.

use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use nix_lexer::{Span, Token, TokenDiscriminants};
use thiserror::Error;

/// Errors that can occur during Nix expression parsing.
///
/// This enum covers all possible parsing errors, from lexical issues to
/// semantic problems like duplicate function arguments. Each error variant
/// provides specific information about what went wrong.
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// An invalid or unrecognized token was encountered during lexing
    #[error("Invalid token")]
    InvalidToken,

    /// The input ended unexpectedly while parsing was still in progress
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,

    /// An unexpected token was found at the top level of an expression
    #[error("Unexpected token {0:?}")]
    UnexpectedTopLevelToken(TokenDiscriminants),

    /// An unexpected token was found when specific tokens were expected
    #[error("Unexpected token {0:?} expected one of {1:?}")]
    UnexpectedToken(TokenDiscriminants, Vec<TokenDiscriminants>),

    /// Failed to parse an integer literal (e.g., number too large)
    #[error("Invalid integer: {0}")]
    ParseInt(ParseIntError),

    /// Failed to parse a floating-point literal (e.g., invalid format)
    #[error("Invalid float: {0}")]
    ParseFloat(ParseFloatError),

    /// A function parameter was declared multiple times
    #[error("Duplicate formal function argument: {0}")]
    DuplicateFunctionArgument(String),

    /// An attribute was defined multiple times in the same attribute set
    #[error("Attribute `{0}` already defined")]
    AttributeAlreadyDefined(String),
}

impl Error {
    /// Attaches source location information to this error.
    ///
    /// This method converts a plain `Error` into a `SpannedError` by adding
    /// the source span where the error occurred. This is essential for
    /// providing useful error messages to users.
    ///
    /// # Arguments
    ///
    /// * `span` - The source location where this error occurred
    ///
    /// # Examples
    ///
    /// ```rust
    /// use nix_ast::Error;
    ///
    /// let error = Error::UnexpectedEndOfFile;
    /// let spanned = error.with_span(0..10);
    /// ```
    pub fn with_span(self, span: Span) -> SpannedError {
        SpannedError { span, error: self }
    }
}

/// A trait for attaching span information to `Result<T, Error>` values.
///
/// This trait provides a convenient way to add source location information
/// to parsing errors, making it easy to convert from `Result<T, Error>` to
/// `Result<T, SpannedError>`.
pub trait WithSpan<T> {
    /// Attaches span information to the error case of this result.
    ///
    /// # Arguments
    ///
    /// * `span` - The source location to attach to any error
    fn with_span(self, span: Span) -> Result<T, SpannedError>;
}

impl<T> WithSpan<T> for Result<T, Error> {
    fn with_span(self, span: Span) -> Result<T, SpannedError> {
        self.map_err(|e| e.with_span(span))
    }
}

/// A parsing error with associated source location information.
///
/// This struct combines an `Error` with a `Span` indicating where in the
/// source code the error occurred. This is the primary error type returned
/// by parsing functions and provides rich context for error reporting.
///
/// # Examples
///
/// ```rust
/// use nix_ast::{parse_expression, format_error};
/// use nix_lexer::{Token, Logos};
/// use std::path::Path;
///
/// let result = parse_expression(Token::lexer("invalid syntax"));
/// if let Err(spanned_error) = result {
///     println!("Error at {:?}: {}", spanned_error.span, spanned_error.error);
///     
///     // Format for user display
///     let diagnostic = format_error(spanned_error, Path::new("example.nix"));
///     eprintln!("{}", diagnostic);
/// }
/// ```
#[derive(Debug)]
pub struct SpannedError {
    /// The source location where this error occurred
    pub span: Span,
    /// The underlying parsing error
    pub error: Error,
}

impl Display for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)?;
        write!(f, " (source: {:?})", self.span)?;
        Ok(())
    }
}

impl std::error::Error for SpannedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

/// A trait for converting token stream results into properly spanned errors.
///
/// This trait provides a uniform interface for handling errors that occur
/// when consuming tokens from a lexer stream. It automatically converts
/// lexer errors and end-of-stream conditions into appropriate `SpannedError`
/// instances with correct source location information.
///
/// # Examples
///
/// ```rust
/// use nix_ast::HandleStreamError;
/// use nix_lexer::{Token, Logos};
///
/// let mut lexer = Token::lexer("42").spanned();
/// let (token, span) = lexer.next().span_error()?;
/// # Ok::<(), nix_ast::SpannedError>(())
/// ```
pub trait HandleStreamError<T> {
    /// Converts this value into a result with proper span information.
    ///
    /// This method handles the conversion of lexer results (which may be
    /// `Ok(token)`, `Err(())` for invalid tokens, or `None` for end of stream)
    /// into properly typed results with span information.
    fn span_error(self) -> Result<(T, Span), SpannedError>;
}

impl<'a, 'b: 'a> HandleStreamError<&'a Token<'a>> for &'b (Result<Token<'a>, ()>, Span) {
    fn span_error(self) -> Result<(&'a Token<'a>, Span), SpannedError> {
        match self.0.as_ref() {
            Ok(v) => Ok((v, self.1.clone())),
            Err(()) => Err(SpannedError {
                span: self.1.clone(),
                error: Error::InvalidToken,
            }),
        }
    }
}

impl<'a, 'b: 'a> HandleStreamError<&'a Token<'a>> for Option<&'b (Result<Token<'a>, ()>, Span)> {
    fn span_error(self) -> Result<(&'a Token<'a>, Span), SpannedError> {
        match self {
            Some((Ok(v), span)) => Ok((v, span.clone())),
            Some((Err(()), span)) => Err(SpannedError {
                span: span.clone(),
                error: Error::InvalidToken,
            }),
            None => Err(SpannedError {
                span: 0..0,
                error: Error::UnexpectedEndOfFile,
            }),
        }
    }
}

impl<'a> HandleStreamError<Token<'a>> for Option<(Result<Token<'a>, ()>, Span)> {
    fn span_error(self) -> Result<(Token<'a>, Span), SpannedError> {
        match self {
            Some((Ok(v), span)) => Ok((v, span)),
            Some((Err(()), span)) => Err(SpannedError {
                span,
                error: Error::InvalidToken,
            }),
            None => Err(SpannedError {
                span: 0..0,
                error: Error::UnexpectedEndOfFile,
            }),
        }
    }
}
