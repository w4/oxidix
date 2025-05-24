use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use nix_lexer::{Span, Token, TokenDiscriminants};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid token")]
    InvalidToken,
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,
    #[error("Unexpected token {0:?}")]
    UnexpectedTopLevelToken(TokenDiscriminants),
    #[error("Unexpected token {0:?} expected one of {1:?}")]
    UnexpectedToken(TokenDiscriminants, Vec<TokenDiscriminants>),
    #[error("Invalid integer: {0}")]
    ParseInt(ParseIntError),
    #[error("Invalid float: {0}")]
    ParseFloat(ParseFloatError),
    #[error("Duplicate formal function argument: {0}")]
    DuplicateFunctionArgument(String),
    #[error("Attribute `{0}` already defined")]
    AttributeAlreadyDefined(String),
}

impl Error {
    pub fn with_span(self, span: Span) -> SpannedError {
        SpannedError { span, error: self }
    }
}

pub trait WithSpan<T> {
    fn with_span(self, span: Span) -> Result<T, SpannedError>;
}

impl<T> WithSpan<T> for Result<T, Error> {
    fn with_span(self, span: Span) -> Result<T, SpannedError> {
        self.map_err(|e| e.with_span(span))
    }
}

#[derive(Debug)]
pub struct SpannedError {
    pub span: Span,
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

pub trait HandleStreamError<T> {
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
