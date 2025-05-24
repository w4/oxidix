//! # Nix Lexer
//!
//! A high-performance lexer for the Nix programming language built with the `logos` crate.
//! This crate provides tokenization capabilities for Nix source code, including support for
//! complex string interpolation and all Nix language constructs.
//!
//! ## Features
//!
//! - **Zero-copy tokenization**: String tokens reference the original source
//! - **String interpolation**: Full support for nested interpolation expressions
//! - **Complete Nix syntax**: All operators, keywords, and literals
//! - **Span information**: Source location tracking for error reporting
//! - **High performance**: Optimized with `memchr` for fast character scanning
//!
//! ## Basic Usage
//!
//! ```rust
//! use nix_lexer::{Token, Logos};
//!
//! let source = r#"let x = "hello ${name}"; in x"#;
//! let tokens: Result<Vec<Token>, _> = Token::lexer(source).collect();
//! ```
//!
//! ## String Interpolation
//!
//! The lexer handles complex string interpolation including nested expressions:
//!
//! ```rust
//! use nix_lexer::{Token, StringToken, Logos};
//!
//! let source = r#""Hello ${user.name}!""#;
//! let mut lexer = Token::lexer(source);
//!
//! if let Some(Ok(Token::String(tokens))) = lexer.next() {
//!     for token in tokens {
//!         match token {
//!             StringToken::Content(text) => println!("Text: {}", text),
//!             StringToken::Interpolation(expr_tokens) => {
//!                 println!("Expression: {:?}", expr_tokens);
//!             }
//!         }
//!     }
//! }
//! ```

pub use logos::{Lexer, Logos, Span, SpannedIter};
use memchr::{memchr2_iter, memchr3_iter};
use strum::{EnumDiscriminants, EnumMessage, EnumString};

/// A token in the Nix programming language.
///
/// This enum represents all possible tokens that can appear in Nix source code,
/// including literals, operators, keywords, and punctuation. The lexer uses
/// zero-copy string slices where possible to reference the original source.
///
/// # Examples
///
/// ```rust
/// use nix_lexer::{Token, Logos};
///
/// let source = "let x = 42; in x";
/// let tokens: Vec<Token> = Token::lexer(source)
///     .collect::<Result<Vec<_>, _>>()
///     .unwrap();
///
/// assert!(matches!(tokens[0], Token::Let));
/// assert!(matches!(tokens[1], Token::Ident("x")));
/// ```
#[derive(Logos, Debug, PartialEq, Clone, EnumDiscriminants)]
#[strum_discriminants(derive(EnumString, EnumMessage))]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'a> {
    /// Boolean literal (`true` or `false`)
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    /// `let` keyword for let expressions
    #[token("let")]
    Let,

    /// `in` keyword for let expressions
    #[token("in")]
    In,

    /// `if` keyword for conditional expressions
    #[token("if")]
    If,

    /// `else` keyword for conditional expressions
    #[token("else")]
    Else,

    /// `then` keyword for conditional expressions
    #[token("then")]
    Then,

    /// Integer literal (e.g., `42`, `123`)
    #[regex("[0-9]+")]
    Int(&'a str),

    /// Floating-point literal (e.g., `3.14`, `2.5`)
    #[regex("[0-9]+\\.[0-9]+")]
    Float(&'a str),

    /// Identifier (variable name, function name, etc.)
    #[regex(r"[a-zA-Z_](['a-zA-Z0-9_\-])*")]
    Ident(&'a str),

    /// Ellipsis (`...`) for variadic function parameters
    #[token("...")]
    DotDotDot,

    /// Dot (`.`) for attribute selection
    #[token(".")]
    Dot,

    /// Assignment operator (`=`)
    #[token("=")]
    Equals,

    /// Equality operator (`==`)
    #[token("==")]
    DoubleEquals,

    /// Greater than or equal operator (`>=`)
    #[token(">=")]
    Ge,

    /// Less than or equal operator (`<=`)
    #[token("<=")]
    Le,

    /// Less than operator (`<`)
    #[token("<")]
    Lt,

    /// Greater than operator (`>`)
    #[token(">")]
    Gt,

    /// Logical OR operator (`||`)
    #[token("||")]
    Or,

    /// Alternative operator (`or`) for attribute selection with default
    #[token("or")]
    TextOr,

    /// Logical AND operator (`&&`)
    #[token("&&")]
    And,

    /// Semicolon (`;`) for separating bindings
    #[token(";")]
    Semicolon,

    /// List concatenation operator (`++`)
    #[token("++")]
    PlusPlus,

    /// Addition operator (`+`)
    #[token("+")]
    Plus,

    /// Logical NOT operator (`!`)
    #[token("!")]
    Bang,

    /// Inequality operator (`!=`)
    #[token("!=")]
    BangEquals,

    /// Subtraction/negation operator (`-`)
    #[token("-")]
    Neg,

    /// Update operator (`//`) for merging attribute sets
    #[token("//")]
    SlashSlash,

    /// Opening brace (`{`)
    #[token("{")]
    BraceOpen,

    /// Closing brace (`}`)
    #[token("}")]
    BraceClose,

    /// Opening parenthesis (`(`)
    #[token("(")]
    BracketOpen,

    /// Closing parenthesis (`)`)
    #[token(")")]
    BracketClose,

    /// Opening square bracket (`[`)
    #[token("[")]
    SquareBracketOpen,

    /// Closing square bracket (`]`)
    #[token("]")]
    SquareBracketClose,

    /// Block comment (`/* ... */`)
    #[token("/*", |lex| {
        let len = lex.remainder().find("*/")?;
        lex.bump(len + 2); // include len of `*/`
        Some(())
    })]
    BlockComment,

    /// Inline comment (`# ...`)
    #[regex("#[^\n]*")]
    InlineComment,

    /// Colon (`:`) for lambda expressions
    #[token(":")]
    Colon,

    /// `inherit` keyword for inheriting attributes
    #[token("inherit")]
    Inherit,

    /// `rec` keyword for recursive attribute sets
    #[token("rec")]
    Rec,

    /// Question mark (`?`) for has-attribute operator and optional parameters
    #[token("?")]
    Question,

    /// Dollar sign (`$`) for string interpolation
    #[token("$")]
    Dollar,

    /// String interpolation start (`${`)
    #[token("${")]
    InterpolationStart,

    /// String literal with possible interpolation
    #[token("\"", lex_string)]
    String(Vec<StringToken<'a>>),

    /// Multiline string literal with possible interpolation
    #[token("''", lex_multiline_string)]
    MultilineString(Vec<StringToken<'a>>),

    /// Comma (`,`) for separating list elements and function parameters
    #[token(",")]
    Comma,

    /// Division operator (`/`)
    #[token("/")]
    Slash,

    /// `throw` keyword for throwing errors
    #[token("throw")]
    Throw,

    /// At symbol (`@`) for binding function parameters
    #[token("@")]
    At,

    /// Multiplication operator (`*`)
    #[token("*")]
    Asterisk,

    /// Implication operator (`->`)
    #[token("->")]
    Arrow,

    /// Right pipe operator (`|>`)
    #[token("|>")]
    PipeRight,

    /// Left pipe operator (`<|`)
    #[token("<|")]
    PipeLeft,

    /// Path literal (e.g., `./path`, `/absolute/path`)
    #[regex("[.~]?/[A-Za-z0-9.][./A-Za-z0-9]*")]
    Path(&'a str),
}

/// A component of a string literal that may contain interpolated expressions.
///
/// String literals in Nix can contain both plain text content and interpolated
/// expressions using the `${...}` syntax. This enum represents the different
/// components that make up a complete string.
///
/// # Examples
///
/// ```rust
/// use nix_lexer::{Token, StringToken, Logos};
///
/// let source = r#""Hello ${name}!""#;
/// let mut lexer = Token::lexer(source);
///
/// if let Some(Ok(Token::String(tokens))) = lexer.next() {
///     match &tokens[0] {
///         StringToken::Content(text) => assert_eq!(*text, "Hello "),
///         _ => panic!("Expected content"),
///     }
///     match &tokens[1] {
///         StringToken::Interpolation(expr_tokens) => {
///             // expr_tokens contains the tokenized expression
///         }
///         _ => panic!("Expected interpolation"),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum StringToken<'a> {
    /// Plain text content within a string literal.
    ///
    /// This represents literal text that appears in a string without any
    /// interpolation. The string slice references the original source.
    Content(&'a str),

    /// An interpolated expression within a string literal.
    ///
    /// This represents a `${...}` interpolation expression that has been
    /// tokenized. The vector contains all tokens that make up the expression.
    /// Nested interpolations are supported and will be recursively tokenized.
    Interpolation(Vec<Token<'a>>),
}

/// Lexes a multiline string literal (delimited by `''`).
///
/// This function handles the complex parsing of multiline strings in Nix, including:
/// - String interpolation with `${...}` syntax
/// - Escape sequences like `''$` and `''\`
/// - Nested interpolation expressions
/// - Proper handling of the closing `''` delimiter
///
/// # Arguments
///
/// * `lex` - The lexer instance positioned after the opening `''`
///
/// # Returns
///
/// Returns `Some(Vec<StringToken>)` if the string is successfully parsed,
/// or `None` if there's a syntax error (e.g., unterminated interpolation).
///
/// # Implementation Details
///
/// The function uses `memchr` for efficient scanning of special characters
/// (`$` and `'`) and maintains a depth counter for nested braces to properly
/// handle complex interpolation expressions.
#[allow(clippy::single_call_fn)]
fn lex_multiline_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<Vec<StringToken<'a>>> {
    const SINGLE_QUOTE: u8 = b'\'';

    let mut out = Vec::new();

    while !lex.remainder().is_empty() {
        let bytes = lex.remainder().as_bytes();

        for pos in memchr2_iter(b'$', SINGLE_QUOTE, bytes) {
            match bytes[pos..] {
                [b'$', b'{', ..] => {
                    if bytes.get(pos.saturating_sub(2)..pos) == Some(b"''")
                        || bytes.get(pos.saturating_sub(1)) == Some(&b'$')
                    {
                        // escaped
                        continue;
                    }

                    // push contents and advance
                    out.push(StringToken::Content(&lex.remainder()[..pos]));
                    lex.bump(pos + 2);

                    // keep going until we find our end token
                    let mut depth = 1;
                    let mut interpolated_tokens = Vec::new();
                    let mut token_lexer = Token::lexer(lex.remainder());

                    while let Some(token) = token_lexer.next().transpose().ok()? {
                        match token {
                            Token::BraceOpen | Token::InterpolationStart => depth += 1,
                            Token::BraceClose => depth -= 1,
                            _ => {}
                        }

                        if depth == 0 {
                            break;
                        }

                        interpolated_tokens.push(token);
                    }

                    if depth != 0 {
                        return None;
                    }

                    out.push(StringToken::Interpolation(interpolated_tokens));
                    *lex = token_lexer.morph();
                    break; // skipped ahead the lexer so break out of the memchr2_iter
                }
                [SINGLE_QUOTE, SINGLE_QUOTE, b'$', ..] => {
                    // escaped
                }
                [SINGLE_QUOTE, SINGLE_QUOTE, b'\\', ..] => {
                    // literal escape code, read into string and skip ahead
                    out.push(StringToken::Content(&lex.remainder()[..pos]));
                    out.push(StringToken::Content(&lex.remainder()[pos + 2..pos + 4]));
                    lex.bump(pos + 4);
                    break;
                }
                [SINGLE_QUOTE, SINGLE_QUOTE, SINGLE_QUOTE, ..] => {
                    // escaped double quote, write & skip ahead
                    out.push(StringToken::Content(&lex.remainder()[..pos + 2]));
                    lex.bump(pos + 3);
                    break;
                }
                [SINGLE_QUOTE, SINGLE_QUOTE, ..] => {
                    out.push(StringToken::Content(&lex.remainder()[..pos]));
                    lex.bump(pos + 2);
                    return Some(out);
                }
                _ => {}
            }
        }
    }

    None
}

/// Lexes a regular string literal (delimited by `"`).
///
/// This function handles the parsing of regular strings in Nix, including:
/// - String interpolation with `${...}` syntax
/// - Escape sequences like `\"`, `\\`, `\$`
/// - Nested interpolation expressions
/// - Proper handling of the closing `"` delimiter
///
/// # Arguments
///
/// * `lex` - The lexer instance positioned after the opening `"`
///
/// # Returns
///
/// Returns `Some(Vec<StringToken>)` if the string is successfully parsed,
/// or `None` if there's a syntax error (e.g., unterminated interpolation).
///
/// # Implementation Details
///
/// The function uses `memchr` for efficient scanning of special characters
/// (`$`, `"`, and `\`) and maintains a depth counter for nested braces to
/// properly handle complex interpolation expressions.
#[allow(clippy::single_call_fn, clippy::string_slice)]
fn lex_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<Vec<StringToken<'a>>> {
    let mut out = Vec::new();

    while !lex.remainder().is_empty() {
        let bytes = lex.remainder().as_bytes();

        for pos in memchr3_iter(b'$', b'"', b'\\', bytes) {
            let prefix = pos.checked_sub(1).and_then(|v| bytes.get(v)).copied();

            match (prefix, &bytes[pos..]) {
                (Some(b'\\'), [b'\\', ..]) => {
                    // rather than escaping another control character like `$` or `"`, a slash itself
                    // is being escaped. we need to skip over both, otherwise the next iteration will
                    // see this current escape as its prefix.
                    out.push(StringToken::Content(&lex.remainder()[..=pos]));
                    lex.bump(pos + 1);
                    break;
                }
                (Some(b'\\'), _) | (Some(b'$'), [b'$', ..]) => {
                    // escaped
                }
                (_, [b'$', b'{', ..]) => {
                    // push contents and advance
                    out.push(StringToken::Content(&lex.remainder()[..pos]));
                    lex.bump(pos + 2);

                    // keep going until we find our end token
                    let mut depth = 1_u32;
                    let mut interpolated_tokens = Vec::new();
                    let mut token_lexer = Token::lexer(lex.remainder());

                    while let Some(token) = token_lexer.next().transpose().ok()? {
                        match token {
                            Token::BraceOpen | Token::InterpolationStart => {
                                depth = depth.saturating_add(1);
                            }
                            Token::BraceClose => {
                                depth = depth.saturating_sub(1);
                            }
                            _ => {}
                        }

                        if depth == 0 {
                            break;
                        }

                        interpolated_tokens.push(token);
                    }

                    if depth != 0 {
                        return None;
                    }

                    out.push(StringToken::Interpolation(interpolated_tokens));
                    *lex = token_lexer.morph();
                    break; // skipped ahead the lexer so break out of the memchr2_iter
                }
                (_, [b'"', ..]) => {
                    out.push(StringToken::Content(&lex.remainder()[..pos]));
                    lex.bump(pos.wrapping_add(1));
                    return Some(out);
                }
                _ => {}
            }
        }
    }

    None
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod test {
    use std::fs::read_to_string;

    use insta::{assert_debug_snapshot, glob};
    use logos::Logos as _;

    use crate::Token;

    #[test]
    fn fixtures() {
        glob!("../fixtures", "*.nix", |path| {
            let input = read_to_string(path).expect("failed to read fixture");
            let actual = Token::lexer(&input)
                .collect::<Result<Vec<_>, _>>()
                .expect("failed to lex nix file");
            assert_debug_snapshot!("fixture", actual, &input);
        });
    }
}
