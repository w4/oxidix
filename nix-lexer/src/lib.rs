pub use logos::{Lexer, Logos};
use memchr::{memchr2_iter, memchr3_iter};
use strum::{EnumDiscriminants, EnumMessage, EnumString};

#[derive(Logos, Debug, PartialEq, Clone, EnumDiscriminants)]
#[strum_discriminants(derive(EnumString, EnumMessage))]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'a> {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("then")]
    Then,
    #[regex("[0-9]+")]
    Int(&'a str),
    #[regex("[0-9]+\\.[0-9]+")]
    Float(&'a str),
    #[regex(r"[a-zA-Z_](['a-zA-Z0-9_\-])*")]
    Ident(&'a str),
    #[token(".")]
    Dot,
    #[token("=")]
    Equals,
    #[token("==")]
    DoubleEquals,
    #[token(">=")]
    Ge,
    #[token("<=")]
    Le,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("||")]
    Or,
    #[token("or")]
    TextOr,
    #[token("&&")]
    And,
    #[token(";")]
    Semicolon,
    #[token("++")]
    PlusPlus,
    #[token("+")]
    Plus,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEquals,
    #[token("-")]
    Neg,
    #[token("//")]
    SlashSlash,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    BracketOpen,
    #[token(")")]
    BracketClose,
    #[token("[")]
    SquareBracketOpen,
    #[token("]")]
    SquareBracketClose,
    #[token("/*", |lex| {
        let len = lex.remainder().find("*/")?;
        lex.bump(len + 2); // include len of `*/`
        Some(())
    })]
    BlockComment,
    #[regex("#[^\n]*")]
    InlineComment,
    #[token(":")]
    Colon,
    #[token("inherit")]
    Inherit,
    #[token("rec")]
    Rec,
    #[token("?")]
    Question,
    #[token("$")]
    Dollar,
    #[token("${")]
    InterpolationStart,
    #[token("\"", lex_string)]
    String(Vec<StringToken<'a>>),
    #[token("''", lex_multiline_string)]
    MultilineString(Vec<StringToken<'a>>),
    #[token(",")]
    Comma,
    #[token("/")]
    Slash,
    #[token("throw")]
    Throw,
    #[token("@")]
    At,
    #[token("*")]
    Asterisk,
    #[token("->")]
    Arrow,
    #[token("|>")]
    PipeRight,
    #[token("<|")]
    PipeLeft,
    #[regex("[.~]?/[A-Za-z0-9.][./A-Za-z0-9]*")]
    Path(&'a str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringToken<'a> {
    Content(&'a str),
    Interpolation(Vec<Token<'a>>),
}

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
