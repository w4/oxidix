//! Basic lexing example demonstrating how to tokenize Nix source code.

use nix_lexer::{Logos, StringToken, Token};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple expression
    let source = r#"let x = 42; y = "hello"; in x + 1"#;

    println!("Source: {}", source);
    println!("Tokens:");

    let mut lexer = Token::lexer(source).spanned();
    while let Some((token_result, span)) = lexer.next() {
        match token_result {
            Ok(token) => println!("  {:?} at {:?}", token, span),
            Err(_) => println!("  Invalid token at {:?}", span),
        }
    }

    println!("\n{}", "=".repeat(50));

    // String interpolation example
    let interpolated = r#""Hello ${user.name}, you have ${count + 1} messages""#;
    println!("Interpolated string: {}", interpolated);

    let mut lexer = Token::lexer(interpolated);
    if let Some(Ok(Token::String(string_tokens))) = lexer.next() {
        println!("String components:");
        for (i, token) in string_tokens.iter().enumerate() {
            match token {
                StringToken::Content(content) => {
                    println!("  [{}] Content: '{}'", i, content);
                }
                StringToken::Interpolation(tokens) => {
                    println!("  [{}] Interpolation: {:?}", i, tokens);
                }
            }
        }
    }

    println!("\n{}", "=".repeat(50));

    // Multiline string example
    let multiline = r#"''
        This is a multiline string
        with ${interpolation}
        and literal newlines
    ''"#;

    println!("Multiline string: {}", multiline);

    let mut lexer = Token::lexer(multiline);
    if let Some(Ok(Token::MultilineString(string_tokens))) = lexer.next() {
        println!("Multiline string components:");
        for (i, token) in string_tokens.iter().enumerate() {
            match token {
                StringToken::Content(content) => {
                    println!("  [{}] Content: {:?}", i, content);
                }
                StringToken::Interpolation(tokens) => {
                    println!("  [{}] Interpolation: {:?}", i, tokens);
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "let x = 42; in x";
        let tokens: Result<Vec<_>, _> = Token::lexer(source).collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        // Should contain: Let, Ident("x"), Equals, Int("42"), Semicolon, In, Ident("x")
        assert_eq!(tokens.len(), 7);
        assert!(matches!(tokens[0], Token::Let));
        assert!(matches!(tokens[1], Token::Ident("x")));
        assert!(matches!(tokens[2], Token::Equals));
        assert!(matches!(tokens[3], Token::Int("42")));
    }

    #[test]
    fn test_string_interpolation() {
        let source = r#""hello ${name}""#;
        let mut lexer = Token::lexer(source);

        if let Some(Ok(Token::String(tokens))) = lexer.next() {
            assert_eq!(tokens.len(), 3);
            assert!(matches!(tokens[0], StringToken::Content("hello ")));
            assert!(matches!(tokens[1], StringToken::Interpolation(_)));
            assert!(matches!(tokens[2], StringToken::Content("")));
        } else {
            panic!("Expected string token");
        }
    }
}
