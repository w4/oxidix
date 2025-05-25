# Oxidix

A clean-room high-performance lexer and parser for the Nix programming language, written in Rust.

## Overview

Oxidix provides two complementary crates for working with Nix source code:

- **`nix-lexer`**: Fast tokenization of Nix source code with support for complex string interpolation
- **`nix-ast`**: Robust parsing into abstract syntax trees with comprehensive error reporting

## Targets

- ✅ **Complete Nix Language Support** - All language constructs including lambdas, attribute sets, string interpolation
- ⚡ **High Performance** - Zero-copy lexing with optimized parsing algorithms
- 🔍 **Rich Error Reporting** - Detailed diagnostics with source location and context
- 🦀 **Type Safe** - Leverages Rust's type system for safe parsing operations
- 🧪 **Well Tested** - Comprehensive test suite with snapshot testing

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
nix-lexer = "0.1.0"
nix-ast = "0.1.0"
```

### Basic Usage

```rust
use nix_lexer::{Token, Logos};
use nix_ast::parse_expression;

// Tokenize Nix source code
let source = r#"let x = "hello ${name}"; in x"#;
let tokens: Vec<Token> = Token::lexer(source).collect::<Result<_, _>>()?;

// Parse into an AST
let lexer = Token::lexer(source);
let ast = parse_expression(lexer)?;

println!("Parsed AST: {:#?}", ast);
```

### String Interpolation

```rust
use nix_lexer::{Token, StringToken, Logos};

let source = r#""Hello ${user.name}, you have ${count} messages""#;
let mut lexer = Token::lexer(source);

if let Some(Ok(Token::String(tokens))) = lexer.next() {
    for token in tokens {
        match token {
            StringToken::Content(text) => println!("Text: '{}'", text),
            StringToken::Interpolation(expr_tokens) => {
                println!("Expression: {:?}", expr_tokens);
            }
        }
    }
}
```

### Error Handling

```rust
use nix_ast::{parse_expression, format_error};
use nix_lexer::{Token, Logos};
use std::path::Path;

let source = "let x = ; in x";  // Invalid syntax
let lexer = Token::lexer(source);

match parse_expression(lexer) {
    Ok(ast) => println!("Success: {:#?}", ast),
    Err(error) => {
        let diagnostic = format_error(error, Path::new("example.nix"));
        eprintln!("{}", diagnostic);
    }
}
```

## Documentation

- [Examples](nix-ast/examples/)

## Supported Nix Constructs

### Literals
- Booleans: `true`, `false`
- Integers: `42`, `-17`
- Floats: `3.14`, `-2.5`
- Strings: `"hello"`, `''multiline''`
- Paths: `./path`, `/absolute/path`

### Expressions
- Variables: `x`, `someVar`
- Function application: `f x y`
- Attribute selection: `obj.attr`, `obj.attr or default`
- Conditionals: `if cond then a else b`
- Let bindings: `let x = 1; y = 2; in x + y`
- Lambdas: `x: x + 1`, `{ x, y ? 42 }: x + y`

### Data Structures
- Lists: `[ 1 2 3 ]`, `[ a b c ]`
- Attribute sets: `{ x = 1; y = 2; }`
- Recursive sets: `rec { x = 1; y = x + 1; }`

### Operators
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical: `&&`, `||`, `!`
- List operations: `++` (concatenation)
- Attribute operations: `//` (update), `?` (has attribute)
- Pipes: `|>`, `<|`

### String Interpolation
- Simple: `"Hello ${name}"`
- Nested: `"${foo.${bar}}"`
- Escaped: `"Not \${interpolated}"`
- Multiline with interpolation: `''Hello ${name}''`

## Performance

Oxidix is designed for high performance:

- **Zero-copy lexing**: String tokens reference the original source
- **Efficient interpolation**: Nested lexing for complex string expressions  
- **Optimized scanning**: Uses `memchr` for fast character operations
- **Minimal allocations**: Careful memory management throughout

## Architecture

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Nix Source  │───▶│ nix-lexer   │───▶│ nix-ast     │
│ Code        │    │ (Tokens)    │    │ (AST)       │
└─────────────┘    └─────────────┘    └─────────────┘
```

### nix-lexer
- Tokenizes Nix source code using the `logos` crate
- Handles complex string interpolation with nested expressions
- Provides span information for error reporting
- Zero-copy operation with string slices

### nix-ast  
- Builds abstract syntax trees from token streams
- Implements operator precedence and associativity rules
- Comprehensive error handling with `ariadne` diagnostics
- Type-safe representation of all Nix language constructs

## Testing

Run the test suite:

```bash
cargo test
```

The project uses snapshot testing with `insta` for comprehensive coverage:

```bash
# Review snapshot changes
cargo insta review

# Accept all changes
cargo insta accept
```

### Development Setup

```bash
git clone https://github.com/w4/oxidix
cd oxidix
nix develop
cargo test
```

### Code Quality

The project maintains high code quality standards:

- All Clippy lints set to "deny"
- Comprehensive test coverage
- Documentation for all public APIs
- Snapshot testing for parser correctness

## License

This project is licensed under the WTFPL (Do What The F*ck You Want To Public License).

## Related Projects

- [Nix](https://nixos.org/) - The Nix package manager and language
- [rnix-parser](https://github.com/nix-community/rnix-parser) - Another Rust Nix parser
- [tree-sitter-nix](https://github.com/cstrahan/tree-sitter-nix) - Tree-sitter grammar for Nix

## Acknowledgments

- Built with [logos](https://github.com/maciejhirsz/logos) for efficient lexing
- Error reporting powered by [ariadne](https://github.com/zesterer/ariadne)
- Testing with [insta](https://github.com/mitsuhiko/insta) snapshot testing
