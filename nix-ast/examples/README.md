# Oxidix Examples

This directory contains practical examples demonstrating how to use the `nix-lexer` and `nix-ast` crates to work with Nix source code.

## Examples Overview

### 1. [basic_lexing.rs](basic_lexing.rs)
**Basic Lexing Example**

Demonstrates fundamental lexing operations:
- Tokenizing simple Nix expressions
- Working with spans for error reporting
- Handling string interpolation
- Processing multiline strings

**Run with:**
```bash
cargo run --example basic_lexing
```

**Key concepts:**
- Creating lexers with `Token::lexer()`
- Iterating over tokens with spans
- Understanding `StringToken` components
- Error handling for invalid tokens

### 2. [nix_formatter.rs](nix_formatter.rs)
**Nix Code Formatter**

A practical example building a simple code formatter:
- Parsing and reformatting Nix code
- Handling operator precedence
- Managing parentheses correctly
- Formatting lambda expressions

**Run with:**
```bash
cargo run --example nix_formatter
```

**Key concepts:**
- Building tools with oxidix crates
- AST traversal and reconstruction
- Operator precedence handling
- Code generation from AST

## Running Examples

### Prerequisites

Make sure you have Rust installed and the project dependencies:

```bash
cd oxidix
cargo build
```

### Running Individual Examples

```bash
# Run a specific example
cargo run --example basic_lexing

# Run with output
cargo run --example ast_parsing 2>&1 | less

# Run tests for examples
cargo test --examples
```

### Running All Examples

```bash
# Run all examples in sequence
for example in basic_lexing nix_formatter; do
    echo "Running $example..."
    cargo run --example $example
    echo "---"
done
```

## Example Patterns

### Basic Lexing Pattern

```rust
use nix_lexer::{Token, Logos};

let source = "let x = 42; in x";
let tokens: Result<Vec<Token>, _> = Token::lexer(source).collect();
```

### Basic Parsing Pattern

```rust
use nix_ast::parse_expression;
use nix_lexer::{Token, Logos};

let source = "x: x + 1";
let lexer = Token::lexer(source);
let ast = parse_expression(lexer)?;
```

### Error Handling Pattern

```rust
use nix_ast::{parse_expression, format_error};
use std::path::Path;

match parse_expression(Token::lexer(source)) {
    Ok(ast) => { /* process AST */ },
    Err(error) => {
        let diagnostic = format_error(error, Path::new("file.nix"));
        eprintln!("{}", diagnostic);
    }
}
```

### String Interpolation Pattern

```rust
use nix_lexer::{Token, StringToken, Logos};

if let Some(Ok(Token::String(tokens))) = Token::lexer(source).next() {
    for token in tokens {
        match token {
            StringToken::Content(text) => { /* handle text */ },
            StringToken::Interpolation(expr_tokens) => { /* handle expression */ },
        }
    }
}
```

## Common Use Cases

### 1. Syntax Highlighting

Use the lexer to tokenize code and apply syntax highlighting based on token types.

### 2. Code Formatting

Parse code into an AST, then regenerate formatted source code with consistent style.

### 3. Static Analysis

Traverse the AST to find patterns, detect issues, or gather metrics about Nix code.

### 4. Code Transformation

Parse, modify the AST, and regenerate code for refactoring or code generation.

### 5. Language Servers

Build language server features like completion, hover information, and diagnostics.

## Performance Tips

### Lexing Performance

- Use `Token::lexer()` directly for simple tokenization
- Use `.spanned()` only when you need position information
- Collect tokens into `Vec` only when necessary

### Parsing Performance

- Parse incrementally when possible
- Cache parsed results for frequently accessed files
- Use error recovery to continue parsing after errors

### Memory Usage

- The lexer uses zero-copy string slices
- AST nodes reference the original source when possible
- Consider using `Rc` or `Arc` for shared AST nodes

## Testing Examples

Each example includes unit tests demonstrating key functionality:

```bash
# Test all examples
cargo test --examples

# Test specific example
cargo test --example basic_lexing
```

## Integration Examples

### With Clap (CLI)

```rust
use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    file: PathBuf,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.file)?;
    let ast = parse_expression(Token::lexer(&source))?;
    // Process AST...
}
```

### With Serde (Serialization)

```rust
use serde_json;

let ast = parse_expression(Token::lexer(source))?;
let json = serde_json::to_string_pretty(&ast)?;
println!("{}", json);
```

### With Rayon (Parallel Processing)

```rust
use rayon::prelude::*;

let files: Vec<PathBuf> = /* collect files */;
let results: Vec<_> = files
    .par_iter()
    .map(|path| {
        let source = std::fs::read_to_string(path)?;
        parse_expression(Token::lexer(&source))
    })
    .collect();
```

## Contributing Examples

When adding new examples:

1. **Focus on practical use cases** - Show real-world applications
2. **Include comprehensive comments** - Explain the why, not just the how
3. **Add unit tests** - Demonstrate expected behavior
4. **Update this README** - Document the new example
5. **Follow naming conventions** - Use descriptive, consistent names

### Example Template

```rust
//! Brief description of what this example demonstrates.
//! 
//! This example shows how to:
//! - Key concept 1
//! - Key concept 2
//! - Key concept 3

use nix_lexer::{Token, Logos};
use nix_ast::parse_expression;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example implementation
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_example_functionality() {
        // Test implementation
    }
}
```

## Further Reading

- [Project README](../../README.md)
- [Nix Language Reference](https://nixos.org/manual/nix/stable/language/)
- [Logos Documentation](https://docs.rs/logos/)
- [Ariadne Documentation](https://docs.rs/ariadne/)
