//! Example of a simple Nix code formatter using the lexer and AST.
//! This demonstrates how to use the oxidix crates to build tools that work with Nix code.

use nix_ast::{
    Expression,
    expression::{
        BinaryAssociativity, BinaryExpression, BinaryOperator, LambdaExpression, LambdaParameters,
    },
    parse_expression,
};
use nix_lexer::{Logos, Token};
use std::fmt::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let examples = vec![
        "let x=42;y=x+1;in y",
        "x:y:x+y",
        "{x,y?42,...}@args:x+y",
        "if true then 1 else 2",
        "{x=1;y=2;}",
        "[1 2 3]",
        "obj.attr.nested or default",
        "1+2*3",
        "f x y z",
    ];

    println!("Nix Code Formatter Example");
    println!("{}", "=".repeat(50));

    for source in examples {
        println!("\nOriginal:  {}", source);

        match format_nix_expression(source) {
            Ok(formatted) => println!("Formatted: {}", formatted),
            Err(e) => println!("Error:     {}", e),
        }
    }

    Ok(())
}

fn format_nix_expression(source: &str) -> Result<String, Box<dyn std::error::Error>> {
    let lexer = Token::lexer(source);
    let ast = parse_expression(lexer)?;

    let mut formatter = NixFormatter::new();
    formatter.format_expression(&ast)?;
    Ok(formatter.output)
}

struct NixFormatter {
    output: String,
}

impl NixFormatter {
    fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    fn write(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        self.output.write_str(s)
    }

    fn format_expression(&mut self, expr: &Expression) -> Result<(), std::fmt::Error> {
        match expr {
            Expression::Bool(b) => self.write(&b.to_string()),
            Expression::Int(i) => self.write(&i.to_string()),
            Expression::Float(f) => self.write(&format!("{:?}", f)),
            Expression::Ident(name) => self.write(name),

            Expression::Binary(binary) => self.format_binary_expression(binary),
            Expression::Lambda(lambda) => self.format_lambda_expression(lambda),
            Expression::Let(_) => self.write("let ... in ..."), // Simplified
            Expression::If(_) => self.write("if ... then ... else ..."), // Simplified
            Expression::Array(_) => self.write("[ ... ]"),      // Simplified
            Expression::Attrset(_) => self.write("{ ... }"),    // Simplified
            Expression::Unary(_) => self.write("!..."),         // Simplified
            Expression::Add(add) => {
                self.format_expression(&add.0)?;
                self.write(" + ")?;
                self.format_expression(&add.1)
            }
            Expression::Throw(expression) => {
                self.write("throw ")?;
                self.format_expression(&expression.expression)
            }
            Expression::Comment => self.write("# comment"),
        }
    }

    fn format_binary_expression(
        &mut self,
        binary: &BinaryExpression,
    ) -> Result<(), std::fmt::Error> {
        let needs_parens = self.needs_parentheses(&binary.left, &binary.operator, true);

        if needs_parens {
            self.write("(")?;
        }
        self.format_expression(&binary.left)?;
        if needs_parens {
            self.write(")")?;
        }

        match binary.operator {
            BinaryOperator::AttributeSelection => {
                self.write(".")?;
                self.format_expression(&binary.right)?;
                if let Some(default) = &binary.postfix {
                    self.write(" or ")?;
                    self.format_expression(default)?;
                }
            }
            BinaryOperator::FunctionApplication => {
                self.write(" ")?;
                let needs_parens = self.needs_parentheses(&binary.right, &binary.operator, false);
                if needs_parens {
                    self.write("(")?;
                }
                self.format_expression(&binary.right)?;
                if needs_parens {
                    self.write(")")?;
                }
            }
            BinaryOperator::HasAttribute => {
                self.write(" ? ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::ListConcatenation => {
                self.write(" ++ ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Multiplication => {
                self.write(" * ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Division => {
                self.write(" / ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Addition => {
                self.write(" + ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Subtraction => {
                self.write(" - ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Update => {
                self.write(" // ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Lt => {
                self.write(" < ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Le => {
                self.write(" <= ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Gt => {
                self.write(" > ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Ge => {
                self.write(" >= ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Eq => {
                self.write(" == ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::Ne => {
                self.write(" != ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::LogicalAnd => {
                self.write(" && ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::LogicalOr => {
                self.write(" || ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::LogicalImplication => {
                self.write(" -> ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::LeftAssocPipeOperator => {
                self.write(" |> ")?;
                self.format_expression(&binary.right)?;
            }
            BinaryOperator::RightAssocPipeOperator => {
                self.write(" <| ")?;
                self.format_expression(&binary.right)?;
            }
        }

        Ok(())
    }

    fn format_lambda_expression(
        &mut self,
        lambda: &LambdaExpression,
    ) -> Result<(), std::fmt::Error> {
        match &lambda.parameters {
            LambdaParameters::Named(name) => {
                self.write(name)?;
            }
            LambdaParameters::Set(set) => {
                self.write("{ ")?;

                let mut first = true;
                for (name, param) in &set.inner {
                    if !first {
                        self.write(", ")?;
                    }
                    first = false;

                    self.write(name)?;
                    if let Some(default) = &param.default {
                        self.write(" ? ")?;
                        self.format_expression(default)?;
                    }
                }

                if set.extra_ignored {
                    if !first {
                        self.write(", ")?;
                    }
                    self.write("...")?;
                }

                self.write(" }")?;

                if let Some(bound) = set.bound {
                    self.write(" @ ")?;
                    self.write(bound)?;
                }
            }
        }

        self.write(": ")?;
        self.format_expression(&lambda.body)
    }

    fn needs_parentheses(
        &self,
        expr: &Expression,
        parent_op: &BinaryOperator,
        is_left: bool,
    ) -> bool {
        match expr {
            Expression::Binary(binary) => {
                let expr_prec = binary.operator.precedence();
                let parent_prec = parent_op.precedence();

                if expr_prec > parent_prec {
                    return true;
                }

                if expr_prec == parent_prec {
                    // Handle associativity
                    match parent_op.associativity() {
                        Some(BinaryAssociativity::Left) => !is_left,
                        Some(BinaryAssociativity::Right) => is_left,
                        None => true, // Non-associative operators need parens
                    }
                } else {
                    false
                }
            }
            Expression::Lambda(_) => {
                // Lambdas usually need parentheses in binary expressions
                true
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_formatting() {
        assert_eq!(format_nix_expression("1+2").unwrap(), "1 + 2");
        assert_eq!(format_nix_expression("x:x+1").unwrap(), "x: x + 1");
    }

    #[test]
    fn test_precedence_formatting() {
        // Test that precedence is preserved
        assert_eq!(format_nix_expression("1+2*3").unwrap(), "1 + 2 * 3");
        assert_eq!(format_nix_expression("(1+2)*3").unwrap(), "(1 + 2) * 3");
    }

    #[test]
    fn test_lambda_formatting() {
        assert_eq!(format_nix_expression("x:y:x+y").unwrap(), "x: y: x + y");
        assert_eq!(
            format_nix_expression("{x,y}:x+y").unwrap(),
            "{ x, y }: x + y"
        );
    }

    #[test]
    fn test_attribute_selection() {
        assert_eq!(format_nix_expression("obj.attr").unwrap(), "obj.attr");
        assert_eq!(
            format_nix_expression("obj.attr or default").unwrap(),
            "obj.attr or default"
        );
    }
}
