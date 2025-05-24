//! Expression types for the Nix AST.
//!
//! This module contains all the different types of expressions that can appear
//! in Nix code, from simple literals to complex constructs like lambda functions
//! and let expressions. Each expression type has its own parsing logic and
//! representation in the AST.

mod array;
mod attrset;
mod binary;
mod r#if;
mod lambda;
mod r#let;
mod throw;
mod unary;

pub use array::ArrayExpression;
pub use attrset::AttrsetExpression;
pub use binary::{BinaryAssociativity, BinaryExpression, BinaryOperator};
pub use r#if::IfExpression;
pub use lambda::{LambdaExpression, LambdaParameter, LambdaParameters};
pub use r#let::LetExpression;
pub use throw::ThrowExpression;
pub use unary::UnaryExpression;
