use std::{collections::BTreeMap, iter::Peekable};

use nix_lexer::{Lexer, Token, TokenDiscriminants};

use crate::{
    BindingName, Error, Expression, expect_next_token_or_error, parse_binding_name,
    parse_expression_inner,
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct LetExpression<'a> {
    pub bindings: BTreeMap<Vec<BindingName<'a>>, Expression<'a>>,
    pub inner: Expression<'a>,
}

impl<'a> LetExpression<'a> {
    pub fn parse(stream: &mut Peekable<Lexer<'a, Token<'a>>>) -> Result<LetExpression<'a>, Error> {
        let mut bindings = BTreeMap::new();

        let inner = loop {
            match stream.peek() {
                Some(Ok(Token::In)) => {
                    stream.next();
                    break parse_expression_inner(stream, u8::MAX)?;
                }
                Some(Ok(Token::Ident(_))) => {
                    let name = parse_binding_name(stream)?;
                    let value = parse_expression_inner(stream, u8::MAX)?;
                    expect_next_token_or_error(stream, TokenDiscriminants::Semicolon)?;
                    bindings.insert(name, value);
                }
                Some(Ok(token)) => {
                    return Err(Error::UnexpectedToken(
                        token.into(),
                        vec![
                            TokenDiscriminants::In,
                            TokenDiscriminants::Ident,
                            TokenDiscriminants::Inherit,
                        ],
                    ));
                }
                Some(Err(())) => panic!(),
                None => return Err(Error::UnexpectedEndOfFile),
            }
        };

        Ok(LetExpression { bindings, inner })
    }
}
