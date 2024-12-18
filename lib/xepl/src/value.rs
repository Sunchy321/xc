use std::fmt::Display;

use xc_ast::literal::{Literal, LiteralKind};
use xc_span::symbol::kw;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i128),
}

impl Value {
    pub fn from_lit(lit: Literal) -> Self {
        use LiteralKind::*;

        match lit.kind {
            Bool => if lit.value == kw::True {
                Value::Bool(true)
            } else {
                Value::Bool(false)
            },

            _ => todo!()
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
        }
    }
}