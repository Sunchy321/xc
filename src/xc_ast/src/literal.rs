use core::fmt;

use crate::token::{is_bool, Token};

#[derive(Clone, Copy)]
pub enum LiteralKind {
    Bool,
    Integer,
    Floating,
    String,
    Symbol,
}

#[derive(Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: String,
    pub suffix: Option<String>,
}

impl Literal {
    pub fn new(kind: LiteralKind, value: String, suffix: Option<String>) -> Self {
        Self {
            kind,
            value,
            suffix,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        use crate::token::TokenKind::*;

        match token.kind.clone() {
            Identifier(str) if is_bool(&str) => Some(Self::new(LiteralKind::Bool, str, None)),
            Literal(lit) => Some(lit),
            _ => None,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       let Literal { kind, value, suffix } = self;

        match kind {
            LiteralKind::Bool | LiteralKind::Integer | LiteralKind::Floating => write!(f, "{value}")?,
            LiteralKind::String => write!(f, "\"{value}\"")?,
            LiteralKind::Symbol => write!(f, "'{value}")?,
        }


        if let Some(suffix) = suffix {
            write!(f, "{suffix}")?;
        }

        Ok(())
    }
}