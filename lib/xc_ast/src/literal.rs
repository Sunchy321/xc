use core::fmt;
use std::os::windows::io::RawHandle;

use xc_span::Symbol;

use crate::token::Token;

#[derive(Clone, Copy)]
pub enum LiteralKind {
    Bool,
    Integer,
    Floating,
    String,
    RawString { at_count: u32 },
    MultilineString { quote_count: u32 },
    Symbol,
    Error
}

#[derive(Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: Symbol,
    pub suffix: Option<Symbol>,
}

impl Literal {
    pub fn new(kind: LiteralKind, value: Symbol, suffix: Option<Symbol>) -> Self {
        Self {
            kind,
            value,
            suffix,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        use crate::token::TokenKind::*;

        match token.kind.clone() {
            Identifier(sym) if sym.is_bool_lit() => Some(Self::new(LiteralKind::Bool, sym, None)),
            Literal(lit) => Some(lit),
            _ => None,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LiteralKind::*;

        let Literal {
            kind,
            value,
            suffix,
        } = self;

        match kind {
            Bool | Integer | Floating | Error => {
                write!(f, "{value}")?
            }
            String => write!(f, "\"{value}\"")?,
            RawString { at_count } => {
                write!(f, "{count}\"{value}\"{count}", count = "@".repeat(*at_count as usize))?
            }
            MultilineString { quote_count } => {
                write!(f, "{count}{value}{count}", count = "\"".repeat(*quote_count as usize))?
            }
            Symbol => write!(f, "'{value}")?,
        }

        if let Some(suffix) = suffix {
            write!(f, "{suffix}")?;
        }

        Ok(())
    }
}
