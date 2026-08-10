use std::fmt;

use xc_ast::literal::{Literal, LiteralKind};
use xc_span::symbol::kw;
use xc_span::Symbol;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Void,
    Nil,
    Bool(bool),
    Int(i128),
    String(String),
    Symbol(Symbol),
    Array(Vec<Value>),
}

impl Value {
    pub fn from_lit(lit: &Literal) -> Result<Self, String> {
        use LiteralKind::*;
        Ok(match &lit.kind {
            Nil => Value::Nil,
            Bool => Value::Bool(lit.value == kw::True),
            Integer => {
                let s = lit.value.as_str();
                let (s, radix) = if let Some(rest) = s.strip_prefix("0x") {
                    (rest, 16)
                } else if let Some(rest) = s.strip_prefix("0b") {
                    (rest, 2)
                } else {
                    (s, 10)
                };
                Value::Int(
                    i128::from_str_radix(s, radix)
                        .map_err(|_| format!("invalid integer literal: {s}"))?,
                )
            }
            String => Value::String(lit.value.to_string()),
            NameLit => Value::Symbol(lit.value),
            _ => return Err(format!("unsupported literal in this build: {lit}")),
        })
    }
}

pub fn write_value(v: &Value, out: &mut impl fmt::Write) -> fmt::Result {
    match v {
        Value::Void => write!(out, "void"),
        Value::Nil => write!(out, "nil"),
        Value::Bool(b) => write!(out, "{b}"),
        Value::Int(i) => write!(out, "{i}"),
        Value::String(s) => write!(out, "{s}"),
        Value::Symbol(s) => write!(out, "\\{s}"),
        Value::Array(items) => {
            write!(out, "[")?;
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write_value(item, out)?;
            }
            write!(out, "]")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use xc_ast::literal::{Literal, LiteralKind};
    use xc_span::create_session_globals_then;
    use xc_span::Symbol;

    #[test]
    fn int_from_lit() {
        create_session_globals_then(|| {
            let lit = Literal::new(LiteralKind::Integer, Symbol::intern("42"), None);
            assert_eq!(Value::from_lit(&lit).unwrap(), Value::Int(42));
        });
    }

    #[test]
    fn write_string_raw() {
        let mut out = String::new();
        write_value(&Value::String("hi".into()), &mut out).unwrap();
        assert_eq!(out, "hi");
    }
}
