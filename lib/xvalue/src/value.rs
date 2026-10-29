//! 运行时值表示：16 字节 fat enum（vm-bytecode-design-2026-10.md「值模型表示」）。
//!
//! - 复制白名单（int/float/bool/char/Symbol）为内联立即数；string 为不可变堆对象，
//!   复制即指针拷贝（语言裁定：复制共享其字符内容）。
//! - `Value: Copy`：寄存器传递全是位拷贝；copy 与 move 的区别（源位置是否失效）
//!   是前端语义，运行时零机制。
//! - 相等按值语义：标量比载荷、string 比内容、数组逐元素。

use std::fmt;

use xc_ast::literal::{Literal, LiteralKind};
use xc_span::symbol::kw;
use xc_span::Symbol;

use crate::heap::ObjRef;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Nil,
    Void,
    Bool(bool),
    Int(i64),
    Float(f64),
    Char(char),
    Sym(Symbol),
    Obj(ObjRef),
}

impl Value {
    /// 整数字面量解析（10/0x/0b；负号由前缀运算符承载，不出现在字面量中）。
    pub fn int_from_lit(s: &str) -> Result<i64, String> {
        let (s, radix) = if let Some(rest) = s.strip_prefix("0x") {
            (rest, 16)
        } else if let Some(rest) = s.strip_prefix("0b") {
            (rest, 2)
        } else {
            (s, 10)
        };
        i64::from_str_radix(s, radix).map_err(|_| format!("invalid integer literal: {s}"))
    }

    pub fn from_string_lit(lit: &Literal) -> Option<&str> {
        match &lit.kind {
            LiteralKind::String => Some(lit.value.as_str()),
            _ => None,
        }
    }
}

/// 判断字面量种类并解析为可入常量池的标量/标记；字符串字面量需调用方入堆
/// （`from_string_lit` 取内容），此处不建堆对象。
pub fn lit_scalar(lit: &Literal) -> Result<Option<Value>, String> {
    Ok(match &lit.kind {
        LiteralKind::Nil => Some(Value::Nil),
        LiteralKind::Bool => Some(Value::Bool(lit.value == kw::True)),
        LiteralKind::Integer => Some(Value::Int(Value::int_from_lit(lit.value.as_str())?)),
        LiteralKind::NameLit => Some(Value::Sym(lit.value)),
        _ => None,
    })
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) | (Value::Void, Value::Void) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Sym(a), Value::Sym(b)) => a == b,
            (Value::Obj(a), Value::Obj(b)) => match (a.kind(), b.kind()) {
                (crate::heap::ObjKind::Str(x), crate::heap::ObjKind::Str(y)) => x == y,
                (crate::heap::ObjKind::Arr(x), crate::heap::ObjKind::Arr(y)) => x == y,
                // cell/闭包：按 place/对象恒等比较（引用恒等语义）。
                (x, y) => a == b && std::mem::discriminant(x) == std::mem::discriminant(y),
            },
            _ => false,
        }
    }
}

pub fn write_value(v: &Value, out: &mut impl fmt::Write) -> fmt::Result {
    match v {
        Value::Void => write!(out, "void"),
        Value::Nil => write!(out, "nil"),
        Value::Bool(b) => write!(out, "{b}"),
        Value::Int(i) => write!(out, "{i}"),
        Value::Float(f) => write!(out, "{f}"),
        Value::Char(c) => write!(out, "{c}"),
        Value::Sym(s) => write!(out, "\\{s}"),
        Value::Obj(o) => match o.kind() {
            crate::heap::ObjKind::Str(s) => write!(out, "{s}"),
            crate::heap::ObjKind::Arr(items) => {
                write!(out, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    write_value(item, out)?;
                }
                write!(out, "]")
            }
            crate::heap::ObjKind::Cell(inner) => write_value(inner, out),
            crate::heap::ObjKind::ElemRef { .. } => write!(out, "<&elem>"),
            crate::heap::ObjKind::Closure { .. } => write!(out, "<func>"),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_from_lit() {
        assert_eq!(Value::int_from_lit("42"), Ok(42));
        assert_eq!(Value::int_from_lit("0x10"), Ok(16));
        assert_eq!(Value::int_from_lit("0b101"), Ok(5));
        assert!(Value::int_from_lit("99999999999999999999").is_err());
    }

    #[test]
    fn write_scalar() {
        let mut out = String::new();
        write_value(&Value::Bool(true), &mut out).unwrap();
        write_value(&Value::Int(-3), &mut out).unwrap();
        assert_eq!(out, "true-3");
    }
}
