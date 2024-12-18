use std::fmt::Display;

use termcolor::{ColorSpec, WriteColor};
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
            Nil => Value::Nil,

            Bool => if lit.value == kw::True {
                Value::Bool(true)
            } else {
                Value::Bool(false)
            },



            _ => todo!()
        }
    }

    pub fn print_value(&self, writer: &mut impl WriteColor) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            Value::Nil => write!(writer, "nil")?,
            Value::Bool(b) => write!(writer, "{}", b)?,
            Value::Int(i) => write!(writer, "{}", i)?,
        }

        Ok(())
    }

    pub fn print_type(&self, writer: &mut impl WriteColor) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            Value::Nil => write!(writer, "<unknown>?")?,
            Value::Bool(_) => write!(writer, "bool")?,
            Value::Int(_) => write!(writer, "int")?,
        }

        Ok(())
    }
}

pub fn println_value(value: Value, writer: &mut impl WriteColor, hint: &ColorSpec) -> Result<(), Box<dyn std::error::Error>> {
    value.print_value(writer)?;

    writer.set_color(hint)?;

    write!(writer, " : ")?;

    value.print_type(writer)?;

    writer.reset()?;

    writeln!(writer)?;

    Ok(())
}