use termcolor::{ColorSpec, WriteColor};
use xc_ast::literal::{Literal, LiteralKind};
use xc_span::symbol::kw;

#[derive(Clone, Debug)]
pub enum Value {
    Void,
    Nil,
    Bool(bool),
    Int(i128),
    String(String),
    Array(Vec<Value>),
}

impl Value {
    pub fn from_lit(lit: &Literal) -> Result<Self, String> {
        use LiteralKind::*;

        let value = match &lit.kind {
            Nil => Value::Nil,

            Bool => {
                if lit.value == kw::True {
                    Value::Bool(true)
                } else {
                    Value::Bool(false)
                }
            }

            Integer => {
                let string = lit.value.as_str();

                let (string, radix) = if string.starts_with("0x") {
                    (&string[2..], 16)
                } else if string.starts_with("0b") {
                    (&string[2..], 2)
                } else {
                    (&string[..], 10)
                };

                let value = match i128::from_str_radix(string, radix) {
                    Ok(value) => value,
                    Err(_) => return Err(format!("invalid integer: {}", string).to_string()),
                };

                Value::Int(value)
            }

            String => Value::String(lit.value.to_string()),

            _ => todo!(),
        };

        Ok(value)
    }

    pub fn print_value(
        &self,
        writer: &mut impl WriteColor,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use Value::*;

        match self {
            Void => write!(writer, "void")?,
            Nil => write!(writer, "nil")?,
            Bool(b) => write!(writer, "{}", b)?,
            Int(i) => write!(writer, "{}", i)?,
            String(s) => write!(writer, "\"{}\"", s)?,
            Array(items) => {
                write!(writer, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(writer, ", ")?; }
                    item.print_value(writer)?;
                }
                write!(writer, "]")?
            }
        }

        Ok(())
    }

    pub fn print_type(
        &self,
        writer: &mut impl WriteColor,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use Value::*;

        match self {
            Void => write!(writer, "void")?,
            Nil => write!(writer, "<unknown>?")?,
            Bool(_) => write!(writer, "bool")?,
            Int(_) => write!(writer, "int")?,
            String(_) => write!(writer, "string")?,
            Array(items) => write!(writer, "[{}]", items.len())?,
        }

        Ok(())
    }
}

pub fn println_value(
    value: Value,
    writer: &mut impl WriteColor,
    hint: &ColorSpec,
) -> Result<(), Box<dyn std::error::Error>> {
    value.print_value(writer)?;

    writer.set_color(hint)?;

    write!(writer, " : ")?;

    value.print_type(writer)?;

    writer.reset()?;

    writeln!(writer)?;

    Ok(())
}
