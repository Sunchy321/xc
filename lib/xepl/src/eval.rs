use xc_ast::expr::{Expr, ExprKind};

use crate::value::Value;

pub fn eval(expr: &Expr) -> Result<Value, String> {
    use ExprKind::*;

    let value = match expr.kind {
        Placeholder => return Err("placeholder".to_string()),

        Literal(lit) => Value::from_lit(lit),

        _ => return Err("unknown kind".to_string()),
    };

    Ok(value)
}