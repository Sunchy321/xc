use xc_ast::expr::{Expr, ExprKind};
use xc_ast::pattern::PatternKind;
use xc_ast::stmt::StmtKind;
use xc_span::symbol::op;

use crate::store::Store;
use crate::value::Value;

pub fn eval(expr: &Expr, store: &mut Store) -> Result<Value, String> {
    use ExprKind::*;

    match &expr.kind {
        Placeholder => Err("placeholder".to_string()),
        Literal(lit) => Value::from_lit(lit),
        Paren(expr) => eval(expr, store),
        Block(block) => {
            let mut last = Value::Nil;
            for stmt in block.stmts.iter() {
                match &stmt.kind {
                    StmtKind::Empty => {}
                    StmtKind::Expr(expr) | StmtKind::ExprSemi(expr) => {
                        last = eval(expr, store)?;
                    }
                    StmtKind::Let(pat, init) => {
                        let value = match init {
                            Some(expr) => eval(expr, store)?,
                            None => Value::Nil,
                        };
                        if let PatternKind::Bind(ident, _) = &pat.kind {
                            store.set(ident.name, value.clone());
                        }
                        last = Value::Nil;
                    }
                }
            }
            Ok(last)
        }
        Infix(sym, exprs) => {
            let mut iter = exprs.iter();
            let mut acc = eval(iter.next().unwrap(), store)?;
            for expr in iter {
                let rhs = eval(expr, store)?;
                acc = eval_infix(*sym, acc, rhs)?;
            }
            Ok(acc)
        }
        Prefix(sym, expr) => {
            let val = eval(expr, store)?;
            eval_prefix(*sym, val)
        }
        Path(path) => {
            // Simple single-segment identifier lookup
            if path.segments.len() == 1 {
                let name = path.segments[0].ident.name;
                store
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("undefined variable: {}", name.as_str()))
            } else {
                Err("qualified paths not yet supported".to_string())
            }
        }
        _ => Err(format!("eval: unimplemented {:?}", expr.kind)),
    }
}

fn eval_infix(op: xc_span::Symbol, lhs: Value, rhs: Value) -> Result<Value, String> {
    use Value::*;

    match (op, &lhs, &rhs) {
        (op::Plus, Int(a), Int(b)) => Ok(Int(a + b)),
        (op::Minus, Int(a), Int(b)) => Ok(Int(a - b)),
        (op::Multiply, Int(a), Int(b)) => Ok(Int(a * b)),
        (op::Divide, Int(a), Int(b)) => {
            if *b == 0 {
                return Err("division by zero".to_string());
            }
            Ok(Int(a / b))
        }
        (op::Concat, String(a), String(b)) => Ok(String(format!("{}{}", a, b))),
        _ => Err(format!(
            "infix {:?} not supported for {:?} and {:?}",
            op.as_str(),
            lhs,
            rhs
        )),
    }
}

fn eval_prefix(op: xc_span::Symbol, val: Value) -> Result<Value, String> {
    use Value::*;

    match (op, &val) {
        (op::Minus, Int(a)) => Ok(Int(-a)),
        (op::Plus, Int(_)) => Ok(val),
        _ => Err(format!(
            "prefix {:?} not supported for {:?}",
            op.as_str(),
            val
        )),
    }
}
