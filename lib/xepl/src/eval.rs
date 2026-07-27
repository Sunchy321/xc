use xc_ast::expr::{Expr, ExprKind};
use xc_ast::func::FuncBody;
use xc_ast::pattern::PatternKind;
use xc_ast::stmt::{Block, StmtKind};
use xc_span::symbol::op;

use crate::store::Store;
use crate::value::Value;

pub fn eval(expr: &Expr, store: &mut Store) -> Result<Value, String> {
    use ExprKind::*;

    match &expr.kind {
        Placeholder => Err("placeholder".to_string()),
        Literal(lit) => Value::from_lit(lit),
        Paren(expr) => eval(expr, store),
        Block(block) => eval_block(block, store),
        Infix(sym, exprs) => {
            let mut iter = exprs.iter();
            let mut acc = eval(iter.next().unwrap(), store)?;
            for expr in iter {
                let rhs = eval(expr, store)?;
                acc = eval_infix(*sym, acc, rhs)?;
            }
            Ok(acc)
        }
        Comparison(syms, exprs) => {
            let mut iter_sym = syms.iter();
            let mut iter_expr = exprs.iter();
            let mut acc = eval(iter_expr.next().unwrap(), store)?;
            for expr in iter_expr {
                let sym = iter_sym.next().unwrap();
                let rhs = eval(expr, store)?;
                let result = eval_infix(*sym, acc, rhs.clone())?;
                if let Value::Bool(false) = &result {
                    return Ok(result);
                }
                acc = rhs;
            }
            Ok(Value::Bool(true))
        }
        Prefix(sym, expr) => {
            let val = eval(expr, store)?;
            eval_prefix(*sym, val)
        }
        If(cond, then_branch, else_branch) => {
            let cond_val = eval(cond, store)?;
            match cond_val {
                Value::Bool(true) => eval(then_branch, store),
                Value::Bool(false) => {
                    if let Some(else_expr) = else_branch {
                        eval(else_expr, store)
                    } else {
                        Ok(Value::Nil)
                    }
                }
                _ => Err(format!("if condition must be Bool, got {:?}", cond_val)),
            }
        }
        Path(path) => {
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
        FuncCall(func_expr, args, _) => {
            let name = match &func_expr.kind {
                ExprKind::Path(path) if path.segments.len() == 1 => {
                    path.segments[0].ident.name
                }
                _ => return Err("function name must be a simple identifier".to_string()),
            };
            let func = store
                .get_func(name)
                .cloned()
                .ok_or_else(|| format!("undefined function: {}", name.as_str()))?;
            let params = &func.sig.params;
            if params.len() != args.0.len() {
                return Err(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.0.len()
                ));
            }
            let saved: Vec<_> = params
                .iter()
                .map(|p| (p.ident.name, store.get(p.ident.name).cloned()))
                .collect();
            for (param, arg) in params.iter().zip(args.0.iter()) {
                let arg_expr = match arg {
                    xc_ast::expr::Argument::Ordinal(e)
                    | xc_ast::expr::Argument::Named(_, e)
                    | xc_ast::expr::Argument::Expansion(e) => e,
                };
                let val = eval(arg_expr, store)?;
                store.set(param.ident.name, val);
            }
            let result = match &func.body {
                FuncBody::Block(block) => eval_block(block, store),
                FuncBody::Arrow(expr) => eval(expr, store),
                FuncBody::Semicolon => Ok(Value::Nil),
            };
            for (name, saved_val) in saved {
                match saved_val {
                    Some(v) => store.set(name, v),
                    None => { store.remove(name); }
                }
            }
            result
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

        (op::Equal, Int(a), Int(b)) => Ok(Bool(a == b)),
        (op::NotEqual, Int(a), Int(b)) => Ok(Bool(a != b)),
        (op::Less, Int(a), Int(b)) => Ok(Bool(a < b)),
        (op::Greater, Int(a), Int(b)) => Ok(Bool(a > b)),
        (op::LessEqual, Int(a), Int(b)) => Ok(Bool(a <= b)),
        (op::GreaterEqual, Int(a), Int(b)) => Ok(Bool(a >= b)),

        (op::Equal, Bool(a), Bool(b)) => Ok(Bool(a == b)),
        (op::NotEqual, Bool(a), Bool(b)) => Ok(Bool(a != b)),

        (op::Equal, String(a), String(b)) => Ok(Bool(a == b)),
        (op::NotEqual, String(a), String(b)) => Ok(Bool(a != b)),

        (op::Equal, Nil, Nil) => Ok(Bool(true)),
        (op::NotEqual, Nil, Nil) => Ok(Bool(false)),

        (op::AndAnd, Bool(a), Bool(b)) => Ok(Bool(*a && *b)),
        (op::OrOr, Bool(a), Bool(b)) => Ok(Bool(*a || *b)),

        (op::Modulo, Int(a), Int(b)) => {
            if *b == 0 {
                return Err("modulo by zero".to_string());
            }
            Ok(Int(a % b))
        }

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

fn eval_block(block: &Block, store: &mut Store) -> Result<Value, String> {
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
