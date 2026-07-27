use xc_ast::expr::{Expr, ExprKind};
use xc_ast::func::FuncBody;
use xc_ast::pattern::PatternKind;
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, StmtKind};
use xc_span::symbol::op;

use crate::store::Store;
use crate::value::Value;

#[derive(Debug)]
enum ControlFlow {
    Err(String),
    Break(Option<Value>),
    Continue,
    Return(Value),
}

pub fn eval(expr: &Expr, store: &mut Store) -> Result<Value, String> {
    match eval_inner(expr, store) {
        Ok(v) => Ok(v),
        Err(ControlFlow::Err(e)) => Err(e),
        Err(ControlFlow::Break(_)) => Err("break outside loop".to_string()),
        Err(ControlFlow::Continue) => Err("continue outside loop".to_string()),
        Err(ControlFlow::Return(v)) => Ok(v),
    }
}

fn err(msg: &str) -> ControlFlow {
    ControlFlow::Err(msg.to_string())
}

fn eval_inner(expr: &Expr, store: &mut Store) -> Result<Value, ControlFlow> {
    use ExprKind::*;
    Ok(match &expr.kind {
        Placeholder => return Err(err("placeholder")),
        Literal(lit) => Value::from_lit(lit).map_err(|e| err(&e))?,
        Array(items) => {
            let mut vals = Vec::new();
            for item in items {
                match item {
                    xc_ast::expr::ExprItem::Expr(e) => vals.push(eval_inner(e, store)?),
                    _ => {} // skip spread for now
                }
            }
            Value::Array(vals)
        }
        Paren(expr) => eval_inner(expr, store)?,
        Block(block) => eval_block(block, store)?,
        Infix(sym, exprs) => {
            let mut iter = exprs.iter();
            let mut acc = eval_inner(iter.next().unwrap(), store)?;
            for expr in iter {
                let rhs = eval_inner(expr, store)?;
                acc = eval_infix(*sym, acc, rhs)?;
            }
            acc
        }
        Comparison(syms, exprs) => {
            let mut iter_sym = syms.iter();
            let mut iter_expr = exprs.iter();
            let mut acc = eval_inner(iter_expr.next().unwrap(), store)?;
            for expr in iter_expr {
                let sym = iter_sym.next().unwrap();
                let rhs = eval_inner(expr, store)?;
                let result = eval_infix(*sym, acc, rhs.clone())?;
                if let Value::Bool(false) = &result {
                    return Ok(result);
                }
                acc = rhs;
            }
            Value::Bool(true)
        }
        Prefix(sym, expr) => {
            let val = eval_inner(expr, store)?;
            eval_prefix(*sym, val)?
        }
        If(cond, then_branch, else_branch) => {
            let cond_val = eval_inner(cond, store)?;
            match cond_val {
                Value::Bool(true) => eval_inner(then_branch, store)?,
                Value::Bool(false) => {
                    if let Some(else_expr) = else_branch {
                        eval_inner(else_expr, store)?
                    } else {
                        Value::Void
                    }
                }
                _ => return Err(err("if condition must be Bool")),
            }
        }
        While(cond, body, else_block, _) => {
            let mut result = Value::Void;
            loop {
                let cond_val = eval_inner(cond, store)?;
                match cond_val {
                    Value::Bool(true) => {
                        match eval_inner(body, store) {
                            Ok(_) => {}
                            Err(ControlFlow::Break(v)) => {
                                result = v.unwrap_or(Value::Void);
                                break;
                            }
                            Err(ControlFlow::Continue) => continue,
                            Err(e) => return Err(e),
                        }
                    }
                    Value::Bool(false) => {
                        if let Some(else_expr) = else_block {
                            result = eval_inner(else_expr, store)?;
                        }
                        break;
                    }
                    _ => return Err(err("while condition must be Bool")),
                }
            }
            result
        }
        Break(val, _label) => {
            let v = val.as_ref().map(|e| eval_inner(e, store)).transpose()?;
            return Err(ControlFlow::Break(v));
        }
        Continue(_label) => return Err(ControlFlow::Continue),
        Throw(val) => {
            let v = val.as_ref().map(|e| eval_inner(e, store)).transpose()?;
            return Err(ControlFlow::Return(Value::Fail(Box::new(v.unwrap_or(Value::Void)))));
        }
        Try(expr, catchers, else_block) => {
            match eval_inner(expr, store) {
                Ok(v) => try_handle(v, catchers, else_block, store)?,
                Err(ControlFlow::Return(v)) => try_handle(v, catchers, else_block, store)?,
                Err(e) => return Err(e),
            }
        }
        For(pat, iter_expr, body, else_block, _kind, _label) => {
            let iter_val = eval_inner(iter_expr, store)?;
            match iter_val {
                Value::Array(items) => {
                    let mut result = Value::Void;
                    for item in &items {
                        bind_pattern(pat, item.clone(), store);
                        match eval_block(body, store) {
                            Ok(_) => {}
                            Err(ControlFlow::Break(v)) => {
                                result = v.unwrap_or(Value::Void);
                                break;
                            }
                            Err(ControlFlow::Continue) => continue,
                            Err(e) => return Err(e),
                        }
                    }
                    result
                }
                _ => return Err(err("for loop requires an array")),
            }
        }
        Return(val) => {
            let v = val.as_ref().map(|e| eval_inner(e, store)).transpose()?;
            return Err(ControlFlow::Return(v.unwrap_or(Value::Void)));
        }
        Path(path) => {
            if path.segments.len() == 1 {
                let name = path.segments[0].ident.name;
                store.get(name).cloned().ok_or_else(|| err("undefined variable"))?
            } else {
                return Err(err("qualified paths not supported"));
            }
        }
        FuncCall(func_expr, args, _) => {
            let name = match &func_expr.kind {
                ExprKind::Path(path) if path.segments.len() == 1 => path.segments[0].ident.name,
                _ => return Err(err("function name must be a simple identifier")),
            };
            let func = store.get_func(name).cloned()
                .ok_or_else(|| err("undefined function"))?;
            let params = &func.sig.params;
            if params.len() != args.0.len() {
                return Err(err("wrong number of arguments"));
            }
            let saved: Vec<_> = params.iter()
                .map(|p| (p.ident.name, store.get(p.ident.name).cloned()))
                .collect();
            for (param, arg) in params.iter().zip(args.0.iter()) {
                let arg_expr = match arg {
                    xc_ast::expr::Argument::Ordinal(e)
                    | xc_ast::expr::Argument::Named(_, e)
                    | xc_ast::expr::Argument::Expansion(e) => e,
                };
                let val = eval_inner(arg_expr, store)?;
                store.set(param.ident.name, val);
            }
            let result = match &func.body {
                FuncBody::Block(block) => eval_block(block, store),
                FuncBody::Arrow(expr) => eval_inner(expr, store),
                FuncBody::Semicolon => Ok(Value::Void),
            };
            let result = match result {
                Ok(v) => v,
                Err(ControlFlow::Return(v)) => v,
                Err(e) => return Err(e),
            };
            // If function has throw qualifier, wrap success in Ok
            let has_throw = !matches!(&func.sig.tail.throw, xc_ast::func::FuncThrow::None);
            let result = if has_throw && !matches!(&result, Value::Fail(_)) {
                Value::Okay(Box::new(result))
            } else {
                result
            };
            for (name, saved_val) in saved {
                match saved_val {
                    Some(v) => store.set(name, v),
                    None => { store.remove(name); }
                }
            }
            result
        }
        _ => return Err(err(&format!("eval: unimplemented {:?}", expr.kind))),
    })
}

fn eval_infix(op: xc_span::Symbol, lhs: Value, rhs: Value) -> Result<Value, ControlFlow> {
    use Value::*;
    #[allow(unused_qualifications)]
    Ok(match (op, &lhs, &rhs) {
        (op::Plus, Int(a), Int(b)) => Int(a + b),
        (op::Minus, Int(a), Int(b)) => Int(a - b),
        (op::Multiply, Int(a), Int(b)) => Int(a * b),
        (op::Divide, Int(a), Int(b)) => {
            if *b == 0 { return Err(err("division by zero")); }
            Int(a / b)
        }
        (op::Concat, String(a), String(b)) => String(format!("{}{}", a, b)),
        (op::Equal, Int(a), Int(b)) => Bool(a == b),
        (op::NotEqual, Int(a), Int(b)) => Bool(a != b),
        (op::Less, Int(a), Int(b)) => Bool(a < b),
        (op::Greater, Int(a), Int(b)) => Bool(a > b),
        (op::LessEqual, Int(a), Int(b)) => Bool(a <= b),
        (op::GreaterEqual, Int(a), Int(b)) => Bool(a >= b),
        (op::Equal, Bool(a), Bool(b)) => Bool(a == b),
        (op::NotEqual, Bool(a), Bool(b)) => Bool(a != b),
        (op::Equal, String(a), String(b)) => Bool(a == b),
        (op::NotEqual, String(a), String(b)) => Bool(a != b),
        (op::Equal, Nil, Nil) => Bool(true),
        (op::NotEqual, Nil, Nil) => Bool(false),
        (op::AndAnd, Bool(a), Bool(b)) => Bool(*a && *b),
        (op::OrOr, Bool(a), Bool(b)) => Bool(*a || *b),
        (op::Modulo, Int(a), Int(b)) => {
            if *b == 0 { return Err(err("modulo by zero")); }
            Int(a % b)
        }
        _ => return Err(err(&format!("infix not supported: {:?}", op.as_str()))),
    })
}

fn eval_prefix(op: xc_span::Symbol, val: Value) -> Result<Value, ControlFlow> {
    use Value::*;
    Ok(match (op, &val) {
        (op::Minus, Int(a)) => Int(-a),
        (op::Plus, Int(_)) => val,
        _ => return Err(err(&format!("prefix not supported: {:?}", op.as_str()))),
    })
}

fn bind_pattern(pat: &xc_ast::pattern::Pattern, value: Value, store: &mut Store) {
    match &pat.kind {
        PatternKind::Bind(ident, _) => { store.set(ident.name, value); }
        _ => {}
    }
}

fn pattern_matches(pat: &xc_ast::pattern::Pattern, _value: &Value) -> bool {
    match &pat.kind {
        PatternKind::Wildcard => true,
        PatternKind::Bind(_, _) => true,
        PatternKind::Expr(e) => {
            matches!(&e.kind, ExprKind::Path(_) | ExprKind::Literal(_))
        }
        _ => false,
    }
}

fn eval_inner_stmt(stmt: &xc_ast::stmt::Stmt, store: &mut Store) -> Result<Value, ControlFlow> {
    match &stmt.kind {
        StmtKind::Expr(expr) | StmtKind::ExprSemi(expr) => eval_inner(expr, store),
        StmtKind::Let(pat, init) => {
            let value = match init {
                Some(expr) => eval_inner(expr, store)?,
                None => Value::Void,
            };
            if let PatternKind::Bind(ident, _) = &pat.kind {
                store.set(ident.name, value.clone());
            }
            Ok(Value::Void)
        }
        StmtKind::Empty => Ok(Value::Void),
    }
}

fn try_handle(v: Value, catchers: &[xc_ast::stmt::Catcher], else_block: &Option<P<Block>>, store: &mut Store) -> Result<Value, ControlFlow> {
    match v {
        Value::Okay(ok) => Ok(*ok),
        Value::Fail(e) => {
            for c in catchers {
                if pattern_matches(&c.pattern, &*e) {
                    bind_pattern(&c.pattern, (*e).clone(), store);
                    let mut last = Value::Void;
                    for stmt in &c.stmts {
                        last = eval_inner_stmt(stmt, store)?;
                    }
                    return Ok(last);
                }
            }
            Err(ControlFlow::Return(Value::Fail(e)))
        }
        v => {
            if let Some(else_expr) = else_block {
                eval_block(else_expr, store)
            } else {
                Ok(v)
            }
        }
    }
}

fn eval_block(block: &Block, store: &mut Store) -> Result<Value, ControlFlow> {
    let mut last = Value::Void;
    for stmt in block.stmts.iter() {
        match &stmt.kind {
            StmtKind::Empty => {}
            StmtKind::Expr(expr) | StmtKind::ExprSemi(expr) => {
                last = eval_inner(expr, store)?;
            }
            StmtKind::Let(pat, init) => {
                let value = match init {
                    Some(expr) => eval_inner(expr, store)?,
                    None => Value::Void,
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
