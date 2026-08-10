use std::collections::HashMap;

use xc_ast::expr::{Argument, Expr, ExprItem, ExprKind};
use xc_ast::func::{Func, FuncBody};
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_span::Symbol;

use crate::parse::Parsed;

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<P<Func>>,
    pub func_indices: HashMap<Symbol, u32>,
    pub top: Vec<Stmt>,
}

pub fn check(parsed: Parsed) -> Result<Program, String> {
    let mut func_indices = HashMap::new();
    for (i, f) in parsed.funcs.iter().enumerate() {
        let idx = i as u32 + 1;
        if func_indices.insert(f.ident.name, idx).is_some() {
            return Err(format!("duplicate function: {}", f.ident.as_str()));
        }
    }

    let program = Program {
        funcs: parsed.funcs,
        func_indices,
        top: parsed.top,
    };

    for f in &program.funcs {
        check_func(f, &program)?;
    }
    for stmt in &program.top {
        check_stmt(stmt, &program)?;
    }
    Ok(program)
}

fn check_func(f: &P<Func>, program: &Program) -> Result<(), String> {
    if !matches!(f.sig.tail.throw, xc_ast::func::FuncThrow::None) {
        return Err(format!(
            "throw qualifier not supported yet: {}",
            f.ident.as_str()
        ));
    }
    match &f.body {
        FuncBody::Block(block) => check_block(block, program),
        FuncBody::Arrow(e) => check_expr(e, program),
        FuncBody::Semicolon => Err(format!("function without body: {}", f.ident.as_str())),
    }
}

fn check_stmt(s: &Stmt, program: &Program) -> Result<(), String> {
    match &s.kind {
        StmtKind::Expr(e) | StmtKind::ExprSemi(e) => check_expr(e, program),
        StmtKind::Let(_, init) => {
            if let Some(init) = init {
                check_expr(init, program)?;
            }
            Ok(())
        }
        StmtKind::Empty => Ok(()),
    }
}

fn check_block(b: &Block, program: &Program) -> Result<(), String> {
    for stmt in &b.stmts {
        check_stmt(stmt, program)?;
    }
    Ok(())
}

fn check_expr(e: &Expr, program: &Program) -> Result<(), String> {
    use ExprKind::*;
    match &e.kind {
        Placeholder | This | Caret | Dollar | LambdaArgOrdinal(_) | LambdaArgNamed(_)
        | AnonEnumerator(_) | Struct(_) | Dict(_) | Lambda(_) | Match(..) | Try(..)
        | Is(..) | NotIs(..) | Suffix(..) | MemberAccess(..) | MemberDynamicAccess(..)
        | MethodCall(..) | Subscript(..) | Cast(..) | Await(..) => {
            Err(format!("unsupported expression in this build: {e:?}"))
        }
        Literal(_) | Nil | Path(_) => Ok(()),
        Paren(inner) => check_expr(inner, program),
        Array(items) => {
            for item in items {
                if let ExprItem::Expr(e) = item {
                    check_expr(e, program)?;
                }
            }
            Ok(())
        }
        Block(b) => check_block(b, program),
        If(c, t, el) => {
            check_expr(c, program)?;
            check_expr(t, program)?;
            if let Some(el) = el {
                check_expr(el, program)?;
            }
            Ok(())
        }
        For(_, iter, body, else_, ..) => {
            check_expr(iter, program)?;
            check_block(body, program)?;
            if let Some(e) = else_ {
                check_block(e, program)?;
            }
            Ok(())
        }
        While(c, body, el, _) => {
            check_expr(c, program)?;
            check_expr(body, program)?;
            if let Some(el) = el {
                check_expr(el, program)?;
            }
            Ok(())
        }
        Return(v) | Throw(v) | Break(v, _) => {
            if let Some(v) = v {
                check_expr(v, program)?;
            }
            Ok(())
        }
        Continue(_) => Ok(()),
        Prefix(_, x) => check_expr(x, program),
        Infix(_, exprs) => {
            for e in exprs {
                check_expr(e, program)?;
            }
            Ok(())
        }
        Comparison(_, exprs) => {
            for e in exprs {
                check_expr(e, program)?;
            }
            Ok(())
        }
        FuncCall(callee, args, _) => {
            let name = match &callee.kind {
                ExprKind::Path(p) if p.segments.len() == 1 => p.segments[0].ident.name,
                _ => return Err("only simple function calls are supported".to_string()),
            };
            if name.as_str() == "print" {
                for arg in &args.0 {
                    if let Argument::Ordinal(e) = arg {
                        check_expr(e, program)?;
                    }
                }
                return Ok(());
            }
            program
                .func_indices
                .get(&name)
                .ok_or_else(|| format!("undefined function: {}", name.as_str()))?;
            let f = program
                .funcs
                .iter()
                .find(|f| f.ident.name == name)
                .expect("func index present but func missing");
            let expected = f.sig.params.len();
            let mut actual = 0;
            for arg in &args.0 {
                match arg {
                    Argument::Ordinal(e) => {
                        actual += 1;
                        check_expr(e, program)?;
                    }
                    Argument::Named(..) => {
                        return Err("named arguments not supported yet".to_string())
                    }
                    Argument::Expansion(..) => {
                        return Err("expansion arguments not supported yet".to_string())
                    }
                }
            }
            if actual != expected {
                return Err(format!(
                    "function {} expects {expected} arguments, got {actual}",
                    name.as_str()
                ));
            }
            Ok(())
        }
        Error(_) => Err("internal: parse error node".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_program;
    use xc_span::create_session_globals_then;
    use xc_span::source_map::Filename;

    fn run_check(src: &str) -> Result<Program, String> {
        create_session_globals_then(|| {
            let parsed = parse_program(Filename::Normal("test".into()), src)?;
            check(parsed)
        })
    }

    #[test]
    fn duplicate_function_is_error() {
        let err = run_check("func f() {} func f() {}").unwrap_err();
        assert!(err.contains("duplicate function: f"), "{err}");
    }

    #[test]
    fn undefined_function_is_error() {
        let err = run_check("foo(1);").unwrap_err();
        assert!(err.contains("undefined function: foo"), "{err}");
    }

    #[test]
    fn arity_mismatch_is_error() {
        let err = run_check("func f(a: int, b: int) {} f(1);").unwrap_err();
        assert!(err.contains("expects 2 arguments, got 1"), "{err}");
    }

    #[test]
    fn recursion_is_allowed() {
        let p = run_check("func f(n: int) { return f(n - 1); } print(f(1));").unwrap();
        assert_eq!(p.funcs.len(), 1);
    }
}
