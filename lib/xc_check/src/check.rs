use std::collections::{HashMap, HashSet};

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
    /// 程序内被 `let` / 赋值绑定过的名字：名字调用（`c()`）的放行判据——
    /// 不在此集合且非模块函数的名字仍报 undefined function（M1 折中）。
    pub var_names: HashSet<Symbol>,
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
        var_names: HashSet::new(),
        top: parsed.top,
    };

    let mut var_names = HashSet::new();
    for f in &program.funcs {
        if let FuncBody::Block(b) = &f.body {
            collect_var_stmts(&b.stmts, &mut var_names);
        } else if let FuncBody::Arrow(e) = &f.body {
            collect_var_expr(e, &mut var_names);
        }
    }
    collect_var_stmts(&program.top, &mut var_names);
    let program = Program { var_names, ..program };

    for f in &program.funcs {
        check_func(f, &program)?;
    }
    for stmt in &program.top {
        check_stmt(stmt, &program)?;
    }
    Ok(program)
}

// ---- 变量名收集（名字调用的放行判据）----

fn collect_var_stmts(stmts: &[Stmt], set: &mut HashSet<Symbol>) {
    for s in stmts {
        match &s.kind {
            StmtKind::Empty => {}
            StmtKind::Expr(e) | StmtKind::ExprSemi(e) => collect_var_expr(e, set),
            StmtKind::Let(pat, init) => {
                if let xc_ast::pattern::PatternKind::Bind(ident, _) = &pat.kind {
                    set.insert(ident.name);
                }
                if let Some(init) = init {
                    collect_var_expr(init, set);
                }
            }
        }
    }
}

fn collect_var_expr(e: &Expr, set: &mut HashSet<Symbol>) {
    match &e.kind {
        ExprKind::Infix(sym, exprs) => {
            if *sym == xc_span::symbol::op::Assign {
                if let Some(first) = exprs.first() {
                    if let ExprKind::Path(p) = &first.kind {
                        if p.segments.len() == 1 {
                            set.insert(p.segments[0].ident.name);
                        }
                    }
                }
            }
            for e in exprs {
                collect_var_expr(e, set);
            }
        }
        ExprKind::Array(items) => {
            for item in items {
                if let ExprItem::Expr(e) = item {
                    collect_var_expr(e, set);
                }
            }
        }
        ExprKind::Block(b) => collect_var_stmts(&b.stmts, set),
        ExprKind::If(c, t, el) => {
            collect_var_expr(c, set);
            collect_var_expr(t, set);
            if let Some(el) = el {
                collect_var_expr(el, set);
            }
        }
        ExprKind::While(c, body, el, _) => {
            collect_var_expr(c, set);
            collect_var_expr(body, set);
            if let Some(el) = el {
                collect_var_expr(el, set);
            }
        }
        ExprKind::For(pat, iter, body, else_, ..) => {
            collect_var_expr(iter, set);
            if let xc_ast::pattern::PatternKind::Bind(ident, _) = &pat.kind {
                set.insert(ident.name);
            }
            collect_var_stmts(&body.stmts, set);
            if let Some(el) = else_ {
                collect_var_stmts(&el.stmts, set);
            }
        }
        ExprKind::Lambda(l) => match &l.body {
            FuncBody::Block(b) => collect_var_stmts(&b.stmts, set),
            FuncBody::Arrow(e2) => collect_var_expr(e2, set),
            FuncBody::Semicolon => {}
        },
        ExprKind::Subscript(base, args) | ExprKind::FuncCall(base, args, _) => {
            collect_var_expr(base, set);
            for arg in &args.0 {
                if let Argument::Ordinal(e) = arg {
                    collect_var_expr(e, set);
                }
            }
        }
        ExprKind::Prefix(_, x) | ExprKind::Paren(x) => collect_var_expr(x, set),
        ExprKind::Comparison(_, exprs) => {
            for e in exprs {
                collect_var_expr(e, set);
            }
        }
        ExprKind::Return(v) | ExprKind::Throw(v) | ExprKind::Break(v, _) => {
            if let Some(v) = v {
                collect_var_expr(v, set);
            }
        }
        _ => {}
    }
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
        Placeholder | This | SelfExpr | MacroCall(..) | Caret | Dollar | LambdaArgOrdinal(_)
        | LambdaArgNamed(_) | AnonEnumerator(_) | Struct(_) | Dict(_) | Match(..)
        | Try(..) | Is(..) | NotIs(..) | Suffix(..) | MemberAccess(..)
        | MemberDynamicAccess(..) | MethodCall(..) | Cast(..) | Await(..)
        | Let(..) => {
            Err(format!("unsupported expression in this build: {e:?}"))
        }
        Lambda(l) => match &l.body {
            FuncBody::Block(b) => check_block(b, program),
            FuncBody::Arrow(e) => check_expr(e, program),
            FuncBody::Semicolon => Ok(()),
        },
        Subscript(base, args) => {
            check_expr(base, program)?;
            for arg in &args.0 {
                if let Argument::Ordinal(e) = arg {
                    check_expr(e, program)?;
                }
            }
            Ok(())
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
            // 被调者可为任意表达式（变量持有闭包、链式调用）；仅当为已知模块
            // 函数名时做实参个数检查。
            if let ExprKind::Path(p) = &callee.kind {
                if p.segments.len() == 1 {
                    let name = p.segments[0].ident.name;
                    if name.as_str() == "print" {
                        for arg in &args.0 {
                            if let Argument::Ordinal(e) = arg {
                                check_expr(e, program)?;
                            }
                        }
                        return Ok(());
                    }
                    if !program.func_indices.contains_key(&name)
                        && !program.var_names.contains(&name)
                    {
                        return Err(format!("undefined function: {}", name.as_str()));
                    }
                    if let Some(f) = program.funcs.iter().find(|f| f.ident.name == name) {
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
                                    return Err(
                                        "expansion arguments not supported yet".to_string()
                                    )
                                }
                            }
                        }
                        if actual != expected {
                            return Err(format!(
                                "function {} expects {expected} arguments, got {actual}",
                                name.as_str()
                            ));
                        }
                        return Ok(());
                    }
                }
            }
            check_expr(callee, program)?;
            for arg in &args.0 {
                match arg {
                    Argument::Ordinal(e) => check_expr(e, program)?,
                    Argument::Named(..) => {
                        return Err("named arguments not supported yet".to_string())
                    }
                    Argument::Expansion(..) => {
                        return Err("expansion arguments not supported yet".to_string())
                    }
                }
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
