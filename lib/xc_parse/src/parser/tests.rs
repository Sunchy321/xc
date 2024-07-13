use std::any::Any;

use xc_ast::{expr::Expr, ptr::P};
use xc_span::{create_session_globals_then, source_map::Filename};

use crate::{parser_from_source_str, session::ParseSession};

use super::{parser::Parser, ParseResult};

fn session() -> ParseSession {
    ParseSession::new()
}

fn string_to_parser(session: &ParseSession, source: String) -> Parser<'_> {
    parser_from_source_str(session, Filename::Normal("foo".to_string()), source)
}

fn check_parse<'a, T, F>(s: String, session: &'a ParseSession, f: F) -> T
where
    F: FnOnce(&mut Parser<'a>) -> ParseResult<'a, T>,
{
    let mut p = string_to_parser(&session, s);
    let x = f(&mut p).unwrap();
    // p.session.dcx.abort_if_errors();
    x
}

fn string_to_expr(source_str: String) -> P<Expr> {
    check_parse(source_str, &session(), |p| {
        let expr = p.parse_expr()?;
        println!("{:?}", expr);
        Ok(expr)
    })
}

#[test]
fn test_expr() {
    create_session_globals_then(|| {
        string_to_expr("123usize".to_string());
        string_to_expr("$0".to_string());
        string_to_expr("$index".to_string());
        string_to_expr("123 + 456 - 789".to_string());
        string_to_expr("(123)".to_string());
        string_to_expr("(123,)".to_string());
        string_to_expr("(123, 456)".to_string());
        string_to_expr("123(456, 789)".to_string());
        string_to_expr("if 1 then 2 else if 3 then 4 else 5".to_string());
        string_to_expr("break'outer 2".to_string());
        string_to_expr("continue'outer".to_string());
        string_to_expr("return 1 + 2".to_string());
        string_to_expr("throw 1".to_string());
    });
}

#[test]
fn test_block() {
    create_session_globals_then(|| {
        string_to_expr("{ }".to_string());
        string_to_expr("{ 1 }".to_string());
        string_to_expr("{ 1; 2 }".to_string());
        string_to_expr("{ 1 + 2 }".to_string());
        string_to_expr("{ a: 1 }".to_string());
        string_to_expr("{ ...1 }".to_string());
    });
}

#[test]
fn test_array() {
    create_session_globals_then(|| {
        string_to_expr("[]".to_string());
        string_to_expr("[:]".to_string());
        string_to_expr("[1]".to_string());
        string_to_expr("[...a]".to_string());
        string_to_expr("[k:v]".to_string());
        string_to_expr("[k:v,...d]".to_string());
        string_to_expr("[...d,k:v]".to_string());
    })
}

fn string_to_pattern(source_str: String) {
    check_parse(source_str, &session(), |p| {
        let pat = p.parse_pattern()?;
        println!("{:?}", pat);
        Ok(pat)
    });
}

#[test]
fn test_pattern() {
    create_session_globals_then(|| {
        string_to_pattern("_".to_string());
        // string_to_pattern("1".to_string());
        // string_to_pattern("a".to_string());
        // string_to_pattern("let a".to_string());
        // string_to_pattern("[a, b]".to_string());
        // string_to_pattern("[a, let b]".to_string());
        // string_to_pattern("let [a, b]".to_string());
        // string_to_pattern("[a, ..., z]".to_string());
        // string_to_pattern("[a, let ...m, z]".to_string());
        // string_to_pattern("let [a, ..., z]".to_string());
        // string_to_pattern("(a, b)".to_string());
        // string_to_pattern("let (a, b)".to_string());
        // string_to_pattern("(a, let b)".to_string());
        // string_to_pattern("{ k: v }".to_string());
        // string_to_pattern("{ k }".to_string());
        // string_to_pattern("{ k: let v }".to_string());
        // string_to_pattern("let { k: v }".to_string());
        // string_to_pattern("let { k }".to_string());
    })
}

fn string_to_decl(source_str: String) {
    check_parse(source_str, &session(), |p| {
        let decl = p.parse_decl()?.unwrap();
        println!("{:?}", decl);
        Ok(decl)
    });
}

#[test]
fn test_decl() {
    create_session_globals_then(|| {
        string_to_decl(
            "import core.\"prelude\" : *, a, b, c, d as e, operator, operator+, operator prefix+;"
                .to_string(),
        );
    })
}

#[test]
fn test_func() {
    create_session_globals_then(|| {
        string_to_decl("func foo() { }".to_string());
        string_to_decl("func foo(this) { }".to_string());
        string_to_decl("func foo(&mut this) { }".to_string());
        string_to_decl("func foo(this: int) { }".to_string());
        // string_to_decl("func foo(a: int) -> void { }".to_string());
    })
}
