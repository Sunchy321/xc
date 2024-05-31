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
        string_to_expr("f(a, b, c)".to_string());
    });
}
