use xc_ast::decl::{Decl, DeclKind};
use xc_ast::func::Func;
use xc_ast::ptr::P;
use xc_ast::stmt::Stmt;
use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_error::msg::DiagnosticMessage;
use xc_parse::parser_from_source_str;
use xc_parse::session::ParseSession;
use xc_span::source_map::Filename;
use xc_span::symbol::kw;
use xc_span::Span;

#[derive(Debug)]
pub struct Parsed {
    pub funcs: Vec<P<Func>>,
    pub top: Vec<Stmt>,
}

pub fn parse_program(name: Filename, source: &str) -> Result<Parsed, String> {
    let session = ParseSession::new();
    let mut parser = parser_from_source_str(&session, name, source.to_string());

    let mut funcs = Vec::new();
    let mut top = Vec::new();

    loop {
        if parser.token.kind == TokenKind::Eof {
            break;
        }
        if parser.check_keyword(kw::Func) {
            let decl = parser.parse_decl().map_err(format_diag)?;
            match decl {
                Some(decl) => match decl.kind {
                    DeclKind::Func(f) => funcs.push(f),
                    _ => return Err("unsupported top-level declaration".to_string()),
                },
                None => {
                    return Err("internal: parse_decl returned None after func keyword".to_string())
                }
            }
        } else if let Some(stmt) = parser.parse_full_stmt().map_err(format_diag)? {
            top.push(stmt);
        } else {
            break;
        }
    }

    Ok(Parsed { funcs, top })
}

fn format_diag(d: Diagnostic<'_>) -> String {
    d.messages
        .first()
        .map(|m| match m {
            DiagnosticMessage::String(s) => s.to_string(),
        })
        .unwrap_or_else(|| "parse error".to_string())
}

/// Parse a whole file as a sequence of declarations.
///
/// Unlike [`parse_program`], this does not restrict the kinds of accepted
/// declarations; it is used by the `x parse` command to check that the
/// parser accepts a source file without running later phases.
pub fn parse_file(name: Filename, source: &str) -> Result<Vec<Decl>, String> {
    let session = ParseSession::new();
    let mut parser = parser_from_source_str(&session, name, source.to_string());

    let mut decls = Vec::new();

    loop {
        if parser.token.kind == TokenKind::Eof {
            break;
        }

        let lo = parser.token.span;

        match parser.parse_decl().map_err(|d| format_diag_at(d, source))? {
            Some(decl) => decls.push(decl),
            None => {
                return Err(format!(
                    "{}: unrecognized declaration (token: {:?})",
                    line_col(source, lo),
                    parser.token.kind
                ));
            }
        }
    }

    Ok(decls)
}

fn format_diag_at(d: Diagnostic<'_>, source: &str) -> String {
    let span = d.span.get_primary();

    let msg = d
        .messages
        .first()
        .map(|m| match m {
            DiagnosticMessage::String(s) => s.to_string(),
        })
        .unwrap_or_else(|| "parse error".to_string());

    match span {
        Some(span) => format!("{}: {}", line_col(source, span), msg),
        None => msg,
    }
}

/// Best-effort `line:column` for a span within `source` (1-based).
fn line_col(source: &str, span: Span) -> String {
    let lo = (span.lo.0 as usize).min(source.len());
    let bytes = &source.as_bytes()[..lo];

    let line = bytes.iter().filter(|&&b| b == b'\n').count() + 1;
    let col = bytes.iter().rev().take_while(|&&b| b != b'\n').count() + 1;

    format!("{line}:{col}")
}

