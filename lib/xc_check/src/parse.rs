use xc_ast::decl::DeclKind;
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

