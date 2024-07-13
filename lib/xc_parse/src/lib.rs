#![feature(let_chains, iterator_try_reduce)]

use std::{path::Path, rc::Rc};

use lexer::parse_token_tree;
use parser::parser::Parser;
use session::ParseSession;
use xc_ast::tokenstream::TokenStream;
use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_span::{source_file::SourceFile, source_map::Filename, Span};

pub mod session;
pub mod lexer;
pub mod parser;

pub fn parser_from_file<'s>(session: &'s ParseSession, path: &Path, span: Option<Span>) -> Parser<'s> {
    let source_file = session.source_map();

    unimplemented!()
}

pub fn parser_from_source_str<'a>(
    session: &'a ParseSession,
    name: Filename,
    source: String,
) -> Parser<'a> {
    let source_file = session.source_map().new_source_file(name, source);

    // TODO: exception
    unsafe { parser_from_source_file(session, source_file).unwrap_unchecked() }
}

fn parser_from_source_file<'a>(session: &'a ParseSession, source_file: Rc<SourceFile>) -> Result<Parser<'a>, Vec<Diagnostic<'a>>> {
    let end_pos = source_file.end_pos();
    let stream = file_to_stream(session, source_file)?;
    let mut parser = stream_to_parser(session, stream);

    if parser.token.kind == TokenKind::Eof {
        parser.token.span = Span::new(end_pos, end_pos)
    }

    Ok(parser)
}

fn file_to_stream<'a>(
    session: &'a ParseSession,
    source_file: Rc<SourceFile>,
) -> Result<TokenStream, Vec<Diagnostic<'a>>> {
    let src = source_file.src.as_ref().unwrap_or_else(|| {
        todo!()
    });

    parse_token_tree(session, src.as_str(), source_file.start_pos)
}

fn stream_to_parser<'a>(
    session: &'a ParseSession,
    stream: TokenStream
) -> Parser<'a> {
    Parser::new(session, stream)
}