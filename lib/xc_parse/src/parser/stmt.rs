use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_span::Span;

use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(crate) fn make_stmt(&self, kind: StmtKind, span: Span) -> Stmt {
        Stmt { kind, span }
    }

    pub fn parse_block(&mut self) -> ParseResult<'a, P<Block>> {
        unimplemented!()
    }
}