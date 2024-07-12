use std::ops::{Deref, DerefMut};

use xc_ast::expr::{Expr, ExprKind};
use xc_ast::ptr::P;
use xc_ast::token::Delimiter;
use xc_ast::ty::Type;
use xc_error::{diag::Diagnostic, diag_ctx::DiagnosticContext};
use xc_span::Span;

use super::ParseResult;
use super::{parser::Parser, ConsumeClosingDelim};

impl<'a> Parser<'a> {
    pub fn diag_ctx(&self) -> &'a DiagnosticContext {
        &self.session.diag_ctx
    }

    pub(crate) fn recover_from_seq_parse(
        &mut self,
        delim: Delimiter,
        lo: Span,
        err: Diagnostic<'a>,
    ) -> P<Expr> {
        let guar = err.emit();
        // Recover from parse error, callers expect the closing delim to be consumed.
        self.consume_block(delim, ConsumeClosingDelim::Yes);

        self.make_expr_err(lo.to(self.prev_token.span), guar)
    }

    pub fn maybe_recover_from_bad_qpath<T: RecoverQPath>(
        &mut self,
        base: P<T>,
    ) -> ParseResult<'a, P<T>> {
        Ok(base) // TODO
    }
}

pub struct SnapshotParser<'a> {
    parser: Parser<'a>,
}

impl<'a> Deref for SnapshotParser<'a> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a> DerefMut for SnapshotParser<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

impl<'a> Parser<'a> {
    pub fn create_snapshot(&self) -> SnapshotParser<'a> {
        SnapshotParser {
            parser: self.clone(),
        }
    }

    pub fn restore_snapshot(&mut self, snapshot: SnapshotParser<'a>) {
        *self = snapshot.parser;
    }
}

pub trait RecoverQPath: Sized + 'static {
    fn to_type(&self) -> Option<P<Type>>;
}

impl RecoverQPath for Expr {
    fn to_type(&self) -> Option<P<Type>> {
        self.to_type()
    }
}
