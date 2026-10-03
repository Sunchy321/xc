use thin_vec::ThinVec;
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_ast::token::{Delimiter, TokenKind};
use xc_span::symbol::{kw, op};
use xc_span::Span;

use super::Restrictions;
use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(crate) fn make_block(&self, stmts: ThinVec<Stmt>, span: Span) -> P<Block> {
        P(Block { stmts, span })
    }

    pub(crate) fn make_stmt(&self, kind: StmtKind, span: Span) -> Stmt {
        Stmt { kind, span }
    }

    pub fn parse_block(&mut self) -> ParseResult<'a, P<Block>> {
        if !self.eat(&TokenKind::OpenDelim(Delimiter::Brace)) {
            unimplemented!();
        }

        let lo = self.prev_token.span;

        let mut stmts = ThinVec::new();

        while !self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
            if self.token.kind == TokenKind::Eof {
                break;
            }

            let stmt = match self.parse_full_stmt() {
                Ok(stmt) => stmt,
                Err(err) => {
                    return Err(err);
                }
            };

            if let Some(stmt) = stmt {
                stmts.push(stmt);
            } else {
                continue;
            }
        }

        let block = self.make_block(stmts, lo.to(self.prev_token.span));

        Ok(block)
    }

    pub fn parse_full_stmt(&mut self) -> ParseResult<'a, Option<Stmt>> {
        use StmtKind::*;

        let Some(mut stmt) = self.parse_stmt_no_recovery()? else {
            return Ok(None);
        };

        let mut eat_semi = true;
        let mut add_semi = false;

        match &mut stmt.kind {
            StmtKind::Let(..)
                if !self.token.is_keyword(kw::Else)
                    && !matches!(
                        self.token.kind,
                        TokenKind::Eof | TokenKind::CloseDelim(Delimiter::Brace)
                    ) =>
            {
                // The binding grammar ends with `;` (the `else` block form
                // takes none).
                self.expect_semi()?;
            }

            StmtKind::Let(..) | StmtKind::ExprSemi(..) | StmtKind::Empty => {}
            Expr(expr) if self.token.kind != TokenKind::Eof && expr.require_semi_to_be_stmt() => {
                // A non-block expression statement needs `;` (or to end the
                // block as its tail expression); it is not optional.
                let expect_result = self.expect_one_of(
                    &[],
                    &[
                        TokenKind::Semicolon,
                        TokenKind::CloseDelim(Delimiter::Brace),
                    ],
                );
            }

            Expr(..) | StmtKind::Let(..) => {}

            Empty | ExprSemi(..) => {
                eat_semi = false;
            }
        }

        if add_semi || (eat_semi && self.eat(&TokenKind::Semicolon)) {
            stmt = stmt.add_trailing_semicolon();
        }

        stmt.span = stmt.span.to(self.prev_token.span);

        Ok(Some(stmt))
    }

    pub fn parse_stmt_no_recovery(&mut self) -> ParseResult<'a, Option<Stmt>> {
        let lo = self.token.span;

        let stmt = if self.token.is_keyword(kw::Let) {
            self.next();

            let mut restrictions = Restrictions::PAT_IN_LET | Restrictions::NO_EQUAL;

            // `let mut x = ...`
            if self.token.is_keyword(kw::Mut) {
                self.next();
                restrictions |= Restrictions::PAT_IN_LET_MUT;
            }

            let pat = self.with_res(restrictions, |this| this.parse_pattern())?;
            let init = if self.eat(&TokenKind::Op(op::Assign)) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            let span = lo.to(self.prev_token.span);
            self.make_stmt(StmtKind::Let(pat, init), span)
        } else if self.check(&TokenKind::Semicolon) {
            // The grammar has no empty statement: a stray `;` is an error.
            return Err(self
                .diag_ctx()
                .struct_span_error(self.token.span, "unexpected `;`"));
        } else if self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            // expr stmt
            let expr = self.parse_expr_res(Restrictions::STMT_EXPR)?;
            let span = lo.to(expr.span);

            self.make_stmt(StmtKind::Expr(expr), span)
        } else {
            return Ok(None);
        };

        Ok(Some(stmt))
    }
}
