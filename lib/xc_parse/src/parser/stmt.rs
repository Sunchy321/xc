use thin_vec::ThinVec;
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_ast::token::{Delimiter, TokenKind};
use xc_span::symbol::kw;
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
        self.parse_block_impl(true)
    }

    pub(crate) fn parse_block_impl(
        &mut self,
        maybe_struct_literal: bool,
    ) -> ParseResult<'a, P<Block>> {
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
                Err(..) => {
                    todo!();
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
            Expr(expr)
                if expr.require_semi_to_be_stmt() &&
                    // TODO: Expr::attr
                    ![TokenKind::Eof, TokenKind::Semicolon, TokenKind::CloseDelim(Delimiter::Brace)].contains(&self.token.kind) =>
            {
                todo!()
            }

            Expr(expr) if self.token.kind != TokenKind::Eof && expr.require_semi_to_be_stmt() => {
                let expect_result = self.expect_one_of(
                    &[],
                    &[
                        TokenKind::Semicolon,
                        TokenKind::CloseDelim(Delimiter::Brace),
                    ],
                );
            }

            Expr(..) => {}

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
            unimplemented!()
        } else if self.eat(&TokenKind::Semicolon) {
            self.make_stmt(StmtKind::Empty, lo)
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
