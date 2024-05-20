use std::f32::consts::E;
use std::net::SocketAddr;

use xc_ast::expr::{self, CastType, Expr, ExprItem, ExprKind, ForLoopKind};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::ty::Type;
use xc_error::ErrorGuaranteed;
use xc_span::symbol::kw;
use xc_span::{Identifier, Span, Symbol};

use crate::parser::SequenceSeparator;

use super::parser::Parser;
use super::{ParseResult, Restrictions};

impl<'a> Parser<'a> {
    pub(crate) fn make_expr(&self, kind: ExprKind, span: Span) -> P<Expr> {
        P(Expr { kind, span })
    }

    pub(crate) fn make_expr_err(&self, span: Span, guar: ErrorGuaranteed) -> P<Expr> {
        self.make_expr(ExprKind::Error(guar), span)
    }

    pub fn parse_expr(&mut self) -> ParseResult<'a, P<Expr>> {
        self.parse_expr_res(Restrictions::empty())
    }

    pub(crate) fn parse_expr_res(&mut self, res: Restrictions) -> ParseResult<'a, P<Expr>> {
        self.with_res(res, |this| {
            this.parse_expr_impl()
        })
    }

    fn parse_expr_item(&mut self) -> ParseResult<'a, ExprItem> {
        use TokenKind::*;

        if let DotDotDot = self.token.kind {
            self.next();
            let expr = self.parse_expr_impl()?;

            return Ok(ExprItem::ExpandExpr(expr))
        } else {
            let expr = self.parse_expr_impl()?;

            return Ok(ExprItem::Expr(expr))
        }
    }

    fn parse_expr_impl(&mut self) -> ParseResult<'a, P<Expr>> {
        // 1. collect primary exprs and operators
        // 2. convert . xxx into an operator
        // 3. convert as/is xxx into an operator
        let atoms = self.collect_atoms()?;

        // 4. process continuous primary exprs

        unimplemented!()
    }

    fn collect_atoms(&mut self) -> ParseResult<'a, Vec<ExprAtom>> {
        let mut atoms = Vec::new();

        while let Some(atom) = self.collect_atom()? {
            atoms.push(atom);
        }

        Ok(atoms)
    }

    fn collect_atom(&mut self) -> ParseResult<'a, Option<ExprAtom>> {
        use TokenKind::*;

        let atom = match self.token.kind {
            At | Pound | Semicolon | Colon | Comma | RightArrow | FatArrow | DotDotDot => return Ok(None),
            Dollar | SymbolOpen | ColonColon => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),

            OpenDelim(delim) => match delim {
                Delimiter::Paren | Delimiter::Bracket | Delimiter::Brace | Delimiter::DictBound => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),
                Delimiter::Invisible => return Ok(None),
            }

            CloseDelim(_) => return Ok(None),

            Literal(_) => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),

            Identifier(_) => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),
            LambdaArgNamed(_) => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),
            LambdaArgUnnamed(_) => ExprAtom::new(ExprAtomKind::Primary(self.parse_expr_primary()?)),

            Eof => return Ok(None),

            Op(op) => {
                self.next();
                ExprAtom::new(ExprAtomKind::Op(op))
            }
        };

        Ok(Some(atom))
    }

    fn parse_dot_part(&mut self) -> ParseResult<'a, ExprAtom> {
        let lo = self.prev_token.span;

        match self.token.kind {
            TokenKind::Identifier(sym) if sym == kw::Await => {
                Ok(ExprAtom::new(ExprAtomKind::DotAwait).force_suffix())
            }

            _ => unimplemented!()
        }
    }

    fn parse_expr_primary(&mut self) -> ParseResult<'a, P<Expr>> {
        use TokenKind::*;

        let lo = self.token.span;

        if let Literal(lit) = self.token.kind {
            self.next();

            let expr = self.make_expr(ExprKind::Literal(lit), lo);

            Ok(expr)
        } else if self.check(&OpenDelim(Delimiter::Paren)) {
            self.parse_expr_tuple_parens()
        } else if self.eat_keyword(kw::If) {
            self.parse_expr_if()
        } else if self.eat_keyword(kw::For) {
            self.parse_expr_for()
        } else if self.eat_keyword(kw::While) {
            self.parse_expr_while(self.prev_token.span)
        } else if self.eat_keyword(kw::Match) {
            self.parse_expr_match()
        } else if self.eat_keyword(kw::Return) {
            self.parse_expr_return()
        } else if self.eat_keyword(kw::Throw) {
            self.parse_expr_throw()
        } else if self.eat_keyword(kw::Break) {
            self.parse_expr_break()
        } else if self.eat_keyword(kw::Continue) {
            self.parse_expr_continue()
        } else {
            unimplemented!()
        }
    }

    fn parse_expr_tuple_parens(&mut self) -> ParseResult<'a, P<Expr>> {
        use TokenKind::*;

        let lo = self.token.span;
        self.expect(&OpenDelim(Delimiter::Paren))?;

        let (exprs, _) = match self.parse_seq_to_end(
            &CloseDelim(Delimiter::Paren),
            SequenceSeparator::new(Comma),
            |p| p.parse_expr_item(),
        ) {
            Ok(v) => v,
            Err(err) => {
                let err = self.recover_from_seq_parse(Delimiter::Paren, lo, err);
                return Ok(err);
            }
        };

        let kind = if exprs.len() == 1 {
            if let ExprItem::Expr(e) = exprs.iter().next().unwrap() {
                ExprKind::Paren(e.clone())
            } else {
                ExprKind::Tuple(exprs)
            }
        } else {
            ExprKind::Tuple(exprs)
        };

        let expr = self.make_expr(kind, lo.to(self.prev_token.span));

        self.maybe_recover_from_bad_qpath(expr)
    }

    fn parse_expr_if(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.prev_token.span;
        let cond = self.parse_expr_cond(true)?;
        self.parse_expr_if_after_cond(cond, lo)
    }

    fn parse_expr_cond(&mut self, if_expr: bool) -> ParseResult<'a, P<Expr>> {
        let mut res = Restrictions::empty();

        if if_expr {
            res |= Restrictions::THEN_IS_KEYWORD;
        }

        let mut cond = self.parse_expr_res(res)?;

        Ok(cond)
    }

    fn parse_expr_if_after_cond(&mut self, mut cond: P<Expr>, lo: Span) -> ParseResult<'a, P<Expr>> {
        let cond_span = cond.span;

        let mut recover_block_from_cond = |this: &mut Self| {
            let block = match &mut cond.kind {
                ExprKind::Infix(_, _, right) if matches!(&right.kind, ExprKind::Block(_)) => {
                    unimplemented!()
                }

                ExprKind::Block(_) => {
                    // let guar = this.diag_ctx().emit_error(unimplemented!());
                    let guar = unimplemented!();

                    std::mem::replace(&mut cond, this.make_expr_err(cond_span.shrink_to_hi(), guar))
                }

                _ => {
                    return None;
                }
            };

            if let ExprKind::Block(block) = &block.kind {
                Some(self.make_expr(
                    ExprKind::Block(*block),
                    block.span
                ))
            } else {
                unreachable!()
            }
        };

        let then_part = if self.token.is_keyword(kw::Else) {
            if let Some(block) = recover_block_from_cond(self) {
                block
            } else {
                let guar = unimplemented!();

                self.make_expr_err(cond_span.shrink_to_hi(), guar)
            }
        } else if self.token.is_keyword(kw::Then) {
            self.parse_expr_impl()?
        } else {
            // TODO parse attributes
            let block = if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
                self.parse_expr_block()?
            } else {
                if let Some(block) = recover_block_from_cond(self) {
                    block
                } else {
                    unimplemented!()
                }
            };

            block
        };

        let else_part = if self.eat_keyword(kw::Else) {
            Some(self.parse_expr_else_part(true)?)
        } else {
            None
        };

        Ok(self.make_expr(ExprKind::If(cond, then_part, else_part), lo.to(self.prev_token.span)))
    }

    fn parse_expr_for(&mut self) -> ParseResult<'a, P<Expr>> {
        let label = self.eat_symbol();

        let is_await = self.eat_keyword(kw::Await);

        let for_kind = if is_await { ForLoopKind::ForAwait } else { ForLoopKind::For };

        unimplemented!()
    }

    fn parse_expr_while(&mut self, lo: Span) -> ParseResult<'a, P<Expr>> {
        let label = self.eat_symbol();

        let cond = self.parse_expr_cond(false).map_err(|mut err| {
            err.span_label(lo, "while parseing the condition of this `while` expression");
            err
        })?;

        let body = self.parse_block().map_err(|mut err| {
            err.span_label(lo, "while parsing the body of this `while` expression");
            err
        })?;

        let else_part = if self.eat_keyword(kw::Else) {
            Some(self.parse_expr_else_part(false)?)
        } else {
            None
        };

        Ok(self.make_expr(
            ExprKind::While(cond, body, else_part, label),
            lo.to(self.prev_token.span)
        ))
    }

    fn parse_expr_else_part(&mut self, if_expr: bool) -> ParseResult<'a, P<Expr>> {
        let else_span = self.prev_token.span;

        let expr = if if_expr && self.eat_keyword(kw::If) {
            self.parse_expr_if()?
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_expr_block()?
        } else {
            unimplemented!()
        };

        Ok(expr)
    }

    fn parse_expr_match(&mut self) -> ParseResult<'a, P<Expr>> {
       unimplemented!()
    }

    fn parse_expr_opt(&mut self) -> ParseResult<'a, Option<P<Expr>>> {
        let expr = if self.token.can_begin_expr() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(expr)
    }

    fn parse_expr_return(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.prev_token.span;
        let kind = ExprKind::Return(self.parse_expr_opt()?);
        let expr = self.make_expr(kind, lo.to(self.prev_token.span));
        Ok(expr)
    }

    fn parse_expr_throw(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.prev_token.span;
        let kind = ExprKind::Throw(self.parse_expr_opt()?);
        let expr = self.make_expr(kind, lo.to(self.prev_token.span));
        Ok(expr)
    }

    fn parse_expr_break(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.prev_token.span;
        let label = self.eat_symbol();
        let kind = ExprKind::Break(self.parse_expr_opt()?, label);
        let expr = self.make_expr(kind, lo.to(self.prev_token.span));
        Ok(expr)
    }

    fn parse_expr_continue(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.prev_token.span;
        let label = self.eat_symbol();
        let kind = ExprKind::Continue(label);
        let expr = self.make_expr(kind, lo.to(self.prev_token.span));
        Ok(expr)
    }

    fn parse_expr_block(&mut self) -> ParseResult<'a, P<Expr>> {
        let block = self.parse_block()?;
        Ok(self.make_expr(ExprKind::Block(block), block.span))
    }
}

#[derive(Clone)]
pub enum ExprAtomKind {
    Primary(P<Expr>),

    MemberAccess(Symbol),
    Cast(P<Type>, CastType),
    DotAwait,

    Op(Symbol),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ExprAtomRole {
    Unspecified,
    Primary,
    Prefix,
    Suffix,
    Infix,
}

#[derive(Clone)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
    pub role: ExprAtomRole,
}

impl ExprAtom {
    pub fn new(kind: ExprAtomKind) -> Self {
        Self { kind, role: ExprAtomRole::Unspecified }
    }

    pub fn force(self, role: ExprAtomRole) -> Self {
        Self { kind: self.kind, role }
    }

    pub fn force_primary(self) -> Self {
        self.force(ExprAtomRole::Primary)
    }

    pub fn force_prefix(self) -> Self {
        self.force(ExprAtomRole::Prefix)
    }

    pub fn force_suffix(self) -> Self {
        self.force(ExprAtomRole::Suffix)
    }

    pub fn force_infix(self) -> Self {
        self.force(ExprAtomRole::Infix)
    }

    pub fn can_be_primary_expr(&self) -> bool {
        match self.role {
            ExprAtomRole::Unspecified => { },
            ExprAtomRole::Primary => return false,
            ExprAtomRole::Prefix => return false,
            ExprAtomRole::Suffix => return false,
            ExprAtomRole::Infix => return false,
        }

        match self.kind {
            ExprAtomKind::MemberAccess(_) => true,
            ExprAtomKind::Cast(..) | ExprAtomKind::DotAwait => false,
            _ => false
        }
    }

    pub fn can_be_suffix_op(&self) -> bool {
         match self.role {
            ExprAtomRole::Unspecified => { },
            ExprAtomRole::Primary => return false,
            ExprAtomRole::Prefix => return false,
            ExprAtomRole::Suffix => return false,
            ExprAtomRole::Infix => return false,
        }

        match self.kind {
            ExprAtomKind::MemberAccess(_) => true,
            ExprAtomKind::Cast(..) | ExprAtomKind::DotAwait => false,
            _ => false
        }
    }

    pub fn can_be_prefix_op(&self) -> bool {
         match self.role {
            ExprAtomRole::Unspecified => { },
            ExprAtomRole::Primary => return false,
            ExprAtomRole::Prefix => return false,
            ExprAtomRole::Suffix => return false,
            ExprAtomRole::Infix => return false,
        }

        match self.kind {
            ExprAtomKind::MemberAccess(_) => false,
            ExprAtomKind::Cast(..) | ExprAtomKind::DotAwait => true,
            _ => false
        }
    }

    pub fn can_be_infix_op(&self) -> bool {
         match self.role {
            ExprAtomRole::Unspecified => { },
            ExprAtomRole::Primary => return false,
            ExprAtomRole::Prefix => return false,
            ExprAtomRole::Suffix => return false,
            ExprAtomRole::Infix => return false,
        }

        match self.kind {
            ExprAtomKind::MemberAccess(_) => false,
            ExprAtomKind::Cast(..) | ExprAtomKind::DotAwait => false,
            _ => false
        }
    }
}