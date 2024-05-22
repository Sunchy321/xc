use std::iter::once;
use std::vec;

use thin_vec::{thin_vec, ThinVec};
use xc_ast::expr::{Arguments, CastType, Expr, ExprItem, ExprKind, ForLoopKind};
use xc_ast::ptr::P;
use xc_ast::stmt::Block;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::ty::Type;
use xc_error::ErrorGuaranteed;
use xc_span::symbol::kw;
use xc_span::{Span, Symbol};

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
        self.with_res(res, |this| this.parse_expr_impl())
    }

    fn parse_expr_item(&mut self) -> ParseResult<'a, ExprItem> {
        use TokenKind::*;

        if let DotDotDot = self.token.kind {
            self.next();
            let expr = self.parse_expr_impl()?;

            return Ok(ExprItem::ExpandExpr(expr));
        } else {
            let expr = self.parse_expr_impl()?;

            return Ok(ExprItem::Expr(expr));
        }
    }

    fn parse_expr_impl(&mut self) -> ParseResult<'a, P<Expr>> {
        use ExprAtom::*;

        // 1. collect primary exprs and operators
        // 2. convert . xxx into an operator
        // 3. convert as/is xxx into an operator
        let atoms = self.collect_atoms()?;

        // 4. process continuous primary exprs
        let atoms = once(None)
            .chain(atoms.iter().map(Some))
            .zip(atoms.iter())
            .map(|(p, a)| {
                // if not two adjacent primary expr, continue
                if !p.is_some_and(|p| p.can_be_primary_expr()) || !a.can_be_primary_expr() {
                    return Ok(*a);
                }

                match &a {
                    Primary(e) => match &e.kind {
                        ExprKind::Paren(e) => Ok(FuncCall(
                            Arguments::from_expr_list(thin_vec![e.clone()]),
                            thin_vec![],
                        )),
                        ExprKind::Array(a) => Ok(Subscript(Arguments::from_expr_items(a.clone()))),
                        ExprKind::Tuple(t) => {
                            Ok(FuncCall(Arguments::from_expr_items(t.clone()), thin_vec![]))
                        }
                        ExprKind::Block(b) => Ok(FuncCallTrailing(b.clone())),
                        ExprKind::AnonEnumerator(symbol) => {
                            Ok(MemberAccess(*symbol))
                        }

                        _ => Err(self
                            .diag_ctx()
                            .create_error("unexpected primary expr after another primary expr")),
                    },

                    _ => unreachable!(),
                }
            })
            .collect::<Result::<Vec::<_>, _>>()?;

        // combine func call and trailing blocks.
        let atoms = atoms.into_iter().fold(vec![], |mut v, a| {
            match a {
                ExprAtom::FuncCallTrailing(block) => {
                    if let Some(ExprAtom::FuncCall(args, mut blocks)) = v.pop() {
                        blocks.push(block);
                        v.push(ExprAtom::FuncCall(args, blocks));
                    } else {
                        v.push(ExprAtom::FuncCall(Arguments::new(), thin_vec![block]));
                    }
                }

                _ => v.push(a)
            }

            v
        });

        enum GroupedExprAtom {
            Primary(ExprAtom),
            Ops(Vec<ExprAtom>),
        }

        // 5. detect infix operator
        let mut grouped_atoms = atoms.into_iter().fold(vec![], |mut v, a| {
            use ExprAtom::*;

            match a {
                Primary(e) => v.push(GroupedExprAtom::Primary(a)),

                _ => {
                    if let Some(last_group) = v.last_mut() {
                        match last_group {
                            GroupedExprAtom::Primary(_) => {
                                v.push(GroupedExprAtom::Ops(vec![a]));
                            }

                            GroupedExprAtom::Ops(ops) => {
                                ops.push(a);
                            }
                        }
                    } else {
                        v.push(GroupedExprAtom::Ops(vec![a]));
                    }
                }
            }

            v
        });

        for (i, g) in grouped_atoms.iter_mut().enumerate() {
            match g {
                GroupedExprAtom::Primary(..) => { },
                GroupedExprAtom::Ops(mut ops) => {
                    // first operators must be prefix
                    if i == 0 {
                        if ops.iter().any(|a| !a.can_be_prefix_op()) {
                            return Err(self.diag_ctx().create_error("non-prefix operator occurs at start of expression"));
                        }

                        ops = ops.iter().map(|a| a.force_prefix()).collect();
                        continue;
                    }

                    // last operators must be suffix
                    if i == grouped_atoms.len() - 1 {
                        if ops.iter().any(|a| !a.can_be_suffix_op()) {
                            return Err(self.diag_ctx().create_error("non-suffix operator occurs at end of expression"));
                        }

                        ops = ops.iter().map(|a| a.force_suffix()).collect();
                        continue;
                    }

                    let suffix_max_len = ops.iter().take_while(|a| a.can_be_suffix_op()).count();
                    let prefix_max_len = ops.iter().rev().take_while(|a| a.can_be_prefix_op()).count();

                    if suffix_max_len + prefix_max_len + 1 < ops.len() {
                        return Err(self.diag_ctx().create_error("cannot determine infix operator"));
                    }

                    let mut possible_infix = vec![];

                    for i in suffix_max_len..ops.len() - prefix_max_len {
                        if ops[0..i].into_iter().all(|a| a.can_be_prefix_op()) && ops[i+1..].into_iter().all(|a| a.can_be_suffix_op()) {
                            possible_infix.push(i);
                        }
                    }

                    if possible_infix.len() == 0 {
                        return Err(self.diag_ctx().create_error("cannot determine infix operator"));
                    } else if possible_infix.len() > 1 {
                        return Err(self.diag_ctx().create_error("ambiguous infix operator"));
                    }

                    ops = (0..ops.len()).into_iter().map(|i| {
                        if i == possible_infix[0] {
                            ops[i].force_infix()
                        } else if i < possible_infix[0] {
                            ops[i].force_prefix()
                        } else {
                            ops[i].force_suffix()
                        }
                    }).collect();
                }
            }
        }

        let atoms = grouped_atoms.into_iter().flat_map(|g| {
            match g {
                GroupedExprAtom::Primary(a) => vec![a],
                GroupedExprAtom::Ops(ops) => ops,
            }
        }).collect::<Vec<_>>();

        // 6. collect suffix and prefix operators
        enum CollectedExprAtom {
            Operand(P<Expr>),
        }

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
            At | Pound | Semicolon | Colon | Comma | RightArrow | FatArrow | DotDotDot => {
                return Ok(None)
            }
            Dollar | SymbolOpen | ColonColon => ExprAtom::Primary(self.parse_expr_primary()?),

            OpenDelim(delim) => match delim {
                Delimiter::Paren | Delimiter::Bracket | Delimiter::Brace | Delimiter::DictBound => {
                    ExprAtom::Primary(self.parse_expr_primary()?)
                }
                Delimiter::Invisible => return Ok(None),
            },

            CloseDelim(_) => return Ok(None),

            Literal(_) => ExprAtom::Primary(self.parse_expr_primary()?),

            Identifier(_) => ExprAtom::Primary(self.parse_expr_primary()?),
            LambdaArgNamed(_) => ExprAtom::Primary(self.parse_expr_primary()?),
            LambdaArgUnnamed(_) => ExprAtom::Primary(self.parse_expr_primary()?),

            Eof => return Ok(None),

            Op(op) => {
                self.next();
                ExprAtom::Op(op, OpRole::Unspecified)
            }
        };

        Ok(Some(atom))
    }

    fn parse_dot_part(&mut self) -> ParseResult<'a, ExprAtom> {
        let lo = self.prev_token.span;

        match self.token.kind {
            TokenKind::Identifier(sym) if sym == kw::Await => Ok(ExprAtom::DotAwait),

            _ => unimplemented!(),
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

    fn parse_expr_if_after_cond(
        &mut self,
        mut cond: P<Expr>,
        lo: Span,
    ) -> ParseResult<'a, P<Expr>> {
        let cond_span = cond.span;

        let mut recover_block_from_cond = |this: &mut Self| {
            let block = match &mut cond.kind {
                ExprKind::Infix(_, _, right) if matches!(&right.kind, ExprKind::Block(_)) => {
                    unimplemented!()
                }

                ExprKind::Block(_) => {
                    // let guar = this.diag_ctx().emit_error(unimplemented!());
                    let guar = unimplemented!();

                    std::mem::replace(
                        &mut cond,
                        this.make_expr_err(cond_span.shrink_to_hi(), guar),
                    )
                }

                _ => {
                    return None;
                }
            };

            if let ExprKind::Block(block) = &block.kind {
                Some(this.make_expr(ExprKind::Block(*block), block.span))
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

        Ok(self.make_expr(
            ExprKind::If(cond, then_part, else_part),
            lo.to(self.prev_token.span),
        ))
    }

    fn parse_expr_for(&mut self) -> ParseResult<'a, P<Expr>> {
        let label = self.eat_symbol();

        let is_await = self.eat_keyword(kw::Await);

        let for_kind = if is_await {
            ForLoopKind::ForAwait
        } else {
            ForLoopKind::For
        };

        unimplemented!()
    }

    fn parse_expr_while(&mut self, lo: Span) -> ParseResult<'a, P<Expr>> {
        let label = self.eat_symbol();

        let cond = self.parse_expr_cond(false).map_err(|mut err| {
            err.span_label(
                lo,
                "while parseing the condition of this `while` expression",
            );
            err
        })?;

        let body = self.parse_expr_block().map_err(|mut err| {
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
            lo.to(self.prev_token.span),
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
pub enum ExprAtom {
    Primary(P<Expr>),

    Subscript(Arguments),
    FuncCall(Arguments, ThinVec<P<Block>>),
    FuncCallTrailing(P<Block>),
    MemberAccess(Symbol),
    Cast(P<Type>, CastType),
    DotAwait,

    Op(Symbol, OpRole),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum OpRole {
    Unspecified,
    Prefix,
    Suffix,
    Infix,
}

impl ExprAtom {
    fn force(self, role: OpRole) -> Self {
        match self {
            ExprAtom::Op(sym, _) => ExprAtom::Op(sym, role),
            _ => self,
        }
    }

    pub fn force_prefix(self) -> Self {
        self.force(OpRole::Prefix)
    }

    pub fn force_suffix(self) -> Self {
        self.force(OpRole::Suffix)
    }

    pub fn force_infix(self) -> Self {
        self.force(OpRole::Infix)
    }

    pub fn can_be_primary_expr(&self) -> bool {
        use ExprAtom::*;

        match self {
            Primary(..) => true,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(..) => false,
        }
    }

    pub fn can_be_suffix_op(&self) -> bool {
        use ExprAtom::*;

        match self {
            Primary(..) => false,

            FuncCall(..) => true,
            FuncCallTrailing(..) => false,
            Subscript(..) => true,
            MemberAccess(..) => true,
            Cast(..) => true,
            DotAwait => true,

            Op(_, role) => match role {
                OpRole::Unspecified => true,
                OpRole::Prefix => false,
                OpRole::Suffix => true,
                OpRole::Infix => false,
            },
        }
    }

    pub fn can_be_prefix_op(&self) -> bool {
        use ExprAtom::*;

        match self {
            Primary(..) => false,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(_, role) => match role {
                OpRole::Unspecified => true,
                OpRole::Prefix => true,
                OpRole::Suffix => false,
                OpRole::Infix => false,
            },
        }
    }

    pub fn can_be_infix_op(&self) -> bool {
        use ExprAtom::*;

        match self {
            Primary(..) => false,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(_, role) => match role {
                OpRole::Unspecified => true,
                OpRole::Prefix => false,
                OpRole::Suffix => true,
                OpRole::Infix => false,
            },
        }
    }
}
