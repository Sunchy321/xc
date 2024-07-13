use std::cmp::Ordering;
use std::sync::Arc;
use std::vec;

use itertools::Itertools;
use thin_vec::{thin_vec, ThinVec};
use xc_ast::expr::{
    self, Arguments, CastType, DictItem, Expr, ExprItem, ExprKind, ForLoopKind, StructItem,
};
use xc_ast::literal::{Literal, LiteralKind};
use xc_ast::path::PathStyle;
use xc_ast::ptr::P;
use xc_ast::stmt::Block;
use xc_ast::token::{Delimiter, IdentIsRaw, TokenKind};
use xc_ast::tokenstream::Spacing;
use xc_ast::ty::Type;
use xc_error::ErrorGuaranteed;
use xc_span::symbol::{kw, op};
use xc_span::{Span, Symbol};

use crate::parser::op::{Associativity, OpInfo};
use crate::parser::{HasTrailing, SequenceSeparator};

use super::op::OpContext;
use super::parser::Parser;
use super::{ParseResult, Restrictions};

enum ExprAtomGroup {
    Expr(P<Expr>),
    PrefixOp(Vec<ExprAtom>),
    SuffixOp(Vec<ExprAtom>),
    InfixOp(Symbol, Span),
}

impl<'a> Parser<'a> {
    pub(self) fn op_ctx(&self) -> &OpContext {
        &self.session.op_ctx
    }

    fn token_can_begin_expr(&self) -> bool {
        use TokenKind::*;

        if !self.token.can_begin_expr() {
            false
        } else {
            match self.token.kind {
                Identifier(sym, IdentIsRaw::No) => {
                    if sym == kw::Then && self.restrictions.contains(Restrictions::THEN_IS_KEYWORD)
                    {
                        return false;
                    }
                }

                _ => {}
            }

            true
        }
    }

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

            return Ok(ExprItem::Expansion(expr));
        } else {
            let expr = self.parse_expr_impl()?;

            return Ok(ExprItem::Expr(expr));
        }
    }

    fn parse_expr_impl(&mut self) -> ParseResult<'a, P<Expr>> {
        // 1. collect primary exprs and operators
        // 2. convert . xxx into an operator
        // 3. convert as/is xxx into an operator
        let atoms = self.collect_expr_atoms()?;

        if atoms.is_empty() {
            return Err(self.diag_ctx().create_error("unexpected token"));
        }

        // 4. process continuous primary exprs
        let atoms = self.process_expr_cont_primary(atoms)?;

        // 5. detect infix operator
        let grouped_atoms = self.process_expr_cont_operators(atoms)?;

        // 6. collect suffix and prefix operators
        let grouped_atoms = self.process_expr_collect_op(grouped_atoms);

        // 7. process precedence
        let expr = self.process_expr_merge_infix(grouped_atoms)?;

        Ok(expr)
    }

    /// convert continuous primary exprs except first into func call, subscript, member access, etc.
    /// if failed, report an error.
    fn process_expr_cont_primary(&self, atoms: Vec<ExprAtom>) -> ParseResult<'a, Vec<ExprAtom>> {
        use ExprAtomKind::*;

        // E E E -> E S S, after that: no continuous primary exprs
        let atoms = atoms
            .iter()
            .enumerate()
            .map(|(i, a)| (if i == 0 { None } else { atoms.get(i - 1) }, a))
            .map(|(p, a)| {
                // if not two adjacent primary expr, continue
                if !p.is_some_and(|p| p.can_be_primary_expr(self.op_ctx()))
                    || !a.can_be_primary_expr(self.op_ctx())
                {
                    return Ok(a.clone());
                }

                let kind = match &a.kind {
                    Primary(e) => match &e.kind {
                        ExprKind::Paren(e) => {
                            FuncCall(Arguments::from_expr_list(thin_vec![e.clone()]), thin_vec![])
                        }
                        ExprKind::Array(a) => Subscript(Arguments::from_expr_items(a.clone())),
                        ExprKind::Tuple(t) => {
                            FuncCall(Arguments::from_expr_items(t.clone()), thin_vec![])
                        }
                        ExprKind::Block(b) => FuncCallTrailing(b.clone()),
                        ExprKind::AnonEnumerator(symbol) => MemberAccess(*symbol),

                        _ => {
                            return Err(self.diag_ctx().create_error(
                                "unexpected primary expr after another primary expr",
                            ))
                        }
                    },

                    _ => unreachable!(),
                };

                Ok(self.make_expr_atom(kind, a.span))
            })
            .collect::<Result<Vec<_>, _>>()?;

        // combine func call and trailing blocks.
        // after that: no FuncCallTrailing
        let atoms = atoms.into_iter().fold(vec![], |mut v: Vec<ExprAtom>, a| {
            match a.kind {
                FuncCallTrailing(block) => {
                    if let Some(l) = v.last_mut()
                        && let FuncCall(_, ref mut blocks) = &mut l.kind
                    {
                        blocks.push(block);
                        l.span = l.span.to(a.span);
                    } else {
                        let span = block.span;
                        let kind = FuncCall(Arguments::new(), thin_vec![block]);
                        v.push(self.make_expr_atom(kind, span))
                    }
                }

                _ => v.push(a),
            }

            v
        });

        Ok(atoms)
    }

    /// convert continuous operators into suffix, prefix and infix operators.
    /// collect atoms and add pesudo operators at start and end.
    fn process_expr_cont_operators(
        &self,
        atoms: Vec<ExprAtom>,
    ) -> ParseResult<'a, Vec<ExprAtomGroup>> {
        use ExprAtomKind::*;

        enum ExprAtomGroupInner {
            Primary(P<Expr>),
            Ops(Vec<ExprAtom>),
        }

        // collect operators.
        // after that: E [O] E [O] E ...
        let grouped_atoms = atoms.into_iter().fold(vec![], |mut v, a| {
            match a.kind {
                Primary(e) => v.push(ExprAtomGroupInner::Primary(e)),

                _ => {
                    if let Some(last_group) = v.last_mut() {
                        match last_group {
                            ExprAtomGroupInner::Primary(_) => {
                                v.push(ExprAtomGroupInner::Ops(vec![a]));
                            }

                            ExprAtomGroupInner::Ops(ops) => {
                                ops.push(a);
                            }
                        }
                    } else {
                        v.push(ExprAtomGroupInner::Ops(vec![a]));
                    }
                }
            }

            v
        });

        let grouped_atoms_len = grouped_atoms.len();

        // [O] -> P I S, after that: (P? E S) I (P E S) I ... I (P E S?)
        let mut grouped_atoms =
            grouped_atoms
                .into_iter()
                .enumerate()
                .try_fold(vec![], |mut v, (i, g)| {
                    let a = match g {
                        ExprAtomGroupInner::Primary(e) => vec![ExprAtomGroup::Expr(e)],
                        ExprAtomGroupInner::Ops(mut ops) => {
                            if i == 0 {
                                // first operators must be prefix
                                if ops.iter().any(|a| !a.can_be_prefix_op(self.op_ctx())) {
                                    return Err(self.diag_ctx().create_error(
                                        "non-prefix operator occurs at start of expression",
                                    ));
                                }

                                vec![ExprAtomGroup::PrefixOp(ops)]
                            } else if i == grouped_atoms_len - 1 {
                                // last operators must be suffix
                                if ops.iter().any(|a| !a.can_be_suffix_op(self.op_ctx())) {
                                    return Err(self.diag_ctx().create_error(
                                        "non-suffix operator occurs at end of expression",
                                    ));
                                }

                                vec![ExprAtomGroup::SuffixOp(ops)]
                            } else {
                                let len = ops.len();

                                let suffix_max_len = ops
                                    .iter()
                                    .take_while(|a| a.can_be_suffix_op(self.op_ctx()))
                                    .count();
                                let prefix_max_len = ops
                                    .iter()
                                    .rev()
                                    .take_while(|a| a.can_be_prefix_op(self.op_ctx()))
                                    .count();

                                if suffix_max_len + prefix_max_len + 1 < ops.len() {
                                    return Err(self
                                        .diag_ctx()
                                        .create_error("cannot determine infix operator"));
                                }

                                let mut possible_infix = vec![];

                                let infix_lo = if prefix_max_len == len {
                                    0
                                } else {
                                    len - prefix_max_len - 1
                                };
                                let infix_hi = if suffix_max_len == len {
                                    len
                                } else {
                                    suffix_max_len + 1
                                };

                                for i in infix_lo..infix_hi {
                                    if ops[0..i]
                                        .into_iter()
                                        .all(|a| a.can_be_prefix_op(self.op_ctx()))
                                        && ops[i + 1..]
                                            .into_iter()
                                            .all(|a| a.can_be_suffix_op(self.op_ctx()))
                                    {
                                        possible_infix.push(i);
                                    }
                                }

                                if possible_infix.len() == 0 {
                                    return Err(self
                                        .diag_ctx()
                                        .create_error("cannot determine infix operator"));
                                } else if possible_infix.len() > 1 {
                                    return Err(self
                                        .diag_ctx()
                                        .create_error("ambiguous infix operator"));
                                }

                                let suffixes = ops.drain(..possible_infix[0]).collect();
                                let prefixes = ops.drain(possible_infix[0] + 1..).collect();

                                let infix = match ops.pop().unwrap() {
                                    ExprAtom {
                                        kind: Op(sym, _),
                                        span,
                                        ..
                                    } => (sym, span),
                                    _ => unreachable!(),
                                };

                                vec![
                                    ExprAtomGroup::SuffixOp(suffixes),
                                    ExprAtomGroup::InfixOp(infix.0, infix.1),
                                    ExprAtomGroup::PrefixOp(prefixes),
                                ]
                            }
                        }
                    };

                    v.extend(a);

                    Ok(v)
                })?;

        // add pseudo prefix and suffix operators.
        // after that: (P E S) I (P E S) I ... I (P E S)
        if !matches!(grouped_atoms.first(), Some(ExprAtomGroup::PrefixOp(..))) {
            grouped_atoms.insert(0, ExprAtomGroup::PrefixOp(vec![]));
        }

        if !matches!(grouped_atoms.last(), Some(ExprAtomGroup::SuffixOp(..))) {
            grouped_atoms.push(ExprAtomGroup::SuffixOp(vec![]));
        }

        Ok(grouped_atoms)
    }

    /// collect operators and primary exprs.
    /// convert (prefixes primary suffixes) into a single expr.
    fn process_expr_collect_op(&self, grouped_atoms: Vec<ExprAtomGroup>) -> Vec<ExprAtomGroup> {
        // P E S -> E, after that: E I E I ... I E
        let grouped_atoms = grouped_atoms
            .into_iter()
            .fold((vec![], None, None), |(mut v, mut p, mut e), g| {
                match g {
                    ExprAtomGroup::InfixOp(..) => {
                        v.push(g);
                    }

                    ExprAtomGroup::PrefixOp(prefixes) => {
                        p = Some(prefixes);
                    }

                    ExprAtomGroup::Expr(expr) => {
                        e = Some(expr);
                    }

                    ExprAtomGroup::SuffixOp(suffixes) => {
                        let expr = e.take().unwrap();
                        let prefixes = p.take().unwrap();

                        let suffix_expr = self.make_suffix_expr(expr, suffixes);

                        let full_expr = self.make_prefix_expr(suffix_expr, prefixes);

                        v.push(ExprAtomGroup::Expr(full_expr));
                    }
                }

                (v, p, e)
            })
            .0;

        grouped_atoms
    }

    fn process_expr_merge_infix(
        &self,
        mut grouped_atoms: Vec<ExprAtomGroup>,
    ) -> ParseResult<'a, P<Expr>> {
        if grouped_atoms.len() == 1 {
            match grouped_atoms.pop().unwrap() {
                ExprAtomGroup::Expr(expr) => return Ok(expr),
                _ => unreachable!(),
            }
        }

        let infos = grouped_atoms
            .iter()
            .filter_map(|g| match g {
                ExprAtomGroup::InfixOp(sym, _) => self.op_ctx().get_info(*sym),
                _ => None,
            })
            .collect::<Vec<_>>();

        // detect operators with lowest (with higher value) precedence.
        // note because of operators without precedence, there may be multiple operators with lowest precedence.
        // == -> + -> *
        //    -> `some operators without precedence`
        let main_info = infos.into_iter().fold(vec![], |maxes: Vec<OpInfo>, p| {
            let mut less_than_some = false;
            let mut equal_with_some = false;

            let mut maxes = maxes
                .into_iter()
                .filter(
                    |v| match v.precedence.partial_cmp(&p.precedence, self.op_ctx()) {
                        None => true,
                        Some(Ordering::Less) => {
                            less_than_some = true;
                            true
                        }
                        Some(Ordering::Equal) => {
                            equal_with_some = true;
                            true
                        }
                        Some(Ordering::Greater) => false,
                    },
                )
                .collect_vec();

            if !less_than_some && !equal_with_some {
                maxes.push(p);
            }

            maxes
        });

        if main_info.len() > 1 {
            return Err(self
                .diag_ctx()
                .create_error("cannot mix operators without precedence with others"));
        }

        let main_info = main_info.into_iter().next().unwrap();

        enum ExprAtomGroupInner {
            Operator(Symbol, Span),
            Operand(Vec<ExprAtomGroup>),
        }

        let (operators, operands): (Vec<_>, Vec<_>) = grouped_atoms
            .into_iter()
            .fold(vec![], |mut v, g| {
                match &g {
                    ExprAtomGroup::Expr(..) => match v.last_mut() {
                        Some(ExprAtomGroupInner::Operand(ref mut operands)) => {
                            operands.push(g);
                        }

                        _ => v.push(ExprAtomGroupInner::Operand(vec![g])),
                    },

                    ExprAtomGroup::InfixOp(sym, span) => {
                        let precedence = self.op_ctx().precedence(*sym).unwrap();

                        if precedence == main_info.precedence {
                            v.push(ExprAtomGroupInner::Operator(sym.clone(), span.clone()));
                            v.push(ExprAtomGroupInner::Operand(vec![]));
                        } else {
                            match v.last_mut() {
                                Some(ExprAtomGroupInner::Operand(ref mut operands)) => {
                                    operands.push(g);
                                }

                                _ => v.push(ExprAtomGroupInner::Operand(vec![g])),
                            };
                        }
                    }

                    _ => unreachable!(),
                }

                v
            })
            .into_iter()
            .partition(|g| matches!(g, ExprAtomGroupInner::Operator(..)));

        let operators = operators
            .into_iter()
            .map(|g| match g {
                ExprAtomGroupInner::Operator(sym, span) => (sym, span),
                _ => unreachable!(),
            })
            .collect_vec();

        let operands = operands
            .into_iter()
            .map(|g| match g {
                ExprAtomGroupInner::Operand(operands) => operands,
                _ => unreachable!(),
            })
            .collect_vec();

        let mut is_comparison_chain = false;

        // check associativity.
        match main_info.assoc {
            Associativity::None => {
                if operators.len() > 1 {
                    return Err(self.diag_ctx().create_error("non-associative operators"));
                }
            }

            Associativity::Isolate => {
                if operators.iter().any(|o| o.0 != operators[0].0) {
                    return Err(self.diag_ctx().create_error("isolated operators"));
                }
            }

            Associativity::Comparison => {
                enum ComparisonStyle {
                    Individual(Symbol),

                    LessChain,
                    GreaterChain,
                    UnknownChain,
                }

                let mut style = None;

                for (op, _) in operators.iter() {
                    if *op == op::Equal {
                        match style {
                            None => style = Some(ComparisonStyle::UnknownChain),
                            Some(
                                ComparisonStyle::LessChain
                                | ComparisonStyle::GreaterChain
                                | ComparisonStyle::UnknownChain,
                            ) => {}
                            Some(ComparisonStyle::Individual(..)) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                        }
                    }
                    if *op == op::Less || *op == op::LessEqual {
                        match style {
                            None | Some(ComparisonStyle::UnknownChain) => {
                                style = Some(ComparisonStyle::LessChain)
                            }
                            Some(ComparisonStyle::LessChain) => {}
                            Some(ComparisonStyle::GreaterChain) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                            Some(ComparisonStyle::Individual(..)) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                        }
                    } else if *op == op::Greater || *op == op::GreaterEqual {
                        match style {
                            None | Some(ComparisonStyle::UnknownChain) => {
                                style = Some(ComparisonStyle::GreaterChain)
                            }
                            Some(ComparisonStyle::GreaterChain) => {}
                            Some(ComparisonStyle::LessChain) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                            Some(ComparisonStyle::Individual(..)) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                        }
                    } else {
                        match style {
                            None => style = Some(ComparisonStyle::Individual(*op)),
                            Some(
                                ComparisonStyle::LessChain
                                | ComparisonStyle::GreaterChain
                                | ComparisonStyle::UnknownChain,
                            ) => {
                                return Err(self
                                    .diag_ctx()
                                    .create_error("invalid comparison operator"))
                            }
                            Some(ComparisonStyle::Individual(sym)) => {
                                if sym != *op {
                                    return Err(self
                                        .diag_ctx()
                                        .create_error("invalid comparison operator"));
                                }
                            }
                        }
                    }
                }

                if matches!(
                    style,
                    Some(
                        ComparisonStyle::UnknownChain
                            | ComparisonStyle::LessChain
                            | ComparisonStyle::GreaterChain
                    )
                ) {
                    is_comparison_chain = true;
                }
            }

            Associativity::Left | Associativity::Right => {}
        }

        let operands = operands.into_iter().try_fold(vec![], |mut v, exprs| {
            let expr = self.process_expr_merge_infix(exprs)?;
            v.push(expr);
            Ok(v)
        })?;

        let expr = match (main_info.assoc, is_comparison_chain) {
            (Associativity::Comparison, false)
            | (Associativity::None | Associativity::Isolate, _) => {
                let op = operators.into_iter().next().unwrap();

                let lo = operands.first().unwrap().span;
                let hi = operands.last().unwrap().span;

                let span = lo.to(hi);

                let kind = ExprKind::Infix(op.0, operands.into());

                self.make_expr(kind, span)
            }

            (Associativity::Comparison, true) => {
                let lo = operands.first().unwrap().span;
                let hi = operands.last().unwrap().span;

                let span = lo.to(hi);

                let kind =
                    ExprKind::Comparison(operators.iter().map(|o| o.0).collect(), operands.into());

                self.make_expr(kind, span)
            }

            (Associativity::Left, _) => {
                assert!(operators.len() == operands.len() - 1);

                let mut iter = operands.into_iter();

                let mut expr = iter.next().unwrap();

                for ((op, _), e) in operators.into_iter().zip(iter) {
                    let span = expr.span.to(e.span);

                    let kind = ExprKind::Infix(op, thin_vec![expr, e]);

                    expr = self.make_expr(kind, span);
                }

                expr
            }

            (Associativity::Right, _) => {
                assert!(operators.len() == operands.len() - 1);

                let mut iter = operands.into_iter().rev();

                let mut expr = iter.next().unwrap();

                for ((op, _), e) in operators.into_iter().rev().zip(iter) {
                    let span = e.span.to(expr.span);

                    let kind = ExprKind::Infix(op, thin_vec![e, expr]);

                    expr = self.make_expr(kind, span);
                }

                expr
            }
        };

        Ok(expr)
    }

    fn make_suffix_expr(&self, mut expr: P<Expr>, suffixes: Vec<ExprAtom>) -> P<Expr> {
        use ExprAtomKind::*;

        for suffix in suffixes {
            let span = expr.span.to(suffix.span);

            match suffix.kind {
                Primary(..) => unreachable!(),

                Subscript(args) => {
                    expr = self.make_expr(ExprKind::Subscript(expr, args), span);
                }

                FuncCall(args, blocks) => {
                    expr = self.make_expr(ExprKind::FuncCall(expr, args, blocks), span);
                }

                FuncCallTrailing(..) => unreachable!(),

                MemberAccess(symbol) => {
                    expr = self.make_expr(ExprKind::MemberAccess(expr, symbol), span);
                }

                MemberDynamicAccess(e) => {
                    expr = self.make_expr(ExprKind::MemberDynamicAccess(expr, e), span);
                }

                Cast(ty, cast_type) => {
                    expr = self.make_expr(ExprKind::Cast(expr, ty, cast_type), span);
                }

                DotAwait => {
                    expr = self.make_expr(ExprKind::Await(expr), span);
                }

                Op(op, _) => {
                    expr = self.make_expr(ExprKind::Suffix(op, expr), span);
                }
            }
        }

        expr
    }

    fn make_prefix_expr(&self, mut expr: P<Expr>, prefixes: Vec<ExprAtom>) -> P<Expr> {
        use ExprAtomKind::*;

        for prefix in prefixes {
            let span = prefix.span.to(expr.span);

            match prefix.kind {
                Primary(..) => unreachable!(),
                Subscript(..) => unreachable!(),
                FuncCall(..) => unreachable!(),
                FuncCallTrailing(..) => unreachable!(),
                MemberAccess(..) => unreachable!(),
                MemberDynamicAccess(..) => unreachable!(),
                Cast(..) => unreachable!(),
                DotAwait => unreachable!(),

                Op(op, _) => {
                    expr = self.make_expr(ExprKind::Prefix(op, expr), span);
                }
            }
        }

        expr
    }

    fn collect_expr_atoms(&mut self) -> ParseResult<'a, Vec<ExprAtom>> {
        let mut atoms = Vec::new();

        while let Some(atom) = self.collect_expr_atom()? {
            atoms.push(atom);
        }

        Ok(atoms)
    }

    fn collect_expr_atom(&mut self) -> ParseResult<'a, Option<ExprAtom>> {
        use TokenKind::*;

        let atom = if !self.token_can_begin_expr() {
            return Ok(None);
        } else {
            match self.token.kind {
                OpenDelim(Delimiter::Invisible) => return Ok(None),

                Op(op) => {
                    if !self.op_ctx().is_op(op) {
                        return Err(self.diag_ctx().create_error("unknown operator"));
                    }

                    self.next();

                    macro_rules! combine_op {
                        ($first_id:ident == $first:path, $second_id:ident == $second:path, $result:path) => {
                            if $first_id == $first
                                && $second_id == $second
                                && matches!(self.spacing, Spacing::Joint)
                            {
                                let span = self.prev_token.span.to(self.token.span);
                                self.next();
                                return Ok(Some(self.make_expr_atom(
                                    ExprAtomKind::Op($result, OpRole::Infix),
                                    span,
                                )));
                            }
                        };
                    }

                    // parse !in and !is.
                    match (self.prev_token.kind.clone(), self.token.kind.clone()) {
                        (Op(first), Identifier(second, IdentIsRaw::No)) => {
                            combine_op!(first == op::Exclamation, second == kw::In, op::NotIn);
                            combine_op!(first == op::Exclamation, second == kw::Is, op::NotIs);
                        }

                        (Identifier(first, IdentIsRaw::No), Op(second)) => {
                            combine_op!(first == kw::As, second == op::Question, op::AsQuestion);
                            combine_op!(
                                first == kw::As,
                                second == op::Exclamation,
                                op::AsExclamation
                            );
                        }

                        _ => {}
                    }

                    return Ok(Some(self.make_expr_atom(
                        ExprAtomKind::Op(op, OpRole::Unspecified),
                        self.prev_token.span,
                    )));
                }

                _ => {}
            }

            self.make_expr_atom_primary()?
        };

        Ok(Some(atom))
    }

    fn make_expr_atom(&self, kind: ExprAtomKind, span: Span) -> ExprAtom {
        ExprAtom::new(kind, span)
    }

    fn make_expr_atom_primary(&mut self) -> ParseResult<'a, ExprAtom> {
        Ok(ExprAtom::new_primary(self.parse_expr_primary()?))
    }

    fn parse_dot_part(&mut self) -> ParseResult<'a, ExprAtom> {
        let lo = self.prev_token.span;

        let kind = if self.eat_keyword(kw::Await) {
            ExprAtomKind::DotAwait
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Paren)) {
            let expr = self.parse_expr_tuple_parens()?;
            ExprAtomKind::MemberDynamicAccess(expr)
        } else if let Some((ident, ..)) = self.token.to_identifier() && !self.token.is_reserved_ident() {
            self.next();
            ExprAtomKind::MemberAccess(ident.name)
        } else if let TokenKind::Literal(lit) = self.token.kind && lit.kind == LiteralKind::Integer {
            self.next();
            ExprAtomKind::MemberAccess(lit.value)
        } else {
            todo!()
        };

        let span = lo.to(self.prev_token.span);

        Ok(self.make_expr_atom(kind, span))
    }

    fn parse_expr_primary(&mut self) -> ParseResult<'a, P<Expr>> {
        use TokenKind::*;

        let lo = self.token.span;

        if let Literal(lit) = self.token.kind {
            self.next();

            let expr = self.make_expr(ExprKind::Literal(lit), lo);

            Ok(expr)
        } else if let LambdaArgNamed(sym) = self.token.kind {
            self.next();

            let expr = self.make_expr(ExprKind::LambdaArgNamed(sym), lo);

            Ok(expr)
        } else if let LambdaArgUnnamed(index) = self.token.kind {
            self.next();

            let expr = self.make_expr(ExprKind::LambdaArgUnnamed(index), lo);

            Ok(expr)
        } else if self.check_path() {
            self.parse_expr_path()
        } else if self.check(&OpenDelim(Delimiter::Paren)) {
            self.parse_expr_tuple_parens()
        } else if self.check(&OpenDelim(Delimiter::Brace)) {
            self.parse_expr_block_struct()
        } else if self.check(&OpenDelim(Delimiter::Bracket)) {
            self.parse_expr_array_dict()
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

    fn parse_expr_path(&mut self) -> ParseResult<'a, P<Expr>> {
        let path = self.parse_path(PathStyle::Expr)?;

        let (span, kind) = (path.span, ExprKind::Path(path));

        let expr = self.make_expr(kind, span);

        self.maybe_recover_from_bad_qpath(expr)
    }

    fn parse_expr_tuple_parens(&mut self) -> ParseResult<'a, P<Expr>> {
        use TokenKind::*;

        let lo = self.token.span;

        self.expect(&OpenDelim(Delimiter::Paren))?;

        let (exprs, has_trailing) = match self.parse_seq_to_end(
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

        let kind = if exprs.len() == 1 && !matches!(has_trailing, HasTrailing::Yes) {
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

    fn parse_expr_block_struct(&mut self) -> ParseResult<'a, P<Expr>> {
        if self.look_ahead(1, |t| t.kind == TokenKind::DotDotDot) {
            self.parse_struct_literal()
        } else if self.look_ahead(1, |t| t.is_identifier())
            && self.look_ahead(2, |t| t.kind == TokenKind::Colon)
        {
            self.parse_struct_literal()
        } else {
            self.parse_expr_block()
        }
    }

    fn parse_struct_literal(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.token.span;

        let (items, _) =
            self.parse_delim_comma_seq(Delimiter::Brace, |this| this.parse_struct_item())?;
        let span = lo.to(self.prev_token.span);
        let expr = self.make_expr(ExprKind::Struct(items), span);

        Ok(expr)
    }

    fn parse_struct_item(&mut self) -> ParseResult<'a, StructItem> {
        if self.token.kind == TokenKind::DotDotDot {
            self.next();

            let expr = self.parse_expr()?;

            return Ok(StructItem::Expansion(expr));
        }

        if let Some((ident, is_raw)) = self.token.to_identifier() && self.look_ahead(1, |t| t.kind == TokenKind::Colon) {
            let key = ident.name;

            self.next();
            self.next();

            let expr = self.parse_expr()?;

            return Ok(StructItem::KeyValue(key, expr));
        }

        todo!()
    }

    fn parse_expr_array_dict(&mut self) -> ParseResult<'a, P<Expr>> {
        let lo = self.token.span;

        if self.look_ahead(1, |t| t.kind == TokenKind::CloseDelim(Delimiter::Bracket)) {
            self.next();
            self.next();

            let span = lo.to(self.prev_token.span);
            let expr = self.make_expr(ExprKind::Array(thin_vec![]), span);

            return Ok(expr);
        }

        if self.look_ahead(1, |t| t.kind == TokenKind::Colon)
            && self.look_ahead(2, |t| t.kind == TokenKind::CloseDelim(Delimiter::Bracket))
        {
            self.next();
            self.next();
            self.next();

            let span = lo.to(self.prev_token.span);
            let expr = self.make_expr(ExprKind::Dict(thin_vec![]), span);

            return Ok(expr);
        }

        let (items, _) =
            self.parse_delim_comma_seq(Delimiter::Bracket, |this| this.parse_array_dict_item())?;

        #[derive(PartialEq, Eq)]
        enum Kind {
            Unknown,
            Array,
            Dict,
            Conflict,
        }

        let mut kind = Kind::Unknown;

        for item in items.iter() {
            match item {
                ArrayDictItem::Item(..) => {
                    if kind == Kind::Unknown {
                        kind = Kind::Array;
                    } else if kind == Kind::Dict {
                        kind = Kind::Conflict;
                    }
                }

                ArrayDictItem::KeyValue(..) => {
                    if kind == Kind::Unknown {
                        kind = Kind::Dict;
                    } else if kind == Kind::Array {
                        kind = Kind::Conflict;
                    }
                }

                ArrayDictItem::Expansion(..) => { }
            }
        }

        match kind {
            Kind::Array => {
                let span = lo.to(self.prev_token.span);

                let items = items
                    .into_iter()
                    .map(|item| match item {
                        ArrayDictItem::Item(expr) => ExprItem::Expr(expr),
                        ArrayDictItem::KeyValue(_, _) => unreachable!(),
                        ArrayDictItem::Expansion(expr) => ExprItem::Expansion(expr),
                    })
                    .collect();

                let expr = self.make_expr(ExprKind::Array(items), span);

                Ok(expr)
            }

            Kind::Dict => {
                let span = lo.to(self.prev_token.span);

                let items = items
                    .into_iter()
                    .map(|item| match item {
                        ArrayDictItem::Item(_) => unreachable!(),
                        ArrayDictItem::KeyValue(key, value) => DictItem::KeyValue(key, value),
                        ArrayDictItem::Expansion(expr) => DictItem::Expansion(expr),
                    })
                    .collect();

                let expr = self.make_expr(ExprKind::Dict(items), span);

                Ok(expr)
            }

            Kind::Conflict => {
                todo!()
            }

            Kind::Unknown => {
                let span = lo.to(self.prev_token.span);

                let items = items
                    .into_iter()
                    .map(|item| match item {
                        ArrayDictItem::Item(_) => unreachable!(),
                        ArrayDictItem::KeyValue(_, _) => unreachable!(),
                        ArrayDictItem::Expansion(expr) => ExprItem::Expansion(expr),
                    })
                    .collect();

                let expr = self.make_expr(ExprKind::Array(items), span);

                Ok(expr)
            }
        }
    }

    fn parse_array_dict_item(&mut self) -> ParseResult<'a, ArrayDictItem> {
        if self.token.kind == TokenKind::DotDotDot {
            self.next();
            let expr = self.parse_expr()?;

            return Ok(ArrayDictItem::Expansion(expr));
        }

        let expr = self.parse_expr()?;

        if self.token.kind == TokenKind::Colon {
            self.next();
            let value = self.parse_expr()?;

            return Ok(ArrayDictItem::KeyValue(expr, value));
        }

        return Ok(ArrayDictItem::Item(expr));
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
                ExprKind::Infix(_, exprs)
                    if matches!(&exprs.last().unwrap().kind, ExprKind::Block(_)) =>
                {
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

        let mut with_then = false;

        let then_part = if self.token.is_keyword(kw::Else) {
            if let Some(block) = recover_block_from_cond(self) {
                block
            } else {
                let guar = unimplemented!();

                self.make_expr_err(cond_span.shrink_to_hi(), guar)
            }
        } else if self.token.is_keyword(kw::Then) {
            with_then = true;
            self.next();
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
            Some(self.parse_expr_else_part(if with_then {
                ElseKind::IfThen
            } else {
                ElseKind::If
            })?)
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
            Some(self.parse_expr_else_part(ElseKind::Loop)?)
        } else {
            None
        };

        Ok(self.make_expr(
            ExprKind::While(cond, body, else_part, label),
            lo.to(self.prev_token.span),
        ))
    }

    fn parse_expr_else_part(&mut self, else_kind: ElseKind) -> ParseResult<'a, P<Expr>> {
        let else_span = self.prev_token.span;

        let expr =
            if matches!(else_kind, ElseKind::If | ElseKind::IfThen) && self.eat_keyword(kw::If) {
                self.parse_expr_if()?
            } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
                self.parse_expr_block()?
            } else if matches!(else_kind, ElseKind::IfThen) {
                self.parse_expr()?
            } else {
                unimplemented!()
            };

        Ok(expr)
    }

    fn parse_expr_match(&mut self) -> ParseResult<'a, P<Expr>> {
        unimplemented!()
    }

    fn parse_expr_opt(&mut self) -> ParseResult<'a, Option<P<Expr>>> {
        use TokenKind::*;

        let expr = if self.token_can_begin_expr() {
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
        let span = block.span;
        Ok(self.make_expr(ExprKind::Block(block), span))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ElseKind {
    If,
    IfThen,
    Loop,
}

#[derive(Clone, Debug)]
pub enum ExprAtomKind {
    Primary(P<Expr>),

    Subscript(Arguments),
    FuncCall(Arguments, ThinVec<P<Block>>),
    FuncCallTrailing(P<Block>),
    MemberAccess(Symbol),
    MemberDynamicAccess(P<Expr>),
    Cast(P<Type>, CastType),
    DotAwait,

    Op(Symbol, OpRole),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpRole {
    Unspecified,
    Prefix,
    Suffix,
    Infix,
}

#[derive(Clone, Debug)]
pub struct ExprAtom {
    kind: ExprAtomKind,
    span: Span,
}

impl ExprAtom {
    pub fn new(kind: ExprAtomKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn new_primary(expr: P<Expr>) -> Self {
        let span = expr.span;

        Self {
            kind: ExprAtomKind::Primary(expr),
            span,
        }
    }

    fn force(&mut self, role: OpRole) {
        match self.kind {
            ExprAtomKind::Op(sym, _) => self.kind = ExprAtomKind::Op(sym, role),
            _ => {}
        }
    }

    fn into(self, role: OpRole) -> Self {
        match self.kind {
            ExprAtomKind::Op(sym, _) => Self {
                kind: ExprAtomKind::Op(sym, role),
                ..self
            },
            _ => self,
        }
    }

    pub fn force_prefix(&mut self) {
        self.force(OpRole::Prefix);
    }

    pub fn into_prefix(self) -> Self {
        self.into(OpRole::Prefix)
    }

    pub fn force_suffix(&mut self) {
        self.force(OpRole::Suffix);
    }

    pub fn into_suffix(self) -> Self {
        self.into(OpRole::Suffix)
    }

    pub fn force_infix(&mut self) {
        self.force(OpRole::Infix);
    }

    pub fn into_infix(self) -> Self {
        self.into(OpRole::Infix)
    }

    pub fn can_be_primary_expr(&self, _ctx: &OpContext) -> bool {
        use ExprAtomKind::*;

        match self.kind {
            Primary(..) => true,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            MemberDynamicAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(..) => false,
        }
    }

    pub fn can_be_suffix_op(&self, ctx: &OpContext) -> bool {
        use ExprAtomKind::*;

        match self.kind {
            Primary(..) => false,

            FuncCall(..) => true,
            FuncCallTrailing(..) => false,
            Subscript(..) => true,
            MemberAccess(..) => true,
            MemberDynamicAccess(..) => true,
            Cast(..) => true,
            DotAwait => true,

            Op(sym, role) => match role {
                OpRole::Prefix => false,
                OpRole::Suffix => true,
                OpRole::Infix => false,
                OpRole::Unspecified => ctx.is_suffix_op(sym),
            },
        }
    }

    pub fn can_be_prefix_op(&self, ctx: &OpContext) -> bool {
        use ExprAtomKind::*;

        match self.kind {
            Primary(..) => false,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            MemberDynamicAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(sym, role) => match role {
                OpRole::Prefix => true,
                OpRole::Suffix => false,
                OpRole::Infix => false,
                OpRole::Unspecified => ctx.is_prefix_op(sym),
            },
        }
    }

    pub fn can_be_infix_op(&self, ctx: &OpContext) -> bool {
        use ExprAtomKind::*;

        match self.kind {
            Primary(..) => false,

            FuncCall(..) => false,
            FuncCallTrailing(..) => false,
            Subscript(..) => false,
            MemberAccess(..) => false,
            MemberDynamicAccess(..) => false,
            Cast(..) => false,
            DotAwait => false,

            Op(sym, role) => match role {
                OpRole::Prefix => false,
                OpRole::Suffix => true,
                OpRole::Infix => false,
                OpRole::Unspecified => ctx.is_infix_op(sym),
            },
        }
    }
}

#[derive(Clone, Debug)]
enum ArrayDictItem {
    Item(P<Expr>),
    KeyValue(P<Expr>, P<Expr>),
    Expansion(P<Expr>),
}
