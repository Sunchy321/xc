use std::collections::{HashMap, HashSet};

use xc_ast::{
    expr::{Argument, Expr, ExprKind, MatchArm},
    func::{Func, FuncBody},
    pattern::PatternKind,
    stmt::{Block, Stmt, StmtKind},
};
use xc_span::Symbol;

use crate::{
    check::Program,
    types::{FunctionTypeFacts, ProgramTypeFacts, Ty},
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct FunctionSummary {
    pub(crate) retained_params: HashSet<usize>,
    pub(crate) returned_from_params: HashSet<usize>,
}

pub(crate) fn infer_summaries(
    program: &Program,
    type_facts: &ProgramTypeFacts,
) -> HashMap<Symbol, FunctionSummary> {
    let visible = program
        .funcs
        .iter()
        .filter(|function| !matches!(function.body, FuncBody::Semicolon))
        .map(|function| (function.ident.name, &**function))
        .collect::<HashMap<_, _>>();
    let mut summaries = visible
        .keys()
        .map(|name| (*name, FunctionSummary::default()))
        .collect::<HashMap<_, _>>();

    let limit = visible.len().saturating_mul(4).max(4);
    for _ in 0..limit {
        let previous = summaries.clone();
        let mut changed = false;
        for (name, function) in &visible {
            let Some(signature) = type_facts.functions.get(name) else {
                continue;
            };
            let next = summarize(function, signature, type_facts, &visible, &previous);
            let current = summaries.entry(*name).or_default();
            let before = current.clone();
            current.retained_params.extend(next.retained_params);
            current
                .returned_from_params
                .extend(next.returned_from_params);
            changed |= *current != before;
        }
        if !changed {
            return summaries;
        }
    }

    // If recursive propagation did not stabilize, use a safe over-approximation.
    for (name, function) in visible {
        let Some(signature) = type_facts.functions.get(&name) else {
            continue;
        };
        let summary = summaries.entry(name).or_default();
        summary.retained_params.extend(
            signature
                .params
                .iter()
                .enumerate()
                .filter_map(|(index, ty)| matches!(ty, Ty::Reference { .. }).then_some(index)),
        );
        if matches!(signature.ret, Ty::Reference { .. }) {
            summary.returned_from_params.extend(
                signature
                    .params
                    .iter()
                    .enumerate()
                    .filter_map(|(index, ty)| matches!(ty, Ty::Reference { .. }).then_some(index)),
            );
        }
        let _ = function;
    }
    summaries
}

fn summarize(
    function: &Func,
    signature: &FunctionTypeFacts,
    type_facts: &ProgramTypeFacts,
    visible: &HashMap<Symbol, &Func>,
    summaries: &HashMap<Symbol, FunctionSummary>,
) -> FunctionSummary {
    let mut scanner = SummaryScanner {
        type_facts,
        visible,
        summaries,
        result: FunctionSummary::default(),
        scopes: vec![HashMap::new()],
        locals: collect_local_names(function),
        returns_reference: matches!(signature.ret, Ty::Reference { .. }),
    };
    for (index, param) in function.sig.params.iter().enumerate() {
        let origins = if matches!(signature.params.get(index), Some(Ty::Reference { .. })) {
            HashSet::from([index])
        } else {
            HashSet::new()
        };
        scanner
            .scopes
            .last_mut()
            .unwrap()
            .insert(param.ident.name, origins);
    }
    match &function.body {
        FuncBody::Block(block) => scanner.scan_block(block, true),
        FuncBody::Arrow(expression) => {
            let origins = scanner.scan_expr(expression, true);
            if scanner.returns_reference {
                scanner.result.returned_from_params.extend(origins);
            }
        }
        FuncBody::Semicolon => {}
    }
    scanner.result
}

struct SummaryScanner<'a> {
    type_facts: &'a ProgramTypeFacts,
    visible: &'a HashMap<Symbol, &'a Func>,
    summaries: &'a HashMap<Symbol, FunctionSummary>,
    result: FunctionSummary,
    scopes: Vec<HashMap<Symbol, HashSet<usize>>>,
    locals: HashSet<Symbol>,
    returns_reference: bool,
}

impl SummaryScanner<'_> {
    fn lookup(&self, name: Symbol) -> HashSet<usize> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name).cloned())
            .unwrap_or_default()
    }

    fn bind(&mut self, name: Symbol, origins: HashSet<usize>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, origins);
        }
    }

    fn assign(&mut self, name: Symbol, origins: HashSet<usize>) {
        if let Some(scope) = self
            .scopes
            .iter_mut()
            .rev()
            .find(|scope| scope.contains_key(&name))
        {
            scope.insert(name, origins);
        }
    }

    fn scan_block(&mut self, block: &Block, function_body: bool) {
        self.scopes.push(HashMap::new());
        for (index, statement) in block.stmts.iter().enumerate() {
            let tail = function_body && index + 1 == block.stmts.len();
            self.scan_stmt(statement, tail);
        }
        self.scopes.pop();
    }

    fn scan_stmt(&mut self, statement: &Stmt, tail: bool) {
        match &statement.kind {
            StmtKind::Let(pattern, initializer) => {
                let origins = initializer
                    .as_ref()
                    .map(|expr| self.scan_expr(expr, false))
                    .unwrap_or_default();
                if let PatternKind::Bind(ident, _) = &pattern.kind {
                    self.bind(ident.name, origins);
                }
            }
            StmtKind::Expr(expression) => {
                let origins = self.scan_expr(expression, tail && self.returns_reference);
                if tail && self.returns_reference {
                    self.result.returned_from_params.extend(origins);
                }
            }
            StmtKind::ExprSemi(expression) => {
                self.scan_expr(expression, false);
            }
            StmtKind::Defer(block) => self.scan_block(block, false),
            StmtKind::Empty => {}
        }
    }

    fn scan_expr(&mut self, expression: &Expr, return_context: bool) -> HashSet<usize> {
        let origins = match &expression.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                self.lookup(path.segments[0].ident.name)
            }
            ExprKind::Paren(inner) => self.scan_expr(inner, return_context),
            ExprKind::Prefix(operator, operand)
                if *operator == xc_span::symbol::op::Amp || *operator == Symbol::intern("&mut") =>
            {
                self.scan_expr(operand, false)
            }
            ExprKind::Prefix(_, operand) | ExprKind::MemberAccess(operand, _) => {
                self.scan_expr(operand, return_context)
            }
            ExprKind::MemberDynamicAccess(base, member) => {
                let mut origins = self.scan_expr(base, false);
                origins.extend(self.scan_expr(member, false));
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::Subscript(base, arguments) => {
                let mut origins = self.scan_expr(base, false);
                for argument in &arguments.0 {
                    origins.extend(self.scan_argument(argument));
                }
                origins
            }
            ExprKind::FuncCall(callee, arguments, blocks) => {
                let target = match &callee.kind {
                    ExprKind::Path(path) if path.segments.len() == 1 => {
                        Some(path.segments[0].ident.name)
                    }
                    _ => None,
                };
                let visible_target = target.and_then(|name| self.visible.get(&name).map(|_| name));
                let summary = visible_target.and_then(|name| self.summaries.get(&name));
                let signature = target.and_then(|name| self.type_facts.functions.get(&name));
                let mut returned = HashSet::new();
                for (index, argument) in arguments.0.iter().enumerate() {
                    let origins = self.scan_argument(argument);
                    if summary.is_some_and(|summary| summary.retained_params.contains(&index)) {
                        self.result.retained_params.extend(origins.iter().copied());
                    }
                    if summary.is_none()
                        && (signature.is_some_and(|signature| {
                            matches!(signature.params.get(index), Some(Ty::Reference { .. }))
                        }) || !origins.is_empty())
                    {
                        self.result.retained_params.extend(origins.iter().copied());
                    }
                    if summary.is_some_and(|summary| summary.returned_from_params.contains(&index))
                        || (summary.is_none()
                            && signature.is_some_and(|signature| {
                                matches!(signature.ret, Ty::Reference { .. })
                                    && matches!(
                                        signature.params.get(index),
                                        Some(Ty::Reference { .. })
                                    )
                            }))
                    {
                        returned.extend(origins);
                    }
                }
                for block in blocks {
                    self.scan_block(block, false);
                }
                if return_context && self.returns_reference {
                    self.result
                        .returned_from_params
                        .extend(returned.iter().copied());
                }
                returned
            }
            ExprKind::Infix(operator, expressions)
                if *operator == xc_span::symbol::op::Assign && expressions.len() >= 2 =>
            {
                let origins = self.scan_expr(&expressions[1], false);
                if let ExprKind::Path(path) = &expressions[0].kind {
                    if path.segments.len() == 1 {
                        let name = path.segments[0].ident.name;
                        if self.locals.contains(&name)
                            && self.scopes.iter().any(|scope| scope.contains_key(&name))
                        {
                            self.assign(name, origins.clone());
                        } else {
                            self.result.retained_params.extend(origins.iter().copied());
                        }
                    }
                }
                origins
            }
            ExprKind::Return(value) => {
                let origins = value
                    .as_ref()
                    .map(|value| self.scan_expr(value, false))
                    .unwrap_or_default();
                if self.returns_reference {
                    self.result
                        .returned_from_params
                        .extend(origins.iter().copied());
                }
                origins
            }
            ExprKind::If(condition, then, else_) => {
                self.scan_expr(condition, false);
                let before = self.scopes.clone();
                self.scopes = before.clone();
                let mut origins = self.scan_expr(then, return_context);
                let then_scopes = self.scopes.clone();
                self.scopes = before.clone();
                if let Some(else_) = else_ {
                    origins.extend(self.scan_expr(else_, return_context));
                }
                for (scope_index, scope) in self.scopes.iter_mut().enumerate() {
                    if let Some(then_scope) = then_scopes.get(scope_index) {
                        for (name, values) in then_scope {
                            scope
                                .entry(*name)
                                .or_default()
                                .extend(values.iter().copied());
                        }
                    }
                }
                if return_context && self.returns_reference {
                    self.result
                        .returned_from_params
                        .extend(origins.iter().copied());
                }
                origins
            }
            ExprKind::Block(block) => {
                let mut origins = HashSet::new();
                self.scopes.push(HashMap::new());
                for (index, statement) in block.stmts.iter().enumerate() {
                    let tail = return_context && index + 1 == block.stmts.len();
                    self.scan_stmt(statement, tail);
                    if tail && let StmtKind::Expr(expr) = &statement.kind {
                        origins.extend(self.scan_expr(expr, false));
                    }
                }
                self.scopes.pop();
                origins
            }
            ExprKind::Array(items) => {
                let mut origins = HashSet::new();
                for item in items {
                    if let xc_ast::expr::ExprItem::Expr(expr)
                    | xc_ast::expr::ExprItem::Expansion(expr) = item
                    {
                        origins.extend(self.scan_expr(expr, false));
                    }
                }
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::Struct(items) => {
                let mut origins = HashSet::new();
                for item in items {
                    match item {
                        xc_ast::expr::StructItem::Ordinal(expr)
                        | xc_ast::expr::StructItem::Named(_, expr)
                        | xc_ast::expr::StructItem::Expansion(expr) => {
                            origins.extend(self.scan_expr(expr, false));
                        }
                        xc_ast::expr::StructItem::ExpansionPlaceHolder => {}
                    }
                }
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::Dict(items) => {
                let mut origins = HashSet::new();
                for item in items {
                    match item {
                        xc_ast::expr::DictItem::KeyValue(key, value) => {
                            origins.extend(self.scan_expr(key, false));
                            origins.extend(self.scan_expr(value, false));
                        }
                        xc_ast::expr::DictItem::Expansion(value) => {
                            origins.extend(self.scan_expr(value, false));
                        }
                    }
                }
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::While(condition, body, else_, _) => {
                self.scan_expr(condition, false);
                let mut origins = self.scan_expr(body, false);
                if let Some(else_) = else_ {
                    origins.extend(self.scan_expr(else_, false));
                }
                origins
            }
            ExprKind::For(_, iter, body, else_, _, _) => {
                self.scan_expr(iter, false);
                self.scan_block(body, false);
                if let Some(else_) = else_ {
                    self.scan_block(else_, false);
                }
                HashSet::new()
            }
            ExprKind::MethodCall(call) => {
                let mut origins = self.scan_expr(&call.receiver, false);
                for argument in &call.args.0 {
                    origins.extend(self.scan_argument(argument));
                }
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::Lambda(_) => {
                let origins = self
                    .scopes
                    .iter()
                    .flat_map(|scope| scope.values().flatten().copied())
                    .collect::<HashSet<_>>();
                self.result.retained_params.extend(origins.iter().copied());
                if return_context && self.returns_reference {
                    self.result
                        .returned_from_params
                        .extend(origins.iter().copied());
                }
                origins
            }
            ExprKind::Match(value, arms) => {
                let mut origins = self.scan_expr(value, false);
                for arm in arms {
                    self.scan_match_arm(arm, &mut origins, return_context);
                }
                origins
            }
            ExprKind::Let(pattern, value) => {
                let origins = self.scan_expr(value, false);
                if let PatternKind::Bind(ident, _) = &pattern.kind {
                    self.bind(ident.name, origins.clone());
                }
                origins
            }
            ExprKind::Is(value, _) | ExprKind::NotIs(value, _) => self.scan_expr(value, false),
            ExprKind::Try(value, catchers, finally) => {
                let origins = self.scan_expr(value, return_context);
                for catcher in catchers {
                    self.scopes.push(HashMap::new());
                    for statement in &catcher.stmts {
                        self.scan_stmt(statement, false);
                    }
                    self.scopes.pop();
                }
                if let Some(finally) = finally {
                    self.scan_block(finally, false);
                }
                self.result.retained_params.extend(origins.iter().copied());
                origins
            }
            ExprKind::Cast(value, _, _) | ExprKind::Suffix(_, value) | ExprKind::Await(value) => {
                self.scan_expr(value, return_context)
            }
            ExprKind::MacroCall(..) => {
                let origins = self
                    .scopes
                    .iter()
                    .flat_map(|scope| scope.values().flatten().copied())
                    .collect::<HashSet<_>>();
                self.result.retained_params.extend(origins.iter().copied());
                if self.returns_reference {
                    self.result
                        .returned_from_params
                        .extend(origins.iter().copied());
                }
                origins
            }
            ExprKind::Literal(_)
            | ExprKind::Nil
            | ExprKind::This
            | ExprKind::SelfExpr
            | ExprKind::Placeholder
            | ExprKind::Caret
            | ExprKind::Dollar
            | ExprKind::LambdaArgOrdinal(_)
            | ExprKind::LambdaArgNamed(_)
            | ExprKind::AnonEnumerator(_)
            | ExprKind::Continue(_)
            | ExprKind::Break(..)
            | ExprKind::Throw(..)
            | ExprKind::Comparison(..)
            | ExprKind::Error(_) => HashSet::new(),
            ExprKind::Path(_) => HashSet::new(),
            ExprKind::Infix(_, expressions) => expressions
                .iter()
                .flat_map(|expression| self.scan_expr(expression, false))
                .collect(),
        };

        if self
            .type_facts
            .expression_type(expression.span)
            .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
        {
            if origins.is_empty() {
                self.scopes
                    .iter()
                    .flat_map(|scope| scope.values().flatten().copied())
                    .collect()
            } else {
                origins
            }
        } else {
            HashSet::new()
        }
    }

    fn scan_argument(&mut self, argument: &Argument) -> HashSet<usize> {
        match argument {
            Argument::Ordinal(expression)
            | Argument::Named(_, expression)
            | Argument::Expansion(expression) => self.scan_expr(expression, false),
        }
    }

    fn scan_match_arm(
        &mut self,
        arm: &MatchArm,
        origins: &mut HashSet<usize>,
        return_context: bool,
    ) {
        if let Some(guard) = &arm.guard {
            origins.extend(self.scan_expr(guard, false));
        }
        if let Some(body) = &arm.body {
            origins.extend(self.scan_expr(body, return_context));
        }
    }
}

fn collect_local_names(function: &Func) -> HashSet<Symbol> {
    fn add_pattern(pattern: &xc_ast::pattern::Pattern, names: &mut HashSet<Symbol>) {
        match &pattern.kind {
            PatternKind::Bind(ident, _) => {
                names.insert(ident.name);
            }
            PatternKind::Paren(inner) | PatternKind::Some(inner) => add_pattern(inner, names),
            PatternKind::Array(items) => {
                for item in items {
                    if let xc_ast::pattern::ArrayPatternItem::Item(pattern) = item {
                        add_pattern(pattern, names);
                    }
                }
            }
            PatternKind::Struct(items) => {
                for item in items {
                    if let xc_ast::pattern::StructPatternItem::Ordinal(pattern)
                    | xc_ast::pattern::StructPatternItem::Named(_, pattern) = item
                    {
                        add_pattern(pattern, names);
                    }
                }
            }
            PatternKind::Wildcard | PatternKind::Expr(_) => {}
        }
    }
    fn walk_stmt(statement: &Stmt, names: &mut HashSet<Symbol>) {
        match &statement.kind {
            StmtKind::Let(pattern, _) => add_pattern(pattern, names),
            StmtKind::Defer(block) => walk_block(block, names),
            StmtKind::Expr(expression) | StmtKind::ExprSemi(expression) => {
                walk_expr(expression, names)
            }
            StmtKind::Empty => {}
        }
    }
    fn walk_block(block: &Block, names: &mut HashSet<Symbol>) {
        for statement in &block.stmts {
            walk_stmt(statement, names);
        }
    }
    fn walk_expr(expression: &Expr, names: &mut HashSet<Symbol>) {
        match &expression.kind {
            xc_ast::expr::ExprKind::Block(block) => walk_block(block, names),
            xc_ast::expr::ExprKind::If(condition, then, else_) => {
                walk_expr(condition, names);
                walk_expr(then, names);
                if let Some(else_) = else_ {
                    walk_expr(else_, names);
                }
            }
            xc_ast::expr::ExprKind::While(condition, body, else_, _) => {
                walk_expr(condition, names);
                walk_expr(body, names);
                if let Some(else_) = else_ {
                    walk_expr(else_, names);
                }
            }
            xc_ast::expr::ExprKind::For(pattern, iter, body, else_, _, _) => {
                add_pattern(pattern, names);
                walk_expr(iter, names);
                walk_block(body, names);
                if let Some(else_) = else_ {
                    walk_block(else_, names);
                }
            }
            xc_ast::expr::ExprKind::Lambda(lambda) => {
                names.extend(lambda.params.iter().map(|param| param.ident.name));
                match &lambda.body {
                    FuncBody::Block(block) => walk_block(block, names),
                    FuncBody::Arrow(expression) => walk_expr(expression, names),
                    FuncBody::Semicolon => {}
                }
            }
            xc_ast::expr::ExprKind::Let(pattern, value) => {
                add_pattern(pattern, names);
                walk_expr(value, names);
            }
            xc_ast::expr::ExprKind::Match(value, arms) => {
                walk_expr(value, names);
                for arm in arms {
                    add_pattern(&arm.pat, names);
                    if let Some(guard) = &arm.guard {
                        walk_expr(guard, names);
                    }
                    if let Some(body) = &arm.body {
                        walk_expr(body, names);
                    }
                }
            }
            xc_ast::expr::ExprKind::Try(value, catchers, finally) => {
                walk_expr(value, names);
                for catcher in catchers {
                    add_pattern(&catcher.pattern, names);
                    for statement in &catcher.stmts {
                        walk_stmt(statement, names);
                    }
                }
                if let Some(finally) = finally {
                    walk_block(finally, names);
                }
            }
            xc_ast::expr::ExprKind::Prefix(_, value)
            | xc_ast::expr::ExprKind::Paren(value)
            | xc_ast::expr::ExprKind::Suffix(_, value)
            | xc_ast::expr::ExprKind::Await(value)
            | xc_ast::expr::ExprKind::Cast(value, _, _)
            | xc_ast::expr::ExprKind::Is(value, _)
            | xc_ast::expr::ExprKind::NotIs(value, _)
            | xc_ast::expr::ExprKind::Return(Some(value))
            | xc_ast::expr::ExprKind::Throw(Some(value))
            | xc_ast::expr::ExprKind::Break(Some(value), _) => walk_expr(value, names),
            xc_ast::expr::ExprKind::MemberAccess(base, _)
            | xc_ast::expr::ExprKind::MemberDynamicAccess(base, _) => walk_expr(base, names),
            xc_ast::expr::ExprKind::Subscript(base, arguments)
            | xc_ast::expr::ExprKind::FuncCall(base, arguments, _) => {
                walk_expr(base, names);
                for argument in &arguments.0 {
                    match argument {
                        Argument::Ordinal(value)
                        | Argument::Named(_, value)
                        | Argument::Expansion(value) => walk_expr(value, names),
                    }
                }
            }
            xc_ast::expr::ExprKind::MethodCall(call) => {
                walk_expr(&call.receiver, names);
                for argument in &call.args.0 {
                    match argument {
                        Argument::Ordinal(value)
                        | Argument::Named(_, value)
                        | Argument::Expansion(value) => walk_expr(value, names),
                    }
                }
                for block in &call.blocks {
                    walk_block(block, names);
                }
            }
            xc_ast::expr::ExprKind::Array(items) => {
                for item in items {
                    if let xc_ast::expr::ExprItem::Expr(value)
                    | xc_ast::expr::ExprItem::Expansion(value) = item
                    {
                        walk_expr(value, names);
                    }
                }
            }
            xc_ast::expr::ExprKind::Struct(items) => {
                for item in items {
                    if let xc_ast::expr::StructItem::Ordinal(value)
                    | xc_ast::expr::StructItem::Named(_, value)
                    | xc_ast::expr::StructItem::Expansion(value) = item
                    {
                        walk_expr(value, names);
                    }
                }
            }
            xc_ast::expr::ExprKind::Dict(items) => {
                for item in items {
                    match item {
                        xc_ast::expr::DictItem::KeyValue(key, value) => {
                            walk_expr(key, names);
                            walk_expr(value, names);
                        }
                        xc_ast::expr::DictItem::Expansion(value) => walk_expr(value, names),
                    }
                }
            }
            xc_ast::expr::ExprKind::Infix(_, expressions)
            | xc_ast::expr::ExprKind::Comparison(_, expressions) => {
                for expression in expressions {
                    walk_expr(expression, names);
                }
            }
            _ => {}
        }
    }

    let mut names = function
        .sig
        .params
        .iter()
        .map(|param| param.ident.name)
        .collect::<HashSet<_>>();
    if let FuncBody::Block(block) = &function.body {
        walk_block(block, &mut names);
    }
    names
}
