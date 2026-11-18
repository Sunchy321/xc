use std::collections::{HashMap, HashSet};

use xc_ast::{
    expr::{Argument, DictItem, Expr, ExprItem, ExprKind, StructItem},
    func::{Func, FuncBody},
    literal::LiteralKind,
    pattern::{Pattern, PatternKind},
    ptr::P,
    stmt::{Block, Stmt, StmtKind},
};
use xc_span::{Span, Symbol};

use crate::{
    borrow_summary::{FunctionSummary, infer_summaries},
    check::Program,
    diagnostic::{CheckDiagnostic, DiagnosticCode},
    types::{ProgramTypeFacts, Ty},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct BindingId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum PlaceRoot {
    Binding(BindingId),
    Temporary(u32),
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct PlacePath {
    root: PlaceRoot,
    projections: Vec<Projection>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Projection {
    Field(Symbol),
    Index(IndexKey),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum IndexKey {
    Int(i128),
    String(String),
    Unknown,
}

impl PlacePath {
    fn overlaps(&self, other: &Self) -> bool {
        if self.root == PlaceRoot::Unknown || other.root == PlaceRoot::Unknown {
            return true;
        }
        if self.root != other.root {
            return false;
        }

        for (left, right) in self.projections.iter().zip(&other.projections) {
            match (left, right) {
                (Projection::Field(left), Projection::Field(right)) if left != right => {
                    return false;
                }
                (
                    Projection::Index(IndexKey::Int(left)),
                    Projection::Index(IndexKey::Int(right)),
                ) if left != right => {
                    return false;
                }
                (
                    Projection::Index(IndexKey::String(left)),
                    Projection::Index(IndexKey::String(right)),
                ) if left != right => return false,
                _ => {}
            }
        }

        true
    }

    fn is_prefix_of(&self, other: &Self) -> bool {
        self.root == other.root
            && self.projections.len() <= other.projections.len()
            && self
                .projections
                .iter()
                .zip(&other.projections)
                .all(|(left, right)| left == right)
    }
}

pub(crate) fn check_program(
    program: &Program,
    type_facts: &ProgramTypeFacts,
) -> Result<(), CheckDiagnostic> {
    let summaries = infer_summaries(program, type_facts);
    let globals = collect_global_bindings(program);
    let global_reference_bindings = global_reference_bindings(program, type_facts, &globals);
    BorrowChecker::new_top_level(program, type_facts, &summaries, &globals).check_top_level()?;
    for function in &program.funcs {
        BorrowChecker::new(
            function,
            type_facts,
            &summaries,
            &globals,
            &global_reference_bindings,
        )
        .check_function(function)?;
    }
    Ok(())
}

struct BorrowChecker<'a> {
    type_facts: &'a ProgramTypeFacts,
    resolver: Resolver,
    summaries: &'a HashMap<Symbol, FunctionSummary>,
    state: FlowState,
    transient_loans: Vec<LoanOrigin>,
    loop_live_bindings: Vec<HashSet<BindingId>>,
    point: u32,
    next_temporary: u32,
    in_loop: usize,
}

impl<'a> BorrowChecker<'a> {
    fn new_top_level(
        program: &Program,
        type_facts: &'a ProgramTypeFacts,
        summaries: &'a HashMap<Symbol, FunctionSummary>,
        globals: &HashMap<Symbol, BindingId>,
    ) -> Self {
        Self {
            type_facts,
            resolver: Resolver::new_top(program, globals),
            summaries,
            state: FlowState::default(),
            transient_loans: Vec::new(),
            loop_live_bindings: Vec::new(),
            point: 0,
            next_temporary: 0,
            in_loop: 0,
        }
    }

    fn new(
        function: &Func,
        type_facts: &'a ProgramTypeFacts,
        summaries: &'a HashMap<Symbol, FunctionSummary>,
        globals: &HashMap<Symbol, BindingId>,
        global_reference_bindings: &HashMap<BindingId, LoanOrigin>,
    ) -> Self {
        let resolver = Resolver::new(function, globals);
        let mut state = FlowState::default();
        for (binding, origin) in global_reference_bindings {
            state.references.insert(*binding, vec![origin.clone()]);
        }
        if let Some(signature) = type_facts.functions.get(&function.ident.name) {
            for (index, param) in function.sig.params.iter().enumerate() {
                let Some(binding) = resolver.declarations.get(&span_key(param.ident.span)) else {
                    continue;
                };
                if matches!(signature.params.get(index), Some(Ty::Reference { .. })) {
                    state.references.insert(
                        *binding,
                        vec![LoanOrigin {
                            place: PlacePath {
                                root: PlaceRoot::Unknown,
                                projections: Vec::new(),
                            },
                            span: param.span,
                        }],
                    );
                }
            }
        }
        Self {
            type_facts,
            resolver,
            summaries,
            state,
            point: function.ident.span.lo.0,
            transient_loans: Vec::new(),
            loop_live_bindings: Vec::new(),
            next_temporary: 0,
            in_loop: 0,
        }
    }

    fn check_function(mut self, function: &Func) -> Result<(), CheckDiagnostic> {
        match &function.body {
            FuncBody::Block(block) => {
                self.visit_block(block)?;
                if let Some(tail) = block_tail_expression(block) {
                    self.consume(tail)?;
                }
                Ok(())
            }
            FuncBody::Arrow(expression) => {
                self.visit_expr(expression)?;
                self.consume(expression)
            }
            FuncBody::Semicolon => Ok(()),
        }
    }

    fn check_top_level(mut self) -> Result<(), CheckDiagnostic> {
        let statements = self.resolver.top_level.clone();
        for statement in &statements {
            if self.in_loop == 0 {
                self.expire_at(statement.span.lo.0);
            }
            self.point = statement.span.lo.0;
            self.visit_stmt(statement)?;
            if self.in_loop == 0 {
                self.expire_at(statement.span.hi.0);
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<Vec<LoanOrigin>, CheckDiagnostic> {
        let mut result = Vec::new();
        for (index, statement) in block.stmts.iter().enumerate() {
            if self.in_loop == 0 {
                self.expire_at(statement.span.lo.0);
            }
            self.point = statement.span.lo.0;
            if index + 1 == block.stmts.len()
                && let StmtKind::Expr(expression) = &statement.kind
            {
                result = self.visit_expr(expression)?;
            } else {
                self.visit_stmt(statement)?;
            }
            if self.in_loop == 0 {
                self.expire_at(statement.span.hi.0);
            }
        }
        Ok(result)
    }

    fn expire_at(&mut self, point: u32) {
        self.point = point;
    }

    fn visit_stmt(&mut self, statement: &Stmt) -> Result<(), CheckDiagnostic> {
        match &statement.kind {
            StmtKind::Expr(expression) | StmtKind::ExprSemi(expression) => {
                self.visit_expr(expression)?;
                Ok(())
            }
            StmtKind::Let(pattern, initializer) => {
                let binding = self
                    .resolver
                    .declarations
                    .get(&span_key(pattern.span))
                    .copied();
                let origins = if let Some(initializer) = initializer {
                    let origins = self.visit_expr(initializer)?;
                    self.consume(initializer)?;
                    self.ensure_reference_origins(initializer, origins)
                } else {
                    Vec::new()
                };
                if let Some(binding) = binding {
                    let place = self.binding_place(binding);
                    self.state.reinitialize(&place);
                    if initializer.as_ref().is_some_and(|expr| {
                        self.type_of(expr)
                            .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                    }) {
                        self.state.references.insert(binding, origins);
                    }
                }
                Ok(())
            }
            StmtKind::Defer(block) => self.visit_block(block).map(|_| ()),
            StmtKind::Empty => Ok(()),
        }
    }

    fn visit_expr(&mut self, expression: &Expr) -> Result<Vec<LoanOrigin>, CheckDiagnostic> {
        match &expression.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                if let Some(binding) = self.resolver.paths.get(&span_key(expression.span)).copied()
                {
                    self.check_available(&self.binding_place(binding), expression.span)?;
                    Ok(self
                        .state
                        .references
                        .get(&binding)
                        .cloned()
                        .unwrap_or_default())
                } else {
                    Ok(Vec::new())
                }
            }
            ExprKind::Paren(inner) => self.visit_expr(inner),
            ExprKind::Prefix(operator, operand) => {
                if *operator == xc_span::symbol::op::Amp || *operator == Symbol::intern("&mut") {
                    let places = self.place_paths(operand);
                    if places.is_empty() {
                        let nested = self.visit_expr(operand)?;
                        if !nested.is_empty() {
                            return Err(self.unsupported(
                                expression.span,
                                "taking a reference to a value containing a reference is unsupported",
                            ));
                        }
                        let id = self.next_temporary;
                        self.next_temporary += 1;
                        return Ok(vec![LoanOrigin {
                            place: PlacePath {
                                root: PlaceRoot::Temporary(id),
                                projections: Vec::new(),
                            },
                            span: expression.span,
                        }]);
                    }
                    for place in &places {
                        self.check_available(place, operand.span)?;
                    }
                    return Ok(places
                        .into_iter()
                        .map(|place| LoanOrigin {
                            place,
                            span: expression.span,
                        })
                        .collect());
                }
                if *operator == xc_span::symbol::op::Multiply {
                    let origins = self.visit_expr(operand)?;
                    for origin in &origins {
                        self.check_available(&origin.place, expression.span)?;
                    }
                    if self
                        .type_of(expression)
                        .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                    {
                        Ok(self.ensure_reference_origins(expression, origins))
                    } else {
                        Ok(Vec::new())
                    }
                } else {
                    self.visit_expr(operand)
                }
            }
            ExprKind::MemberAccess(base, _) => {
                let origins = self.visit_expr(base)?;
                self.check_expr_places_available(expression)?;
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::MemberDynamicAccess(base, member) => {
                let mut origins = self.visit_expr(base)?;
                origins.extend(self.visit_expr(member)?);
                self.check_expr_places_available(expression)?;
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::Subscript(base, arguments) => {
                let mut origins = self.visit_expr(base)?;
                for argument in &arguments.0 {
                    origins.extend(self.visit_argument(argument)?);
                }
                self.check_expr_places_available(expression)?;
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::FuncCall(callee, arguments, blocks) => {
                self.visit_call(callee, &arguments.0, blocks.as_slice(), expression)
            }
            ExprKind::MethodCall(call) => {
                self.visit_expr(&call.receiver)?;
                let mut origins = Vec::new();
                for argument in &call.args.0 {
                    let argument_expr = match argument {
                        Argument::Ordinal(expression)
                        | Argument::Named(_, expression)
                        | Argument::Expansion(expression) => expression,
                    };
                    let argument_origins = self.visit_argument(argument)?;
                    if self
                        .type_of(argument_expr)
                        .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                    {
                        self.state
                            .retained_loans
                            .extend(argument_origins.iter().cloned());
                    }
                    origins.extend(argument_origins);
                }
                for block in &call.blocks {
                    let _ = self.visit_block(block)?;
                }
                if self
                    .type_of(expression)
                    .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                {
                    let origins = self.ensure_reference_origins(expression, origins);
                    self.state.retained_loans.extend(origins.iter().cloned());
                    Ok(origins)
                } else {
                    Ok(Vec::new())
                }
            }
            ExprKind::Infix(operator, expressions)
                if *operator == xc_span::symbol::op::Assign && expressions.len() >= 2 =>
            {
                let target = &expressions[0];
                let value = &expressions[1];
                let origins = self.visit_expr(value)?;
                self.consume(value)?;
                let targets = self.place_paths(target);
                for place in &targets {
                    self.state.reinitialize(place);
                }
                if let Some(binding) = self.resolver.paths.get(&span_key(target.span)).copied() {
                    if self
                        .type_of(target)
                        .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                    {
                        let origins = self.ensure_reference_origins(value, origins);
                        self.state.references.insert(binding, origins);
                        if self.resolver.global_bindings.contains(&binding) {
                            if let Some(origins) = self.state.references.get(&binding).cloned() {
                                self.state.retained_loans.extend(origins);
                            }
                        }
                    }
                }
                for expression in expressions.iter().skip(2) {
                    self.visit_expr(expression)?;
                }
                Ok(Vec::new())
            }
            ExprKind::Is(value, _) | ExprKind::NotIs(value, _) => {
                self.visit_expr(value)?;
                Ok(Vec::new())
            }
            ExprKind::Infix(_, expressions) | ExprKind::Comparison(_, expressions) => {
                let mut origins = Vec::new();
                for child in expressions {
                    origins.extend(self.visit_expr(child)?);
                }
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::Array(items) => {
                let mut origins = Vec::new();
                for item in items {
                    match item {
                        ExprItem::Expr(expression) | ExprItem::Expansion(expression) => {
                            origins.extend(self.visit_expr(expression)?);
                        }
                        ExprItem::ExpansionPlaceHolder => {}
                    }
                }
                self.reject_aggregate_origins(expression, origins)
            }
            ExprKind::Struct(items) => {
                let mut origins = Vec::new();
                for item in items {
                    match item {
                        StructItem::Ordinal(value)
                        | StructItem::Named(_, value)
                        | StructItem::Expansion(value) => origins.extend(self.visit_expr(value)?),
                        StructItem::ExpansionPlaceHolder => {}
                    }
                }
                self.reject_aggregate_origins(expression, origins)
            }
            ExprKind::Dict(items) => {
                let mut origins = Vec::new();
                for item in items {
                    match item {
                        DictItem::KeyValue(key, value) => {
                            origins.extend(self.visit_expr(key)?);
                            origins.extend(self.visit_expr(value)?);
                        }
                        DictItem::Expansion(value) => origins.extend(self.visit_expr(value)?),
                    }
                }
                self.reject_aggregate_origins(expression, origins)
            }
            ExprKind::Block(block) => {
                let origins = self.visit_block(block)?;
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::If(condition, then, else_) => {
                self.visit_expr(condition)?;
                let before = self.state.clone();
                self.state = before.clone();
                let then_origins = self.visit_expr(then)?;
                let then_state = self.state.clone();
                self.state = before.clone();
                let else_origins = if let Some(else_) = else_ {
                    self.visit_expr(else_)?
                } else {
                    Vec::new()
                };
                let else_state = self.state.clone();
                self.state = FlowState::join(&then_state, &else_state);
                let origins = merge_origins(then_origins, else_origins);
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::While(condition, body, else_, _) => {
                self.visit_expr(condition)?;
                self.loop_live_bindings
                    .push(self.reference_bindings_used_in(expression.span));
                let entry = self.state.clone();
                let mut head = entry.clone();
                loop {
                    self.state = head.clone();
                    self.in_loop += 1;
                    let body_origins = self.visit_expr(body)?;
                    if let Some(else_) = else_ {
                        self.visit_expr(else_)?;
                    }
                    self.in_loop -= 1;
                    let joined = FlowState::join(&entry, &self.state);
                    if joined == head {
                        self.state = joined;
                        self.loop_live_bindings.pop();
                        return Ok(self.ensure_reference_origins(expression, body_origins));
                    }
                    head = joined;
                }
            }
            ExprKind::For(_, iter, body, else_, _, _) => {
                self.visit_expr(iter)?;
                self.loop_live_bindings
                    .push(self.reference_bindings_used_in(expression.span));
                let entry = self.state.clone();
                self.state = entry.clone();
                self.in_loop += 1;
                let _ = self.visit_block(body)?;
                if let Some(else_) = else_ {
                    let _ = self.visit_block(else_)?;
                }
                self.in_loop -= 1;
                self.loop_live_bindings.pop();
                self.state = FlowState::join(&entry, &self.state);
                Ok(self.ensure_reference_origins(expression, Vec::new()))
            }
            ExprKind::Return(value) => {
                let origins = if let Some(value) = value {
                    let origins = self.visit_expr(value)?;
                    self.consume(value)?;
                    origins
                } else {
                    Vec::new()
                };
                Ok(origins)
            }
            ExprKind::Break(value, _) | ExprKind::Throw(value) => {
                if let Some(value) = value {
                    self.visit_expr(value)?;
                    self.consume(value)?;
                }
                Ok(Vec::new())
            }
            ExprKind::Let(pattern, value) => {
                let origins = self.visit_expr(value)?;
                self.consume(value)?;
                let origins = self.ensure_reference_origins(value, origins);
                if let Some(binding) = self
                    .resolver
                    .declarations
                    .get(&span_key(pattern.span))
                    .copied()
                {
                    self.state.references.insert(binding, origins.clone());
                }
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::Match(value, arms) => {
                self.visit_expr(value)?;
                let entry = self.state.clone();
                let mut merged = None;
                let mut origins = Vec::new();
                for arm in arms {
                    self.state = entry.clone();
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard)?;
                    }
                    if let Some(body) = &arm.body {
                        origins.extend(self.visit_expr(body)?);
                    }
                    merged = Some(match merged {
                        Some(previous) => FlowState::join(&previous, &self.state),
                        None => self.state.clone(),
                    });
                }
                self.state = merged.unwrap_or(entry);
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::Try(value, catchers, finally) => {
                let mut origins = self.visit_expr(value)?;
                for catcher in catchers {
                    for statement in &catcher.stmts {
                        if let StmtKind::Expr(expr) | StmtKind::ExprSemi(expr) = &statement.kind {
                            origins.extend(self.visit_expr(expr)?);
                        }
                    }
                }
                if let Some(finally) = finally {
                    let _ = self.visit_block(finally)?;
                }
                Ok(self.ensure_reference_origins(expression, origins))
            }
            ExprKind::Lambda(lambda) => {
                if self.lambda_captures_borrow(expression.span, lambda) {
                    Err(self.unsupported(
                        expression.span,
                        "reference capture by closures is unsupported",
                    ))
                } else {
                    Ok(Vec::new())
                }
            }
            ExprKind::Cast(value, _, _) | ExprKind::Suffix(_, value) | ExprKind::Await(value) => {
                let origins = self.visit_expr(value)?;
                if self
                    .type_of(expression)
                    .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
                {
                    Ok(self.ensure_reference_origins(expression, origins))
                } else {
                    Ok(Vec::new())
                }
            }
            ExprKind::Literal(_)
            | ExprKind::Nil
            | ExprKind::This
            | ExprKind::SelfExpr
            | ExprKind::MacroCall(..)
            | ExprKind::Placeholder
            | ExprKind::Caret
            | ExprKind::Dollar
            | ExprKind::LambdaArgOrdinal(_)
            | ExprKind::LambdaArgNamed(_)
            | ExprKind::AnonEnumerator(_)
            | ExprKind::Continue(_)
            | ExprKind::Error(_) => Ok(Vec::new()),
            ExprKind::Path(_) => Ok(Vec::new()),
        }
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        arguments: &[Argument],
        blocks: &[P<Block>],
        expression: &Expr,
    ) -> Result<Vec<LoanOrigin>, CheckDiagnostic> {
        let target = match &callee.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => Some(path.segments[0].ident.name),
            _ => None,
        };
        let signature = match &callee.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                self.type_facts.functions.get(&path.segments[0].ident.name)
            }
            _ => None,
        };
        let summary = target.and_then(|name| self.summaries.get(&name));
        let previous_transient = std::mem::take(&mut self.transient_loans);
        let mut ref_origins = Vec::new();
        for (index, argument) in arguments.iter().enumerate() {
            let argument_expr = match argument {
                Argument::Ordinal(expression)
                | Argument::Named(_, expression)
                | Argument::Expansion(expression) => expression,
            };
            let expected = signature.and_then(|signature| signature.params.get(index));
            let origins = self.visit_expr(argument_expr)?;
            if matches!(expected, Some(Ty::Reference { .. }))
                || self
                    .type_of(argument_expr)
                    .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
            {
                let origins =
                    if matches!(expected, Some(Ty::Reference { .. })) && origins.is_empty() {
                        vec![self.unknown_origin(argument_expr.span)]
                    } else {
                        self.ensure_reference_origins(argument_expr, origins)
                    };
                self.transient_loans.extend(origins.iter().cloned());
                ref_origins.push((index, origins));
            } else {
                self.consume(argument_expr)?;
            }
        }
        for block in blocks {
            let _ = self.visit_block(block)?;
        }

        let mut returned = Vec::new();
        for (index, origins) in &ref_origins {
            if summary
                .map(|summary| summary.retained_params.contains(index))
                .unwrap_or(true)
            {
                self.state.retained_loans.extend(origins.iter().cloned());
            }
            if summary
                .map(|summary| summary.returned_from_params.contains(index))
                .unwrap_or_else(|| {
                    matches!(
                        signature.map(|signature| &signature.ret),
                        Some(Ty::Reference { .. })
                    )
                })
            {
                returned.extend(origins.iter().cloned());
            }
        }
        self.transient_loans = previous_transient;

        if self
            .type_of(expression)
            .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
        {
            Ok(self.ensure_reference_origins(expression, returned))
        } else {
            Ok(Vec::new())
        }
    }

    fn visit_argument(&mut self, argument: &Argument) -> Result<Vec<LoanOrigin>, CheckDiagnostic> {
        let expression = match argument {
            Argument::Ordinal(expression)
            | Argument::Named(_, expression)
            | Argument::Expansion(expression) => expression,
        };
        let origins = self.visit_expr(expression)?;
        self.consume(expression)?;
        Ok(self.ensure_reference_origins(expression, origins))
    }

    fn consume(&mut self, expression: &Expr) -> Result<(), CheckDiagnostic> {
        let is_copy = self.type_of(expression).is_some_and(Ty::is_copy);
        if is_copy {
            return Ok(());
        }
        let places = self.place_paths(expression);
        for place in places {
            self.check_available(&place, expression.span)?;
            for origin in self.active_loans() {
                if origin.place.overlaps(&place) {
                    return Err(CheckDiagnostic::new(
                        DiagnosticCode::Type,
                        expression.span,
                        format!(
                            "cannot consume a borrowed place; live reference at bytes {}..{}",
                            origin.span.lo.0, origin.span.hi.0
                        ),
                    ));
                }
            }
            if !matches!(place.root, PlaceRoot::Unknown | PlaceRoot::Temporary(_)) {
                self.state.mark_moved(place);
            }
        }
        Ok(())
    }

    fn active_loans(&self) -> Vec<LoanOrigin> {
        let mut origins = self.state.retained_loans.clone();
        origins.extend(self.transient_loans.iter().cloned());
        for (binding, references) in &self.state.references {
            if self.is_reference_live(*binding) {
                origins.extend(references.iter().cloned());
            }
        }
        origins
    }

    fn is_reference_live(&self, binding: BindingId) -> bool {
        self.loop_live_bindings
            .iter()
            .any(|bindings| bindings.contains(&binding))
            || self
                .resolver
                .last_uses
                .get(&binding)
                .is_some_and(|last| *last > self.point)
    }

    fn reference_bindings_used_in(&self, span: Span) -> HashSet<BindingId> {
        self.resolver
            .last_uses
            .iter()
            .filter_map(|(binding, last)| {
                (span.lo.0 <= *last && *last <= span.hi.0).then_some(*binding)
            })
            .collect()
    }

    fn check_available(&self, place: &PlacePath, span: Span) -> Result<(), CheckDiagnostic> {
        if place.root == PlaceRoot::Unknown {
            return Ok(());
        }
        match self.state.move_state(place) {
            MoveState::Available => Ok(()),
            MoveState::Moved => Err(CheckDiagnostic::new(
                DiagnosticCode::Type,
                span,
                "use of moved value",
            )),
            MoveState::MaybeMoved => Err(CheckDiagnostic::new(
                DiagnosticCode::Type,
                span,
                "use of maybe moved value",
            )),
        }
    }

    fn check_expr_places_available(&self, expression: &Expr) -> Result<(), CheckDiagnostic> {
        for place in self.place_paths(expression) {
            self.check_available(&place, expression.span)?;
        }
        Ok(())
    }

    fn place_paths(&self, expression: &Expr) -> Vec<PlacePath> {
        match &expression.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => self
                .resolver
                .paths
                .get(&span_key(expression.span))
                .copied()
                .map(|binding| vec![self.binding_place(binding)])
                .unwrap_or_default(),
            ExprKind::Paren(inner) => self.place_paths(inner),
            ExprKind::Block(block) => block_tail_expression(block)
                .map(|tail| self.place_paths(tail))
                .unwrap_or_default(),
            ExprKind::If(_, then, else_) => {
                let mut places = self.place_paths(then);
                if let Some(else_) = else_ {
                    places.extend(self.place_paths(else_));
                }
                places
            }
            ExprKind::Match(_, arms) => arms
                .iter()
                .filter_map(|arm| arm.body.as_ref())
                .flat_map(|body| self.place_paths(body))
                .collect(),
            ExprKind::MemberAccess(base, member) => self
                .place_paths(base)
                .into_iter()
                .map(|mut place| {
                    place.projections.push(Projection::Field(*member));
                    place
                })
                .collect(),
            ExprKind::Subscript(base, arguments) => {
                let mut places = self.place_paths(base);
                for argument in &arguments.0 {
                    let key = match argument {
                        Argument::Ordinal(index) => self.index_key(index),
                        _ => IndexKey::Unknown,
                    };
                    for place in &mut places {
                        place.projections.push(Projection::Index(key.clone()));
                    }
                }
                places
            }
            ExprKind::Prefix(operator, reference) if *operator == xc_span::symbol::op::Multiply => {
                self.origins_for_value(reference)
                    .into_iter()
                    .map(|origin| origin.place)
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    fn origins_for_value(&self, expression: &Expr) -> Vec<LoanOrigin> {
        match &expression.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => self
                .resolver
                .paths
                .get(&span_key(expression.span))
                .and_then(|binding| self.state.references.get(binding))
                .cloned()
                .unwrap_or_default(),
            ExprKind::Paren(inner) => self.origins_for_value(inner),
            ExprKind::Prefix(operator, operand)
                if *operator == xc_span::symbol::op::Amp || *operator == Symbol::intern("&mut") =>
            {
                self.place_paths(operand)
                    .into_iter()
                    .map(|place| LoanOrigin {
                        place,
                        span: expression.span,
                    })
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    fn index_key(&self, expression: &Expr) -> IndexKey {
        match &expression.kind {
            ExprKind::Literal(literal) if literal.kind == LiteralKind::Integer => literal
                .value
                .as_str()
                .parse::<i128>()
                .map(IndexKey::Int)
                .unwrap_or(IndexKey::Unknown),
            ExprKind::Literal(literal)
                if matches!(
                    literal.kind,
                    LiteralKind::String | LiteralKind::RawString { .. }
                ) =>
            {
                IndexKey::String(literal.value.as_str().to_owned())
            }
            _ => IndexKey::Unknown,
        }
    }

    fn binding_place(&self, binding: BindingId) -> PlacePath {
        PlacePath {
            root: PlaceRoot::Binding(binding),
            projections: Vec::new(),
        }
    }

    fn type_of(&self, expression: &Expr) -> Option<&Ty> {
        self.type_facts.expression_type(expression.span)
    }

    fn ensure_reference_origins(
        &self,
        expression: &Expr,
        origins: Vec<LoanOrigin>,
    ) -> Vec<LoanOrigin> {
        if origins.is_empty()
            && self
                .type_of(expression)
                .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
        {
            vec![self.unknown_origin(expression.span)]
        } else {
            origins
        }
    }

    fn unknown_origin(&self, span: Span) -> LoanOrigin {
        LoanOrigin {
            place: PlacePath {
                root: PlaceRoot::Unknown,
                projections: Vec::new(),
            },
            span,
        }
    }

    fn unsupported(&self, span: Span, message: &'static str) -> CheckDiagnostic {
        CheckDiagnostic::new(DiagnosticCode::Unsupported, span, message)
    }

    fn reject_aggregate_origins(
        &self,
        expression: &Expr,
        origins: Vec<LoanOrigin>,
    ) -> Result<Vec<LoanOrigin>, CheckDiagnostic> {
        if origins.is_empty() {
            Ok(Vec::new())
        } else {
            Err(self.unsupported(
                expression.span,
                "storing references in aggregate values is unsupported",
            ))
        }
    }

    fn lambda_captures_borrow(&self, lambda_span: Span, lambda: &xc_ast::lambda::Lambda) -> bool {
        if func_body_contains_reference_operator(&lambda.body) {
            return true;
        }
        let local_bindings = self
            .resolver
            .declaration_spans
            .iter()
            .filter_map(|(binding, span)| span_is_within(*span, lambda_span).then_some(*binding))
            .collect::<HashSet<_>>();
        let active_loans = self.active_loans();
        self.resolver.paths.iter().any(|(key, binding)| {
            let span = Span::new(xc_span::BytePos(key.0), xc_span::BytePos(key.1));
            span_is_within(span, lambda_span)
                && !local_bindings.contains(binding)
                && (self.state.references.contains_key(binding)
                    || active_loans
                        .iter()
                        .any(|origin| origin.place.overlaps(&self.binding_place(*binding))))
        })
    }
}

fn span_is_within(child: Span, parent: Span) -> bool {
    parent.lo.0 <= child.lo.0 && child.hi.0 <= parent.hi.0
}

fn func_body_contains_reference_operator(body: &FuncBody) -> bool {
    match body {
        FuncBody::Block(block) => block.stmts.iter().any(stmt_contains_reference_operator),
        FuncBody::Arrow(expression) => expr_contains_reference_operator(expression),
        FuncBody::Semicolon => false,
    }
}

fn stmt_contains_reference_operator(statement: &Stmt) -> bool {
    match &statement.kind {
        StmtKind::Expr(expression) | StmtKind::ExprSemi(expression) => {
            expr_contains_reference_operator(expression)
        }
        StmtKind::Let(_, initializer) => initializer
            .as_ref()
            .is_some_and(|expression| expr_contains_reference_operator(expression)),
        StmtKind::Defer(block) => block.stmts.iter().any(stmt_contains_reference_operator),
        StmtKind::Empty => false,
    }
}

fn expr_contains_reference_operator(expression: &Expr) -> bool {
    match &expression.kind {
        ExprKind::Prefix(operator, _)
            if {
                *operator == xc_span::symbol::op::Amp
                    || *operator == Symbol::intern("&mut")
                    || *operator == xc_span::symbol::op::Multiply
            } =>
        {
            true
        }
        ExprKind::Prefix(_, value)
        | ExprKind::Paren(value)
        | ExprKind::Suffix(_, value)
        | ExprKind::Await(value)
        | ExprKind::Cast(value, _, _)
        | ExprKind::Is(value, _)
        | ExprKind::NotIs(value, _)
        | ExprKind::Return(Some(value))
        | ExprKind::Throw(Some(value))
        | ExprKind::Break(Some(value), _) => expr_contains_reference_operator(value),
        ExprKind::MemberAccess(base, _) | ExprKind::MemberDynamicAccess(base, _) => {
            expr_contains_reference_operator(base)
        }
        ExprKind::Subscript(base, arguments) | ExprKind::FuncCall(base, arguments, _) => {
            expr_contains_reference_operator(base)
                || arguments.0.iter().any(|argument| match argument {
                    Argument::Ordinal(value)
                    | Argument::Named(_, value)
                    | Argument::Expansion(value) => expr_contains_reference_operator(value),
                })
        }
        ExprKind::MethodCall(call) => {
            expr_contains_reference_operator(&call.receiver)
                || call.args.0.iter().any(|argument| match argument {
                    Argument::Ordinal(value)
                    | Argument::Named(_, value)
                    | Argument::Expansion(value) => expr_contains_reference_operator(value),
                })
        }
        ExprKind::Infix(_, expressions) | ExprKind::Comparison(_, expressions) => expressions
            .iter()
            .any(|value| expr_contains_reference_operator(value)),
        ExprKind::Array(items) => items.iter().any(|item| match item {
            ExprItem::Expr(value) | ExprItem::Expansion(value) => {
                expr_contains_reference_operator(value)
            }
            ExprItem::ExpansionPlaceHolder => false,
        }),
        ExprKind::Struct(items) => items.iter().any(|item| match item {
            StructItem::Ordinal(value)
            | StructItem::Named(_, value)
            | StructItem::Expansion(value) => expr_contains_reference_operator(value),
            StructItem::ExpansionPlaceHolder => false,
        }),
        ExprKind::Dict(items) => items.iter().any(|item| match item {
            DictItem::KeyValue(key, value) => {
                expr_contains_reference_operator(key) || expr_contains_reference_operator(value)
            }
            DictItem::Expansion(value) => expr_contains_reference_operator(value),
        }),
        ExprKind::Block(block) => block.stmts.iter().any(stmt_contains_reference_operator),
        ExprKind::If(condition, then, else_) => {
            expr_contains_reference_operator(condition)
                || expr_contains_reference_operator(then)
                || else_
                    .as_ref()
                    .is_some_and(|expression| expr_contains_reference_operator(expression))
        }
        ExprKind::While(condition, body, else_, _) => {
            expr_contains_reference_operator(condition)
                || expr_contains_reference_operator(body)
                || else_
                    .as_ref()
                    .is_some_and(|expression| expr_contains_reference_operator(expression))
        }
        ExprKind::For(_, iter, body, else_, _, _) => {
            expr_contains_reference_operator(iter)
                || body.stmts.iter().any(stmt_contains_reference_operator)
                || else_
                    .as_ref()
                    .is_some_and(|block| block.stmts.iter().any(stmt_contains_reference_operator))
        }
        ExprKind::Match(value, arms) => {
            expr_contains_reference_operator(value)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|guard| expr_contains_reference_operator(guard))
                        || arm
                            .body
                            .as_ref()
                            .is_some_and(|body| expr_contains_reference_operator(body))
                })
        }
        ExprKind::Try(value, catchers, finally) => {
            expr_contains_reference_operator(value)
                || catchers
                    .iter()
                    .any(|catcher| catcher.stmts.iter().any(stmt_contains_reference_operator))
                || finally
                    .as_ref()
                    .is_some_and(|block| block.stmts.iter().any(stmt_contains_reference_operator))
        }
        ExprKind::Lambda(lambda) => func_body_contains_reference_operator(&lambda.body),
        ExprKind::Let(_, value) => expr_contains_reference_operator(value),
        // Macro expansions are unavailable to this scan, so a closure containing one
        // may hide a reference operation and must be treated conservatively.
        ExprKind::MacroCall(..) => true,
        ExprKind::Return(_)
        | ExprKind::Throw(_)
        | ExprKind::Break(_, _)
        | ExprKind::Path(_)
        | ExprKind::Literal(_)
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
        | ExprKind::Error(_) => false,
    }
}

fn merge_origins(mut left: Vec<LoanOrigin>, right: Vec<LoanOrigin>) -> Vec<LoanOrigin> {
    for origin in right {
        if !left.contains(&origin) {
            left.push(origin);
        }
    }
    left
}

fn block_tail_expression(block: &Block) -> Option<&Expr> {
    match block.stmts.last().map(|statement| &statement.kind) {
        Some(StmtKind::Expr(expression)) => Some(expression),
        _ => None,
    }
}

type SpanKey = (u32, u32);

fn span_key(span: Span) -> SpanKey {
    (span.lo.0, span.hi.0)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MoveState {
    Available,
    Moved,
    MaybeMoved,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LoanOrigin {
    place: PlacePath,
    span: Span,
}

impl std::hash::Hash for LoanOrigin {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.place.hash(state);
        self.span.lo.0.hash(state);
        self.span.hi.0.hash(state);
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct FlowState {
    moves: HashMap<PlacePath, MoveState>,
    references: HashMap<BindingId, Vec<LoanOrigin>>,
    retained_loans: Vec<LoanOrigin>,
}

impl FlowState {
    fn move_state(&self, place: &PlacePath) -> MoveState {
        let mut state = MoveState::Available;
        for (moved, candidate) in &self.moves {
            if !moved.overlaps(place) {
                continue;
            }
            match candidate {
                MoveState::Moved => return MoveState::Moved,
                MoveState::MaybeMoved => state = MoveState::MaybeMoved,
                MoveState::Available => {}
            }
        }
        state
    }

    fn mark_moved(&mut self, place: PlacePath) {
        self.moves.retain(|other, _| !place.is_prefix_of(other));
        self.moves.insert(place, MoveState::Moved);
    }

    fn reinitialize(&mut self, place: &PlacePath) {
        if !place.projections.is_empty() {
            self.moves.remove(place);
            return;
        }
        self.moves.retain(|other, _| !place.is_prefix_of(other));
    }

    fn join(left: &Self, right: &Self) -> Self {
        let mut moves = HashMap::new();
        let keys = left
            .moves
            .keys()
            .chain(right.moves.keys())
            .cloned()
            .collect::<HashSet<_>>();
        for key in keys {
            let left_state = left
                .moves
                .get(&key)
                .copied()
                .unwrap_or(MoveState::Available);
            let right_state = right
                .moves
                .get(&key)
                .copied()
                .unwrap_or(MoveState::Available);
            let state = match (left_state, right_state) {
                (MoveState::Available, MoveState::Available) => MoveState::Available,
                (MoveState::Moved, MoveState::Moved) => MoveState::Moved,
                _ => MoveState::MaybeMoved,
            };
            if state != MoveState::Available {
                moves.insert(key, state);
            }
        }

        let mut references = left.references.clone();
        for (binding, origins) in &right.references {
            let existing = references.entry(*binding).or_default();
            for origin in origins {
                if !existing.contains(origin) {
                    existing.push(origin.clone());
                }
            }
        }

        let mut retained_loans = left.retained_loans.clone();
        for origin in &right.retained_loans {
            if !retained_loans.contains(origin) {
                retained_loans.push(origin.clone());
            }
        }

        Self {
            moves,
            references,
            retained_loans,
        }
    }
}

#[derive(Default)]
struct Resolver {
    next_binding: u32,
    scopes: Vec<HashMap<Symbol, BindingId>>,
    global_bindings: HashSet<BindingId>,
    declarations: HashMap<SpanKey, BindingId>,
    declaration_spans: HashMap<BindingId, Span>,
    paths: HashMap<SpanKey, BindingId>,
    last_uses: HashMap<BindingId, u32>,
    top_level: Vec<Stmt>,
}

impl Resolver {
    fn new_top(program: &Program, globals: &HashMap<Symbol, BindingId>) -> Self {
        let mut this = Self {
            next_binding: globals.len() as u32,
            scopes: vec![globals.clone()],
            global_bindings: globals.values().copied().collect(),
            top_level: program.top.clone(),
            ..Self::default()
        };
        for statement in &program.top {
            if let StmtKind::Let(pattern, initializer) = &statement.kind {
                if let Some(initializer) = initializer {
                    this.walk_expr(initializer);
                }
                if let PatternKind::Bind(ident, _) = &pattern.kind
                    && let Some(binding) = globals.get(&ident.name)
                {
                    this.declarations.insert(span_key(pattern.span), *binding);
                    this.declaration_spans.insert(*binding, pattern.span);
                }
            } else {
                this.walk_stmt(statement);
            }
        }
        this
    }

    fn new(function: &Func, globals: &HashMap<Symbol, BindingId>) -> Self {
        let mut this = Self {
            next_binding: globals.len() as u32,
            scopes: vec![globals.clone()],
            global_bindings: globals.values().copied().collect(),
            ..Self::default()
        };
        for param in &function.sig.params {
            let binding = this.fresh_binding();
            this.declarations
                .insert(span_key(param.ident.span), binding);
            this.declaration_spans.insert(binding, param.ident.span);
            this.scopes[0].insert(param.ident.name, binding);
        }
        match &function.body {
            FuncBody::Block(block) => this.walk_block(block, true),
            FuncBody::Arrow(expression) => this.walk_expr(expression),
            FuncBody::Semicolon => {}
        }
        this
    }

    fn fresh_binding(&mut self) -> BindingId {
        let binding = BindingId(self.next_binding);
        self.next_binding += 1;
        binding
    }

    fn lookup(&self, name: Symbol) -> Option<BindingId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name).copied())
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Bind(ident, _) => {
                let binding = self.fresh_binding();
                self.declarations.insert(span_key(pattern.span), binding);
                self.declaration_spans.insert(binding, pattern.span);
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(ident.name, binding);
                }
            }
            PatternKind::Paren(inner) | PatternKind::Some(inner) => self.bind_pattern(inner),
            PatternKind::Array(items) => {
                for item in items {
                    if let xc_ast::pattern::ArrayPatternItem::Item(pattern) = item {
                        self.bind_pattern(pattern);
                    }
                }
            }
            PatternKind::Struct(items) => {
                for item in items {
                    if let xc_ast::pattern::StructPatternItem::Ordinal(pattern)
                    | xc_ast::pattern::StructPatternItem::Named(_, pattern) = item
                    {
                        self.bind_pattern(pattern);
                    }
                }
            }
            PatternKind::Wildcard | PatternKind::Expr(_) => {}
        }
    }

    fn walk_block(&mut self, block: &Block, new_scope: bool) {
        if new_scope {
            self.scopes.push(HashMap::new());
        }
        for statement in &block.stmts {
            self.walk_stmt(statement);
        }
        if new_scope {
            self.scopes.pop();
        }
    }

    fn walk_stmt(&mut self, statement: &Stmt) {
        match &statement.kind {
            StmtKind::Expr(expression) | StmtKind::ExprSemi(expression) => {
                self.walk_expr(expression)
            }
            StmtKind::Let(pattern, initializer) => {
                if let Some(initializer) = initializer {
                    self.walk_expr(initializer);
                }
                self.bind_pattern(pattern);
            }
            StmtKind::Defer(block) => self.walk_block(block, true),
            StmtKind::Empty => {}
        }
    }

    fn walk_args(&mut self, arguments: &[Argument]) {
        for argument in arguments {
            match argument {
                Argument::Ordinal(expression)
                | Argument::Named(_, expression)
                | Argument::Expansion(expression) => self.walk_expr(expression),
            }
        }
    }

    fn walk_expr(&mut self, expression: &Expr) {
        match &expression.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                if let Some(binding) = self.lookup(path.segments[0].ident.name) {
                    self.paths.insert(span_key(expression.span), binding);
                    self.last_uses
                        .entry(binding)
                        .and_modify(|last| *last = (*last).max(expression.span.hi.0))
                        .or_insert(expression.span.hi.0);
                }
            }
            ExprKind::Paren(inner)
            | ExprKind::Suffix(_, inner)
            | ExprKind::Await(inner)
            | ExprKind::Cast(inner, _, _)
            | ExprKind::Is(inner, _)
            | ExprKind::NotIs(inner, _) => self.walk_expr(inner),
            ExprKind::Prefix(_, inner)
            | ExprKind::MemberAccess(inner, _)
            | ExprKind::MemberDynamicAccess(inner, _) => self.walk_expr(inner),
            ExprKind::Subscript(base, arguments) | ExprKind::FuncCall(base, arguments, _) => {
                self.walk_expr(base);
                self.walk_args(&arguments.0);
            }
            ExprKind::MethodCall(call) => {
                self.walk_expr(&call.receiver);
                self.walk_args(&call.args.0);
            }
            ExprKind::Array(items) => {
                for item in items {
                    if let ExprItem::Expr(expression) | ExprItem::Expansion(expression) = item {
                        self.walk_expr(expression);
                    }
                }
            }
            ExprKind::Struct(items) => {
                for item in items {
                    match item {
                        StructItem::Ordinal(expression)
                        | StructItem::Named(_, expression)
                        | StructItem::Expansion(expression) => self.walk_expr(expression),
                        StructItem::ExpansionPlaceHolder => {}
                    }
                }
            }
            ExprKind::Dict(items) => {
                for item in items {
                    match item {
                        DictItem::KeyValue(key, value) => {
                            self.walk_expr(key);
                            self.walk_expr(value);
                        }
                        DictItem::Expansion(expression) => self.walk_expr(expression),
                    }
                }
            }
            ExprKind::Block(block) => self.walk_block(block, true),
            ExprKind::If(condition, then, else_) => {
                self.walk_expr(condition);
                self.walk_expr(then);
                if let Some(else_) = else_ {
                    self.walk_expr(else_);
                }
            }
            ExprKind::While(condition, body, else_, _) => {
                self.walk_expr(condition);
                self.walk_expr(body);
                if let Some(else_) = else_ {
                    self.walk_expr(else_);
                }
            }
            ExprKind::For(pattern, iter, body, else_, _, _) => {
                self.walk_expr(iter);
                self.scopes.push(HashMap::new());
                self.bind_pattern(pattern);
                self.walk_block(body, false);
                if let Some(else_) = else_ {
                    self.walk_block(else_, false);
                }
                self.scopes.pop();
            }
            ExprKind::Infix(_, expressions) | ExprKind::Comparison(_, expressions) => {
                for expression in expressions {
                    self.walk_expr(expression);
                }
            }
            ExprKind::Return(value) | ExprKind::Throw(value) | ExprKind::Break(value, _) => {
                if let Some(value) = value {
                    self.walk_expr(value);
                }
            }
            ExprKind::Try(value, catchers, finally) => {
                self.walk_expr(value);
                for catcher in catchers {
                    self.scopes.push(HashMap::new());
                    self.bind_pattern(&catcher.pattern);
                    for statement in &catcher.stmts {
                        self.walk_stmt(statement);
                    }
                    self.scopes.pop();
                }
                if let Some(finally) = finally {
                    self.walk_block(finally, true);
                }
            }
            ExprKind::Lambda(lambda) => {
                self.scopes.push(HashMap::new());
                for param in &lambda.params {
                    let binding = self.fresh_binding();
                    self.declarations
                        .insert(span_key(param.ident.span), binding);
                    self.declaration_spans.insert(binding, param.ident.span);
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(param.ident.name, binding);
                }
                match &lambda.body {
                    FuncBody::Block(block) => self.walk_block(block, true),
                    FuncBody::Arrow(expression) => self.walk_expr(expression),
                    FuncBody::Semicolon => {}
                }
                self.scopes.pop();
            }
            ExprKind::Match(value, arms) => {
                self.walk_expr(value);
                for arm in arms {
                    self.scopes.push(HashMap::new());
                    self.bind_pattern(&arm.pat);
                    if let Some(guard) = &arm.guard {
                        self.walk_expr(guard);
                    }
                    if let Some(body) = &arm.body {
                        self.walk_expr(body);
                    }
                    self.scopes.pop();
                }
            }
            ExprKind::Let(pattern, value) => {
                self.walk_expr(value);
                self.bind_pattern(pattern);
            }
            ExprKind::Path(_)
            | ExprKind::Literal(_)
            | ExprKind::Nil
            | ExprKind::This
            | ExprKind::SelfExpr
            | ExprKind::MacroCall(..)
            | ExprKind::Placeholder
            | ExprKind::Caret
            | ExprKind::Dollar
            | ExprKind::LambdaArgOrdinal(_)
            | ExprKind::LambdaArgNamed(_)
            | ExprKind::AnonEnumerator(_)
            | ExprKind::Continue(_)
            | ExprKind::Error(_) => {}
        }
    }
}

fn collect_global_bindings(program: &Program) -> HashMap<Symbol, BindingId> {
    let mut globals = HashMap::new();
    for statement in &program.top {
        if let StmtKind::Let(pattern, _) = &statement.kind
            && let PatternKind::Bind(ident, _) = &pattern.kind
        {
            let binding = BindingId(globals.len() as u32);
            globals.insert(ident.name, binding);
        }
    }
    globals
}

fn global_reference_bindings(
    program: &Program,
    type_facts: &ProgramTypeFacts,
    globals: &HashMap<Symbol, BindingId>,
) -> HashMap<BindingId, LoanOrigin> {
    let mut references = HashMap::new();
    for statement in &program.top {
        if let StmtKind::Let(pattern, Some(initializer)) = &statement.kind
            && let PatternKind::Bind(ident, _) = &pattern.kind
            && type_facts
                .expression_type(initializer.span)
                .is_some_and(|ty| matches!(ty, Ty::Reference { .. }))
            && let Some(binding) = globals.get(&ident.name)
        {
            references.insert(
                *binding,
                LoanOrigin {
                    place: PlacePath {
                        root: PlaceRoot::Unknown,
                        projections: Vec::new(),
                    },
                    span: initializer.span,
                },
            );
        }
    }
    references
}

#[cfg(test)]
mod tests {
    use super::*;
    use xc_span::create_session_globals_then;

    fn field(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    fn path(root: u32, projections: Vec<Projection>) -> PlacePath {
        PlacePath {
            root: PlaceRoot::Binding(BindingId(root)),
            projections,
        }
    }

    fn temporary(id: u32, projections: Vec<Projection>) -> PlacePath {
        PlacePath {
            root: PlaceRoot::Temporary(id),
            projections,
        }
    }

    #[test]
    fn place_paths_overlap_according_to_projection_rules() {
        create_session_globals_then(|| {
            assert!(path(0, vec![]).overlaps(&path(0, vec![])));
            assert!(path(0, vec![]).overlaps(&path(0, vec![Projection::Field(field("name"))])));
            assert!(
                !path(0, vec![Projection::Field(field("left"))])
                    .overlaps(&path(0, vec![Projection::Field(field("right"))]))
            );
            assert!(
                path(0, vec![Projection::Index(IndexKey::Int(1))])
                    .overlaps(&path(0, vec![Projection::Index(IndexKey::Int(1))]))
            );
            assert!(
                !path(0, vec![Projection::Index(IndexKey::Int(0))])
                    .overlaps(&path(0, vec![Projection::Index(IndexKey::Int(1))]))
            );
            assert!(
                path(0, vec![Projection::Index(IndexKey::Unknown)])
                    .overlaps(&path(0, vec![Projection::Index(IndexKey::Int(1))]))
            );
            assert!(!path(0, vec![]).overlaps(&temporary(0, vec![])));
            assert!(!temporary(0, vec![]).overlaps(&temporary(1, vec![])));
        });
    }
}
