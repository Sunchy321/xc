use std::collections::{HashMap, HashSet};

use xc_ast::{
    expr::{Argument, Expr, ExprItem, ExprKind, ForLoopKind},
    func::{FuncBody, FuncReturn},
    literal::LiteralKind,
    pattern::{Pattern, PatternAssertion, PatternKind},
    ptr::P,
    stmt::{Block, Stmt, StmtKind},
};
use xc_span::{Span, Symbol};

use crate::{
    check::Program,
    types::{Ty, common_type, from_ast, is_subtype},
};

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Ty>,
    ret: Ty,
    decl_offset: u32,
}

#[derive(Clone)]
struct TypeConstraint {
    actual: Ty,
    expected: Ty,
    span: Span,
    context: &'static str,
}

#[derive(Clone, Copy)]
enum ArrayAccess {
    Read,
    Write,
}

#[derive(Clone)]
struct ArrayConstraint {
    array: Ty,
    element: Ty,
    span: Span,
    context: &'static str,
    index_type: Option<Ty>,
    access: ArrayAccess,
}

struct Checker<'a> {
    program: &'a Program,
    signatures: HashMap<Symbol, FunctionSig>,
    scopes: Vec<HashMap<Symbol, Ty>>,
    next_var: u32,
    solutions: HashMap<u32, Ty>,
    returns: HashMap<u32, Vec<Ty>>,
    type_constraints: Vec<TypeConstraint>,
    array_constraints: Vec<ArrayConstraint>,
    return_declarations: HashMap<u32, (Symbol, u32)>,
    loop_results: Vec<Vec<Ty>>,
    current_return: Option<Ty>,
}

pub(crate) fn infer_program(program: &Program) -> Result<(), String> {
    let mut checker = Checker::new(program);
    checker.collect_signatures()?;
    checker.collect_globals()?;
    checker.check_top_level()?;
    checker.check_functions()?;
    checker.solve_returns()
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            signatures: HashMap::new(),
            scopes: vec![HashMap::new()],
            next_var: 0,
            solutions: HashMap::new(),
            returns: HashMap::new(),
            type_constraints: Vec::new(),
            array_constraints: Vec::new(),
            return_declarations: HashMap::new(),
            loop_results: Vec::new(),
            current_return: None,
        }
    }

    fn new_var(&mut self) -> u32 {
        let id = self.next_var;
        self.next_var += 1;
        id
    }

    fn collect_signatures(&mut self) -> Result<(), String> {
        for function in &self.program.funcs {
            let params = function
                .sig
                .params
                .iter()
                .map(|param| from_ast(&param.ty))
                .collect();
            let ret = match &function.sig.tail.ret {
                FuncReturn::Inferred => Ty::Var(self.new_var()),
                FuncReturn::Type(ty) => from_ast(ty),
            };
            let decl_offset = function.ident.span.lo.0;
            if let Ty::Var(id) = &ret {
                self.return_declarations
                    .insert(*id, (function.ident.name, decl_offset));
            }
            self.signatures.insert(
                function.ident.name,
                FunctionSig {
                    params,
                    ret,
                    decl_offset,
                },
            );
        }
        Ok(())
    }

    fn collect_globals(&mut self) -> Result<(), String> {
        for statement in &self.program.top {
            if let StmtKind::Let(pattern, _) = &statement.kind {
                if let PatternKind::Bind(ident, _) = &pattern.kind {
                    let ty = self.pattern_type(pattern).unwrap_or(Ty::Unknown);
                    self.scopes[0].insert(ident.name, ty);
                }
            }
        }
        Ok(())
    }

    fn check_top_level(&mut self) -> Result<(), String> {
        for statement in &self.program.top {
            self.infer_stmt(statement)?;
        }
        Ok(())
    }

    fn check_functions(&mut self) -> Result<(), String> {
        for function in &self.program.funcs {
            let signature = self.signatures[&function.ident.name].clone();
            self.push_scope();
            for (param, ty) in function.sig.params.iter().zip(&signature.params) {
                self.insert(param.ident.name, ty.clone());
            }
            let previous_return = self.current_return.replace(signature.ret.clone());
            let body_expected = match &signature.ret {
                Ty::Var(_) => None,
                ty => Some(ty),
            };
            let body_ty = match &function.body {
                FuncBody::Block(block) => self.infer_block(block, body_expected)?,
                FuncBody::Arrow(expression) => self.infer_expr(expression, body_expected)?,
                FuncBody::Semicolon => {
                    return Err(format!(
                        "function without body: {}",
                        function.ident.as_str()
                    ));
                }
            };
            self.current_return = previous_return;
            match signature.ret {
                Ty::Var(id) => self.returns.entry(id).or_default().push(body_ty),
                expected => {
                    self.require_type(
                        body_ty,
                        &expected,
                        function.body.body_span(),
                        "function body type mismatch",
                    )?;
                }
            }
            self.pop_scope();
        }
        Ok(())
    }

    fn solve_returns(&mut self) -> Result<(), String> {
        loop {
            let before = self.solutions.clone();
            for (id, values) in self.returns.clone() {
                let mut alternatives = Vec::new();
                let mut has_unknown = false;
                for value in values {
                    collect_return_candidates(
                        self.resolve(value),
                        id,
                        &mut alternatives,
                        &mut has_unknown,
                    );
                }
                let candidate = if has_unknown {
                    Some(Ty::Unknown)
                } else {
                    alternatives.into_iter().reduce(common_type)
                };
                if let Some(candidate) = candidate {
                    self.set_return_solution(id, candidate)?;
                }
            }
            self.solve_array_constraints(false)?;
            if self.solutions == before {
                break;
            }
        }

        self.solve_array_constraints(true)?;
        self.reject_unresolved_returns()?;
        for constraint in self.type_constraints.clone() {
            let actual = self.resolve(constraint.actual);
            let expected = self.resolve(constraint.expected);
            if contains_unknown(&actual) || contains_unknown(&expected) {
                continue;
            }
            if let Some(unresolved) = first_var(&actual).or_else(|| first_var(&expected)) {
                return Err(self.cannot_infer_return(unresolved));
            }
            if !is_subtype(&actual, &expected) {
                return Err(format!(
                    "{}: expected {expected}, got {actual} at byte {}",
                    constraint.context, constraint.span.lo.0
                ));
            }
        }
        Ok(())
    }

    fn solve_array_constraints(&mut self, final_pass: bool) -> Result<(), String> {
        for constraint in self.array_constraints.clone() {
            let array = self.resolve(constraint.array);
            let element = constraint.element.clone();
            let (element_type, check_index) = match array {
                Ty::Array(array_element) => (Some(*array_element), true),
                union @ Ty::Union(_) if contains_unknown(&union) => (Some(Ty::Unknown), false),
                Ty::Union(members) => {
                    let actual = Ty::Union(members.clone());
                    let elements = members
                        .into_iter()
                        .map(|member| match member {
                            Ty::Array(element) => Some(*element),
                            _ => None,
                        })
                        .collect::<Option<Vec<_>>>();
                    match elements {
                        Some(elements) => {
                            let element = match constraint.access {
                                ArrayAccess::Read => elements.into_iter().reduce(common_type),
                                ArrayAccess::Write => {
                                    let first = elements.first().cloned();
                                    first
                                        .filter(|first| elements.iter().all(|ty| ty == first))
                                        .or(Some(Ty::Never))
                                }
                            };
                            (element, true)
                        }
                        None if final_pass => {
                            return Err(format!(
                                "{}: expected array, got {actual} at byte {}",
                                constraint.context, constraint.span.lo.0
                            ));
                        }
                        None => (None, false),
                    }
                }
                Ty::Unknown => (Some(Ty::Unknown), false),
                Ty::Never => (Some(Ty::Never), false),
                Ty::Var(_) => (None, false),
                other => {
                    if final_pass {
                        return Err(format!(
                            "{}: expected array, got {other} at byte {}",
                            constraint.context, constraint.span.lo.0
                        ));
                    }
                    (None, false)
                }
            };
            if let Some(element_type) = element_type {
                self.merge_inferred_type(element, element_type, constraint.span)?;
                if final_pass
                    && check_index
                    && let Some(index_type) = constraint.index_type
                {
                    self.require_type(
                        index_type,
                        &Ty::Int,
                        constraint.span,
                        "array index must be int",
                    )?;
                }
            }
        }
        Ok(())
    }

    fn merge_inferred_type(
        &mut self,
        variable: Ty,
        candidate: Ty,
        span: Span,
    ) -> Result<(), String> {
        let Ty::Var(id) = variable else {
            return self.unify(variable, candidate, span);
        };
        let candidate = remove_union_self_variable(self.resolve(candidate), id);
        let previous = self.resolve(Ty::Var(id));
        if previous == candidate {
            return Ok(());
        }
        let merged = if previous == Ty::Var(id) {
            candidate
        } else {
            common_type(previous, candidate)
        };
        if contains_var(&merged, id) {
            return Err(format!(
                "cannot infer recursive array element type at byte {}",
                span.lo.0
            ));
        }
        self.solutions.insert(id, merged);
        Ok(())
    }

    fn unify(&mut self, left: Ty, right: Ty, span: Span) -> Result<(), String> {
        let left = self.resolve(left);
        let right = self.resolve(right);
        if left == right {
            return Ok(());
        }
        match (left, right) {
            (Ty::Var(id), Ty::Unknown) | (Ty::Unknown, Ty::Var(id)) => {
                self.solutions.insert(id, Ty::Unknown);
                Ok(())
            }
            (Ty::Unknown, _) | (_, Ty::Unknown) => Ok(()),
            (Ty::Var(id), ty) | (ty, Ty::Var(id)) => {
                if contains_var(&ty, id) {
                    return Err(format!(
                        "cannot infer recursive array element type at byte {}",
                        span.lo.0
                    ));
                }
                self.solutions.insert(id, ty);
                Ok(())
            }
            (Ty::Array(left), Ty::Array(right)) => self.unify(*left, *right, span),
            (
                Ty::Function {
                    params: left_params,
                    ret: left_ret,
                },
                Ty::Function {
                    params: right_params,
                    ret: right_ret,
                },
            ) if left_params.len() == right_params.len() => {
                for (left, right) in left_params.into_iter().zip(right_params) {
                    self.unify(left, right, span)?;
                }
                self.unify(*left_ret, *right_ret, span)
            }
            (left, right) => Err(format!(
                "array element type mismatch: expected {left}, got {right} at byte {}",
                span.lo.0
            )),
        }
    }

    fn set_return_solution(&mut self, id: u32, candidate: Ty) -> Result<(), String> {
        let candidate = self.resolve(candidate);
        let merged = match self.solutions.get(&id).cloned() {
            Some(previous) => common_type(self.resolve(previous), candidate),
            None => candidate,
        };
        let merged = self.resolve(merged);
        if contains_var(&merged, id) {
            return Err(self.cannot_infer_return(id));
        }
        if self.solutions.get(&id) != Some(&merged) {
            self.solutions.insert(id, merged);
        }
        Ok(())
    }

    fn reject_unresolved_returns(&self) -> Result<(), String> {
        for id in self.return_declarations.keys().copied() {
            let resolved = self.resolve(Ty::Var(id));
            if first_var(&resolved).is_some() {
                return Err(self.cannot_infer_return(id));
            }
        }
        Ok(())
    }

    fn cannot_infer_return(&self, id: u32) -> String {
        match self.return_declarations.get(&id) {
            Some((name, recorded_offset)) => {
                let offset = self
                    .signatures
                    .get(name)
                    .map_or(*recorded_offset, |signature| signature.decl_offset);
                format!(
                    "cannot infer function return type {} at byte {offset}",
                    name.as_str()
                )
            }
            None => format!("cannot infer function return type ?{id}"),
        }
    }

    fn infer_stmt(&mut self, statement: &Stmt) -> Result<Ty, String> {
        match &statement.kind {
            StmtKind::Expr(expression) => self.infer_expr(expression, None),
            StmtKind::ExprSemi(expression) => {
                let ty = self.infer_expr(expression, None)?;
                Ok(if ty == Ty::Never { Ty::Never } else { Ty::Void })
            }
            StmtKind::Let(pattern, initializer) => {
                let asserted = self.pattern_type(pattern);
                let value_ty = match initializer {
                    Some(expression) => self.infer_expr(expression, asserted.as_ref())?,
                    None => asserted.clone().unwrap_or(Ty::Unknown),
                };
                let binding_ty = if let Some(expected) = asserted {
                    self.require_type(value_ty, &expected, pattern.span, "binding type mismatch")?;
                    expected
                } else {
                    value_ty
                };
                if let PatternKind::Bind(ident, _) = &pattern.kind {
                    self.insert(ident.name, binding_ty);
                }
                Ok(Ty::Void)
            }
            StmtKind::Defer(block) => {
                self.infer_block(block, None)?;
                Ok(Ty::Void)
            }
            StmtKind::Empty => Ok(Ty::Void),
        }
    }

    fn infer_block(&mut self, block: &Block, expected: Option<&Ty>) -> Result<Ty, String> {
        self.push_scope();
        let mut result = Ty::Void;
        let mut terminated = false;
        for (index, statement) in block.stmts.iter().enumerate() {
            let ty = if index + 1 == block.stmts.len()
                && let StmtKind::Expr(expression) = &statement.kind
            {
                self.infer_expr(expression, expected)?
            } else {
                self.infer_stmt(statement)?
            };
            if ty == Ty::Never {
                result = Ty::Never;
                terminated = true;
            } else if !terminated {
                if index + 1 == block.stmts.len() {
                    result = if matches!(statement.kind, StmtKind::Expr(_)) {
                        ty
                    } else {
                        Ty::Void
                    };
                } else {
                    result = Ty::Void;
                }
            }
        }
        self.pop_scope();
        Ok(result)
    }

    fn infer_expr(&mut self, expression: &Expr, expected: Option<&Ty>) -> Result<Ty, String> {
        let inferred = self.infer_expr_inner(expression, expected)?;
        if let Some(expected) = expected {
            self.require_type(inferred.clone(), expected, expression.span, "type mismatch")?;
        }
        Ok(inferred)
    }

    fn infer_expr_inner(&mut self, expression: &Expr, expected: Option<&Ty>) -> Result<Ty, String> {
        match &expression.kind {
            ExprKind::Literal(literal) => Ok(match literal.kind {
                LiteralKind::Integer => Ty::Int,
                LiteralKind::Bool => Ty::Bool,
                LiteralKind::String | LiteralKind::RawString { .. } => Ty::String,
                _ => Ty::Unknown,
            }),
            ExprKind::Path(path) if path.segments.len() == 1 => {
                let name = path.segments[0].ident.name;
                if let Some(ty) = self.lookup(name) {
                    Ok(ty)
                } else if let Some(signature) = self.signatures.get(&name) {
                    Ok(Ty::Function {
                        params: signature.params.clone(),
                        ret: Box::new(signature.ret.clone()),
                    })
                } else {
                    Ok(Ty::Unknown)
                }
            }
            ExprKind::Paren(inner) => self.infer_expr(inner, expected),
            ExprKind::Array(items) => self.infer_array(items, expected, expression.span),
            ExprKind::Block(block) => self.infer_block(block, expected),
            ExprKind::If(condition, then, else_) => {
                self.infer_if(condition, then, else_.as_deref(), expected)
            }
            ExprKind::For(pattern, iter, body, else_, kind, label) => {
                self.infer_for(pattern, iter, body, else_.as_deref(), *kind, *label)
            }
            ExprKind::While(condition, body, else_, label) => {
                self.infer_while(condition, body, else_.as_deref(), *label)
            }
            ExprKind::Infix(operator, operands) => {
                self.infer_infix(*operator, operands.as_slice(), expression.span)
            }
            ExprKind::Comparison(operators, operands) => {
                self.infer_comparison(operators, operands.as_slice())
            }
            ExprKind::Subscript(base, arguments) => self.infer_subscript(
                base,
                arguments.0.as_slice(),
                expression.span,
                ArrayAccess::Read,
            ),
            ExprKind::FuncCall(callee, arguments, _) => {
                self.infer_call(callee, arguments.0.as_slice())
            }
            ExprKind::Prefix(operator, operand) => {
                let ty = self.infer_expr(operand, None)?;
                match *operator {
                    xc_span::symbol::op::Plus | xc_span::symbol::op::Minus => {
                        self.require_type(
                            ty,
                            &Ty::Int,
                            operand.span,
                            "arithmetic operands must be int",
                        )?;
                        Ok(Ty::Int)
                    }
                    _ => Ok(Ty::Unknown),
                }
            }
            ExprKind::Return(value) => {
                let expected_return = match self.current_return.clone() {
                    Some(Ty::Var(_)) | None => None,
                    Some(ty) => Some(ty),
                };
                let value_ty = match value {
                    Some(value) => self.infer_expr(value, expected_return.as_ref())?,
                    None => Ty::Void,
                };
                if let Some(ret) = self.current_return.clone() {
                    match ret {
                        Ty::Var(id) => self.returns.entry(id).or_default().push(value_ty),
                        expected => self.require_type(
                            value_ty,
                            &expected,
                            expression.span,
                            "return type mismatch",
                        )?,
                    }
                }
                Ok(Ty::Never)
            }
            ExprKind::Break(value, label) => {
                if label.is_some() {
                    return Ok(Ty::Unknown);
                }
                let value_ty = match value {
                    Some(value) => self.infer_expr(value, None)?,
                    None => Ty::Void,
                };
                if let Some(results) = self.loop_results.last_mut() {
                    results.push(value_ty);
                    Ok(Ty::Never)
                } else {
                    Ok(Ty::Unknown)
                }
            }
            ExprKind::Continue(label) => {
                if label.is_some() || self.loop_results.is_empty() {
                    Ok(Ty::Unknown)
                } else {
                    Ok(Ty::Never)
                }
            }
            _ => Ok(Ty::Unknown),
        }
    }

    fn infer_call(&mut self, callee: &Expr, arguments: &[Argument]) -> Result<Ty, String> {
        if let ExprKind::Path(path) = &callee.kind {
            if path.segments.len() == 1 {
                let name = path.segments[0].ident.name;
                match name.as_str() {
                    "print" => {
                        for argument in arguments {
                            if let Argument::Ordinal(expression) = argument {
                                self.infer_expr(expression, None)?;
                            }
                        }
                        return Ok(Ty::Void);
                    }
                    "panic" => {
                        if arguments.len() != 1 {
                            return Err(format!(
                                "panic expects 1 argument, got {}",
                                arguments.len()
                            ));
                        }
                        match arguments.first() {
                            Some(
                                Argument::Ordinal(expression) | Argument::Named(_, expression),
                            ) => {
                                self.infer_expr(expression, Some(&Ty::String))?;
                            }
                            Some(Argument::Expansion(expression)) => {
                                self.infer_expr(expression, None)?;
                                return Ok(Ty::Unknown);
                            }
                            None => unreachable!("argument count checked above"),
                        }
                        return Ok(Ty::Never);
                    }
                    "spawn" => {
                        for argument in arguments {
                            if let Argument::Ordinal(expression) = argument {
                                self.infer_expr(expression, None)?;
                            }
                        }
                        return Ok(Ty::Unknown);
                    }
                    _ => {}
                }
                if self.lookup(name).is_none()
                    && let Some(signature) = self.signatures.get(&name).cloned()
                {
                    if arguments.len() != signature.params.len() {
                        return Err(format!(
                            "function {} expects {} arguments, got {}",
                            name.as_str(),
                            signature.params.len(),
                            arguments.len()
                        ));
                    }
                    for (argument, expected) in arguments.iter().zip(&signature.params) {
                        if let Argument::Ordinal(expression) = argument {
                            self.infer_expr(expression, Some(expected))?;
                        }
                    }
                    return Ok(signature.ret);
                }
            }
        }
        self.infer_expr(callee, None)?;
        for argument in arguments {
            if let Argument::Ordinal(expression) = argument {
                self.infer_expr(expression, None)?;
            }
        }
        Ok(Ty::Unknown)
    }

    fn infer_array(
        &mut self,
        items: &[ExprItem],
        expected: Option<&Ty>,
        span: Span,
    ) -> Result<Ty, String> {
        let expected_element = match expected {
            Some(Ty::Array(element)) => Some(element.as_ref()),
            _ => None,
        };
        let mut element_type: Option<Ty> = None;
        for item in items {
            match item {
                ExprItem::Expr(expression) => {
                    let inferred = self.infer_expr(expression, expected_element)?;
                    if let Some(expected) = expected_element {
                        self.require_type(
                            inferred,
                            expected,
                            expression.span,
                            "array element type mismatch",
                        )?;
                    } else {
                        element_type = Some(match element_type {
                            Some(previous) => common_type(previous, inferred),
                            None => inferred,
                        });
                    }
                }
                ExprItem::Expansion(_) | ExprItem::ExpansionPlaceHolder => {
                    return Ok(Ty::Unknown);
                }
            }
        }
        let element = match element_type.or_else(|| expected_element.cloned()) {
            Some(element) => element,
            None if items.is_empty() => match expected {
                Some(expected @ Ty::Var(_)) => {
                    let element = Ty::Var(self.new_var());
                    self.array_constraints.push(ArrayConstraint {
                        array: expected.clone(),
                        element: element.clone(),
                        span,
                        context: "array literal type context",
                        index_type: None,
                        access: ArrayAccess::Read,
                    });
                    element
                }
                _ => {
                    return Err(format!(
                        "cannot infer empty array element type at byte {}",
                        span.lo.0
                    ));
                }
            },
            None => {
                return Err(format!(
                    "cannot infer empty array element type at byte {}",
                    span.lo.0
                ));
            }
        };
        if matches!(&element, Ty::Unknown) {
            Ok(Ty::Unknown)
        } else {
            Ok(Ty::Array(Box::new(element)))
        }
    }

    fn infer_infix(
        &mut self,
        operator: Symbol,
        operands: &[P<Expr>],
        span: Span,
    ) -> Result<Ty, String> {
        use xc_span::symbol::op;
        match operator {
            op::Plus | op::Minus | op::Multiply | op::Divide | op::Modulo => {
                for operand in operands {
                    let ty = self.infer_expr(operand, None)?;
                    self.require_type(
                        ty,
                        &Ty::Int,
                        operand.span,
                        "arithmetic operands must be int",
                    )?;
                }
                Ok(Ty::Int)
            }
            op::Concat => {
                for operand in operands {
                    let ty = self.infer_expr(operand, None)?;
                    self.require_type(
                        ty,
                        &Ty::String,
                        operand.span,
                        "concat operands must be string",
                    )?;
                }
                Ok(Ty::String)
            }
            op::AndAnd | op::OrOr => {
                for operand in operands {
                    let ty = self.infer_expr(operand, None)?;
                    self.require_type(
                        ty,
                        &Ty::Bool,
                        operand.span,
                        "logical operands must be bool",
                    )?;
                }
                Ok(Ty::Bool)
            }
            op::Assign => self.infer_assignment(operands, span),
            op::Equal | op::NotEqual => {
                for operand in operands {
                    self.infer_expr(operand, None)?;
                }
                Ok(Ty::Bool)
            }
            _ => {
                for operand in operands {
                    self.infer_expr(operand, None)?;
                }
                Ok(Ty::Unknown)
            }
        }
    }

    fn infer_assignment(&mut self, operands: &[P<Expr>], span: Span) -> Result<Ty, String> {
        if operands.len() != 2 {
            for operand in operands {
                self.infer_expr(operand, None)?;
            }
            return Ok(Ty::Unknown);
        }
        let place = &operands[0];
        let value = &operands[1];
        match &place.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                let name = path.segments[0].ident.name;
                if let Some(expected) = self.lookup(name) {
                    self.infer_expr(value, Some(&expected))?;
                    Ok(Ty::Void)
                } else {
                    self.infer_expr(value, None)?;
                    Ok(Ty::Unknown)
                }
            }
            ExprKind::Subscript(base, arguments) => {
                let target = self.infer_subscript(
                    base,
                    arguments.0.as_slice(),
                    place.span,
                    ArrayAccess::Write,
                )?;
                if matches!(target, Ty::Unknown) {
                    self.infer_expr(value, None)?;
                    Ok(Ty::Unknown)
                } else {
                    self.infer_expr(value, Some(&target))?;
                    Ok(Ty::Void)
                }
            }
            _ => {
                self.infer_expr(place, None)?;
                self.infer_expr(value, None)?;
                let _ = span;
                Ok(Ty::Unknown)
            }
        }
    }

    fn infer_comparison(
        &mut self,
        operators: &[Symbol],
        operands: &[P<Expr>],
    ) -> Result<Ty, String> {
        use xc_span::symbol::op;
        let types = operands
            .iter()
            .map(|operand| self.infer_expr(operand, None))
            .collect::<Result<Vec<_>, _>>()?;
        if operands.len() != operators.len() + 1 {
            return Ok(Ty::Unknown);
        }
        for (index, operator) in operators.iter().enumerate() {
            match *operator {
                op::Equal | op::NotEqual => {}
                op::Less | op::LessEqual | op::Greater | op::GreaterEqual => {
                    for operand_index in [index, index + 1] {
                        self.require_type(
                            types[operand_index].clone(),
                            &Ty::Int,
                            operands[operand_index].span,
                            "comparison operands must be int",
                        )?;
                    }
                }
                _ => return Ok(Ty::Unknown),
            }
        }
        Ok(Ty::Bool)
    }

    fn infer_subscript(
        &mut self,
        base: &Expr,
        arguments: &[Argument],
        span: Span,
        access: ArrayAccess,
    ) -> Result<Ty, String> {
        let base_ty = self.infer_expr(base, None)?;
        if matches!(base_ty, Ty::Unknown) {
            for argument in arguments {
                if let Argument::Ordinal(expression) = argument {
                    self.infer_expr(expression, None)?;
                }
            }
            return Ok(Ty::Unknown);
        }
        if arguments.len() != 1 {
            for argument in arguments {
                if let Argument::Ordinal(expression) = argument {
                    self.infer_expr(expression, None)?;
                }
            }
            return Ok(Ty::Unknown);
        }
        let Some(Argument::Ordinal(index)) = arguments.first() else {
            return Ok(Ty::Unknown);
        };
        match base_ty {
            Ty::Array(element) => {
                self.infer_expr(index, Some(&Ty::Int))?;
                Ok(*element)
            }
            array @ Ty::Var(_) => {
                let index_type = self.infer_expr(index, None)?;
                let element = Ty::Var(self.new_var());
                self.array_constraints.push(ArrayConstraint {
                    array,
                    element: element.clone(),
                    span,
                    context: "array subscript",
                    index_type: Some(index_type),
                    access,
                });
                Ok(element)
            }
            Ty::Never => Ok(Ty::Never),
            other => Err(format!(
                "array subscript: expected array, got {other} at byte {}",
                span.lo.0
            )),
        }
    }

    fn infer_if(
        &mut self,
        condition: &Expr,
        then: &Expr,
        else_: Option<&Expr>,
        expected: Option<&Ty>,
    ) -> Result<Ty, String> {
        let condition_ty = self.infer_expr(condition, None)?;
        self.require_type(
            condition_ty,
            &Ty::Bool,
            condition.span,
            "if condition must be bool",
        )?;
        let then_ty = self.infer_expr(then, expected)?;
        match else_ {
            Some(other) => {
                let else_ty = self.infer_expr(other, expected)?;
                Ok(common_type(then_ty, else_ty))
            }
            None => Ok(Ty::Void),
        }
    }

    fn infer_for(
        &mut self,
        pattern: &Pattern,
        iter: &Expr,
        body: &Block,
        else_: Option<&Block>,
        kind: ForLoopKind,
        label: Option<Symbol>,
    ) -> Result<Ty, String> {
        if kind == ForLoopKind::ForAwait || label.is_some() {
            self.infer_expr(iter, None)?;
            return Ok(Ty::Unknown);
        }
        let iter_ty = self.infer_expr(iter, None)?;
        let element_ty = match iter_ty {
            Ty::Array(element) => *element,
            Ty::Unknown => Ty::Unknown,
            Ty::Never => Ty::Never,
            array @ Ty::Var(_) => {
                let element = Ty::Var(self.new_var());
                self.array_constraints.push(ArrayConstraint {
                    array,
                    element: element.clone(),
                    span: iter.span,
                    context: "array iteration",
                    index_type: None,
                    access: ArrayAccess::Read,
                });
                element
            }
            other => {
                return Err(format!(
                    "array iteration: expected array, got {other} at byte {}",
                    iter.span.lo.0
                ));
            }
        };
        self.push_scope();
        if let PatternKind::Bind(ident, _) = &pattern.kind {
            let binding_ty = if let Some(expected) = self.pattern_type(pattern) {
                self.require_type(
                    element_ty,
                    &expected,
                    pattern.span,
                    "loop binding type mismatch",
                )?;
                expected
            } else {
                element_ty
            };
            self.insert(ident.name, binding_ty);
        }
        self.loop_results.push(Vec::new());
        self.infer_block(body, None)?;
        let breaks = self.loop_results.pop().unwrap_or_default();
        self.pop_scope();
        if let Some(else_block) = else_ {
            let else_ty = self.infer_block(else_block, None)?;
            Ok(breaks
                .into_iter()
                .reduce(common_type)
                .map_or(else_ty.clone(), |break_ty| common_type(break_ty, else_ty)))
        } else {
            Ok(Ty::Void)
        }
    }

    fn infer_while(
        &mut self,
        condition: &Expr,
        body: &Expr,
        else_: Option<&Expr>,
        _label: Option<Symbol>,
    ) -> Result<Ty, String> {
        let condition_ty = self.infer_expr(condition, None)?;
        self.require_type(
            condition_ty,
            &Ty::Bool,
            condition.span,
            "while condition must be bool",
        )?;
        self.loop_results.push(Vec::new());
        self.push_scope();
        self.infer_expr(body, None)?;
        self.pop_scope();
        let breaks = self.loop_results.pop().unwrap_or_default();
        if let Some(else_expression) = else_ {
            let else_ty = self.infer_expr(else_expression, None)?;
            Ok(breaks
                .into_iter()
                .reduce(common_type)
                .map_or(else_ty.clone(), |break_ty| common_type(break_ty, else_ty)))
        } else {
            Ok(Ty::Void)
        }
    }

    fn require_type(
        &mut self,
        actual: Ty,
        expected: &Ty,
        span: Span,
        context: &'static str,
    ) -> Result<(), String> {
        let actual = self.resolve(actual);
        let expected = self.resolve(expected.clone());
        if contains_unknown(&actual) || contains_unknown(&expected) {
            return Ok(());
        }
        if first_var(&actual).is_some() || first_var(&expected).is_some() {
            self.type_constraints.push(TypeConstraint {
                actual,
                expected,
                span,
                context,
            });
            return Ok(());
        }
        if is_subtype(&actual, &expected) {
            Ok(())
        } else {
            Err(format!(
                "{context}: expected {expected}, got {actual} at byte {}",
                span.lo.0
            ))
        }
    }

    fn resolve(&self, ty: Ty) -> Ty {
        self.resolve_with_seen(ty, &mut HashSet::new())
    }

    fn resolve_with_seen(&self, ty: Ty, seen: &mut HashSet<u32>) -> Ty {
        match ty {
            Ty::Var(id) => {
                if !seen.insert(id) {
                    return Ty::Var(id);
                }
                let resolved = self
                    .solutions
                    .get(&id)
                    .cloned()
                    .map(|solution| self.resolve_with_seen(solution, seen))
                    .unwrap_or(Ty::Var(id));
                seen.remove(&id);
                resolved
            }
            Ty::Array(element) => Ty::Array(Box::new(self.resolve_with_seen(*element, seen))),
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .into_iter()
                    .map(|param| self.resolve_with_seen(param, seen))
                    .collect(),
                ret: Box::new(self.resolve_with_seen(*ret, seen)),
            },
            Ty::Union(members) => members
                .into_iter()
                .map(|member| self.resolve_with_seen(member, seen))
                .reduce(common_type)
                .unwrap_or(Ty::Never),
            other => other,
        }
    }

    fn pattern_type(&self, pattern: &Pattern) -> Option<Ty> {
        match &pattern.assert {
            Some(PatternAssertion::Type(ty, _)) => Some(from_ast(ty)),
            _ => None,
        }
    }

    fn lookup(&self, name: Symbol) -> Option<Ty> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name).cloned())
    }

    fn insert(&mut self, name: Symbol, ty: Ty) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
}

fn collect_return_candidates(ty: Ty, current: u32, output: &mut Vec<Ty>, has_unknown: &mut bool) {
    match ty {
        Ty::Union(members) => {
            for member in members {
                collect_return_candidates(member, current, output, has_unknown);
            }
        }
        Ty::Var(id) if id == current => {}
        Ty::Unknown => *has_unknown = true,
        known => output.push(known),
    }
}

fn contains_var(ty: &Ty, needle: u32) -> bool {
    match ty {
        Ty::Var(id) => *id == needle,
        Ty::Array(element) => contains_var(element, needle),
        Ty::Function { params, ret } => {
            params.iter().any(|param| contains_var(param, needle)) || contains_var(ret, needle)
        }
        Ty::Union(members) => members.iter().any(|member| contains_var(member, needle)),
        _ => false,
    }
}

fn remove_union_self_variable(ty: Ty, needle: u32) -> Ty {
    match ty {
        Ty::Union(members) => members
            .into_iter()
            .filter(|member| !matches!(member, Ty::Var(id) if *id == needle))
            .reduce(common_type)
            .unwrap_or(Ty::Never),
        other => other,
    }
}

fn first_var(ty: &Ty) -> Option<u32> {
    match ty {
        Ty::Var(id) => Some(*id),
        Ty::Array(element) => first_var(element),
        Ty::Function { params, ret } => {
            params.iter().find_map(first_var).or_else(|| first_var(ret))
        }
        Ty::Union(members) => members.iter().find_map(first_var),
        _ => None,
    }
}

fn contains_unknown(ty: &Ty) -> bool {
    match ty {
        Ty::Unknown => true,
        Ty::Array(element) => contains_unknown(element),
        Ty::Function { params, ret } => {
            params.iter().any(contains_unknown) || contains_unknown(ret)
        }
        Ty::Union(members) => members.iter().any(contains_unknown),
        _ => false,
    }
}

trait BodySpan {
    fn body_span(&self) -> Span;
}

impl BodySpan for FuncBody {
    fn body_span(&self) -> Span {
        match self {
            FuncBody::Block(block) => block.span,
            FuncBody::Arrow(expression) => expression.span,
            FuncBody::Semicolon => Span::DUMMY,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{check::check, parse_program};
    use xc_span::{create_session_globals_then, source_map::Filename};

    fn check_source(source: &str) -> Result<(), String> {
        create_session_globals_then(|| {
            let parsed = parse_program(Filename::Normal("type-test".into()), source)?;
            check(parsed).map(|_| ())
        })
    }

    #[test]
    fn forward_function_calls_use_collected_signatures() {
        check_source("func caller() { return later(1); } func later(n: int) -> int { n }").unwrap();
    }

    #[test]
    fn direct_call_rejects_wrong_argument_type() {
        let error = check_source("func take(n: int) -> int { n } take(\"bad\");").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
        assert!(error.contains("got string"), "{error}");
    }

    #[test]
    fn functions_can_read_typed_top_level_bindings() {
        check_source("let base = 10; func add(n: int) { return base + n; } add(5);").unwrap();
    }

    #[test]
    fn nested_blocks_shadow_bindings_without_leaking() {
        check_source("let value = 1; { let value = \"text\"; print(value); } print(value);")
            .unwrap();
    }

    #[test]
    fn local_binding_shadows_a_module_function_for_call_inference() {
        check_source(
            "func target(value: int) {} func caller(target: string) { target(\"text\"); }",
        )
        .unwrap();
    }

    #[test]
    fn arithmetic_requires_int_and_preserves_runtime_diagnostic() {
        let error = check_source("\"a\" + \"b\";").unwrap_err();
        assert!(error.contains("arithmetic operands must be int"), "{error}");
        assert!(error.contains("got string"), "{error}");
        let error = check_source("let value = -\"one\";").unwrap_err();
        assert!(error.contains("arithmetic operands must be int"), "{error}");
    }

    #[test]
    fn ordered_comparisons_require_int() {
        let error = check_source("\"one\" < 1;").unwrap_err();
        assert!(error.contains("comparison operands must be int"), "{error}");
        assert!(error.contains("got string"), "{error}");
    }

    #[test]
    fn conditions_require_bool_and_branches_use_common_type() {
        let error = check_source("if 1 { 2 } else { 3 };").unwrap_err();
        assert!(error.contains("expected bool"), "{error}");
        check_source("let value = if true { 1 } else { \"one\" };").unwrap();
        let error = check_source("while 1 {};").unwrap_err();
        assert!(error.contains("expected bool"), "{error}");
    }

    #[test]
    fn array_literals_and_indices_are_checked() {
        check_source("func consume(values: int[]) { print(values[0]); } consume([1, 2]);").unwrap();
        let error = check_source("let values = [1, 2]; values[\"zero\"];").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
    }

    #[test]
    fn assignment_rejects_incompatible_values() {
        let error = check_source("let mut value = 1; value = \"one\";").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
        assert!(error.contains("got string"), "{error}");
        let error = check_source("let mut values = [1, 2]; values[0] = \"one\";").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
    }

    #[test]
    fn boolean_concat_and_never_rules_match_their_signatures() {
        check_source(
            "func take(value: int) {} let enabled = true && false; let text = \"a\" ~ \"b\"; let same = 1 == \"one\"; take(panic(\"stop\"));",
        )
        .unwrap();
        let error = check_source("true && 1;").unwrap_err();
        assert!(error.contains("expected bool"), "{error}");
        let error = check_source("func take(value: int) {} take(1 == \"one\");").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
        assert!(error.contains("got bool"), "{error}");
        let error = check_source("panic(1);").unwrap_err();
        assert!(error.contains("expected string"), "{error}");
        let error = check_source("panic(message: 1);").unwrap_err();
        assert!(error.contains("expected string"), "{error}");
    }

    #[test]
    fn empty_array_requires_an_expected_element_type() {
        check_source("func consume(values: int[]) {} consume([]);").unwrap();
        let error = check_source("let values = [];").unwrap_err();
        assert!(
            error.contains("cannot infer empty array element type"),
            "{error}"
        );
    }

    #[test]
    fn array_loop_binds_the_element_type() {
        check_source("func take_item(item: int) {} for x: [1, 2, 3] { take_item(x); }").unwrap();
        check_source("while true { break; }").unwrap();
        let error =
            check_source("func take_string(item: string) {} for x: [1, 2] { take_string(x); }")
                .unwrap_err();
        assert!(error.contains("expected string"), "{error}");
    }

    #[test]
    fn recursive_fibonacci_return_type_is_inferred_as_int() {
        check_source(
            "func fib(n: int) { if n < 2 { return n; } return fib(n - 1) + fib(n - 2); } print(fib(10));",
        )
        .unwrap();
    }

    #[test]
    fn explicit_return_type_rejects_incompatible_return() {
        let error = check_source("func bad() -> int { return \"bad\"; }").unwrap_err();
        assert!(error.contains("expected int"), "{error}");
        assert!(error.contains("got string"), "{error}");
    }

    #[test]
    fn inferred_returns_use_the_common_type() {
        let error = check_source(
            "func choose(flag: bool) { if flag { return 1; } return \"one\"; } func take(value: bool) {} take(choose(true));",
        )
        .unwrap_err();
        assert!(error.contains("expected bool"), "{error}");
        assert!(error.contains("int | string"), "{error}");
    }

    #[test]
    fn recursive_return_type_inside_array_is_rejected() {
        let error =
            check_source("func recursive_array() { return [recursive_array()]; }").unwrap_err();
        assert!(
            error.contains("cannot infer function return type"),
            "{error}"
        );
        assert!(error.contains("recursive_array"), "{error}");
    }

    #[test]
    fn mutually_recursive_returns_resolve_from_a_base_case() {
        let error = check_source(
            "func left(n: int) { if n < 1 { return 1; } return right(n - 1); } func right(n: int) { return left(n); } func take(value: string) {} take(right(2));",
        )
        .unwrap_err();
        assert!(error.contains("expected string"), "{error}");
        assert!(error.contains("got int"), "{error}");
    }

    #[test]
    fn non_terminating_self_recursion_infers_never() {
        check_source("func loop() { return loop(); } func take(value: int) {} take(loop());")
            .unwrap();
    }

    #[test]
    fn unknown_return_source_does_not_create_a_false_mismatch() {
        check_source(
            "func dynamic() { return missing; } func take(value: int) {} take(dynamic());",
        )
        .unwrap();
    }

    #[test]
    fn inferred_array_returns_support_indexing_and_iteration() {
        check_source(
            "func values() { return [1, 2]; } print(values()[0]); for x: values() { print(x); } func first() { return values()[0]; } func take(value: int) {} take(first());",
        )
        .unwrap();
        check_source(
            "func values(n: int) { if n < 1 { return [1]; } return [values(n - 1)[0]]; } print(values(2)[0]);",
        )
        .unwrap();
        let error = check_source("func values() { return [1]; } values()[\"zero\"];").unwrap_err();
        assert!(error.contains("array index must be int"), "{error}");
    }

    #[test]
    fn inferred_return_variables_resolve_in_nested_types_and_expected_positions() {
        check_source(
            "func n() { return 1; } let mut x = n(); x = 2; let xs = [n()]; func take_array(values: int[]) {} take_array(xs); func take(value: int) {} take(if true { n() } else { 2 });",
        )
        .unwrap();
    }

    #[test]
    fn deferred_constraints_still_reject_incompatible_inferred_types() {
        let error =
            check_source("func n() { return \"bad\"; } let mut x = n(); x = 2;").unwrap_err();
        assert!(error.contains("expected string"), "{error}");
        assert!(error.contains("got int"), "{error}");

        let error = check_source(
            "func values() { return [\"bad\"]; } func take(values: int[]) {} take(values());",
        )
        .unwrap_err();
        assert!(error.contains("expected int[]"), "{error}");
        assert!(error.contains("got string[]"), "{error}");

        let error = check_source(
            "func n() { return \"bad\"; } func take(value: int) {} take(if true { n() } else { 2 });",
        )
        .unwrap_err();
        assert!(error.contains("expected int"), "{error}");
        assert!(error.contains("got string"), "{error}");
    }

    #[test]
    fn return_array_shape_solving_is_stable_across_function_order() {
        for _ in 0..32 {
            check_source(
                "func wrap() { return values(); } func values() { return [1]; } print(wrap()[0]);",
            )
            .unwrap();
            check_source(
                "func wrap() { return values(); } func values() { return [1]; } for x: wrap() { print(x); }",
            )
            .unwrap();
            check_source(
                "func element() { return value(); } func value() { return 1; } func array() { return [element()]; } func take(xs: int[]) {} take(array());",
            )
            .unwrap();
        }
        let error = check_source(
            "func values(flag: bool) { if flag { return [1]; } return 1; } values(true)[0];",
        )
        .unwrap_err();
        assert!(error.contains("expected array"), "{error}");
    }

    #[test]
    fn union_array_reads_join_elements_but_writes_preserve_invariance() {
        check_source(
            "let mut ints = [1]; func choose(flag: bool) { if flag { return ints; } return [\"s\"]; } print(choose(true)[0]);",
        )
        .unwrap();
        let error = check_source(
            "let mut ints = [1]; func choose(flag: bool) { if flag { return ints; } return [\"s\"]; } let mut xs = choose(true); xs[0] = \"bad\"; ints[0] + 1;",
        )
        .unwrap_err();
        assert!(error.contains("expected never"), "{error}");
        assert!(error.contains("got string"), "{error}");
    }

    #[test]
    fn unresolved_dictionary_subscript_preserves_dynamic_key_types() {
        check_source("func values() { return [\"key\": 1]; } print(values()[\"key\"]);").unwrap();
    }

    #[test]
    fn unknown_array_elements_keep_dynamic_fallback() {
        check_source(
            "func dynamic() { return nil; } func take(values: int[]) {} let values = [dynamic()]; take(values); take([dynamic()]);",
        )
        .unwrap();
        check_source("func dynamic() { return nil; } func take(f: () -> int) {} take(dynamic);")
            .unwrap();
    }

    #[test]
    fn inferred_array_types_contextually_type_empty_array_assignments() {
        check_source("func values() { return [1]; } let mut xs = values(); xs = [];").unwrap();
        check_source("func values() { return [[1]]; } let mut xs = values(); xs[0] = [];").unwrap();
        let error = check_source(
            "func empty() -> never[] { [] } func take(values: int[]) {} take(empty());",
        )
        .unwrap_err();
        assert!(error.contains("expected int[]"), "{error}");
        assert!(error.contains("got never[]"), "{error}");
    }

    #[test]
    fn explicit_return_type_context_types_empty_arrays() {
        check_source("func values() -> int[] { return []; }").unwrap();
        check_source("func values() -> int[] { return if true { [] } else { [] }; }").unwrap();
    }
}
