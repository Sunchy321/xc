use thin_vec::ThinVec;
use xc_span::{Identifier, Span, Symbol};

use crate::expr::Expr;
use crate::ptr::P;
use crate::ty::Type;
use crate::Mutability;

#[derive(Clone, Debug)]
pub enum PatternKind {
    Paren(P<Pattern>),
    Wildcard,
    Expr(P<Expr>),
    Bind(Identifier, Mutability),
    Array(ThinVec<ArrayPatternItem>),
    Struct(ThinVec<StructPatternItem>),
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub kind: PatternKind,
    pub assert: Option<PatternAssertion>,
    pub span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self {
            kind,
            assert: None,
            span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BindKey {
    Let,
    LetMut,
}

#[derive(Clone, Debug)]
pub enum ArrayPatternItem {
    Item(P<Pattern>),
    Expand(Option<P<Pattern>>),
}

#[derive(Clone, Debug)]
pub enum StructPatternItem {
    Ordinal(P<Pattern>),
    Named(Symbol, P<Pattern>),
    Expand(Option<P<Pattern>>),
}

#[derive(Clone, Debug)]
pub enum PatternAssertion {
    Type(P<Type>, TypeAssertionKey),
    In(P<Expr>),
    If(P<Expr>),
}

#[derive(Clone, Debug)]
pub enum TypeAssertionKey {
    Is,
    Colon,
    As,
}
