use thin_vec::ThinVec;
use xc_span::{Identifier, Span};

use crate::expr::Expr;
use crate::ptr::P;
use crate::ty::Type;

#[derive(Clone, Debug)]
pub enum PatternKind {
    Wildcard,
    Expr(P<Expr>),
    Bind(P<PatternBind>),
    Array(ArrayPattern<Self>),
    Tuple(TuplePattern<Self>),
    Object(ObjectPattern<Self>),
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub kind: PatternKind,
    pub assert: Option<PatternAssertion>,
    pub span: Span
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self {
            kind,
            assert: None,
            span
        }
    }

}

#[derive(Clone, Debug)]
pub enum PatternBindKind {
    Wildcard,
    Expr(P<Expr>),
    Identifier(Identifier),
    Array(ArrayPattern<Self>),
    Tuple(TuplePattern<Self>),
    Object(ObjectPattern<Self>),
}

#[derive(Clone, Copy, Debug)]
pub enum BindKey {
    Let,
    LetMut,
}

#[derive(Clone, Debug)]
pub struct PatternBind {
    pub kind: PatternBindKind,
    pub key: BindKey,
}

#[derive(Clone, Debug)]
pub enum ArrayPatternItem<T> {
    Item(P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone, Debug)]
pub struct ArrayPattern<T> {
    pub items: ThinVec<ArrayPatternItem<T>>,
}

#[derive(Clone, Debug)]
pub enum TuplePatternItem<T> {
    Item(P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone, Debug)]
pub struct TuplePattern<T> {
    pub items: ThinVec<TuplePatternItem<T>>,
}

#[derive(Clone, Debug)]
pub enum ObjectPatternItem<T> {
    Field(String, P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone, Debug)]
pub struct ObjectPattern<T> {
    pub items: ThinVec<ObjectPatternItem<T>>,
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
