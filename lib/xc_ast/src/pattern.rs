use thin_vec::ThinVec;

use crate::expr::Expr;
use crate::ptr::P;
use crate::token::Token;
use crate::ty::Type;

#[derive(Clone)]
pub enum PatternKind {
    Wildcard,
    Expr(P<Expr>),
    Bind(P<PatternBind>),
    Array(ArrayPattern<Self>),
    Tuple(TuplePattern<Self>),
    Object(ObjectPattern<Self>),
}

#[derive(Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub assert: Option<PatternAssertion>,
}

#[derive(Clone)]
pub enum PatternBindKind {
    Wildcard,
    Expr(P<Expr>),
    Identifier(P<Token>),
    Array(ArrayPattern<Self>),
    Tuple(TuplePattern<Self>),
    Object(ObjectPattern<Self>),
}

#[derive(Clone)]
pub enum BindKey {
    Let,
    LetMut,
}

#[derive(Clone)]
pub struct PatternBind {
    pub kind: PatternBindKind,
    pub key: BindKey,
}

#[derive(Clone)]
pub enum ArrayPatternItem<T> {
    Item(P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone)]
pub struct ArrayPattern<T> {
    pub items: ThinVec<ArrayPatternItem<T>>,
}

#[derive(Clone)]
pub enum TuplePatternItem<T> {
    Item(P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone)]
pub struct TuplePattern<T> {
    pub items: ThinVec<TuplePatternItem<T>>,
}

#[derive(Clone)]
pub enum ObjectPatternItem<T> {
    Field(String, P<T>),
    Rest(Option<P<T>>),
}

#[derive(Clone)]
pub struct ObjectPattern<T> {
    pub items: ThinVec<ObjectPatternItem<T>>,
}

#[derive(Clone)]
pub enum PatternAssertion {
    Type(P<Type>, TypeAssertionKey),
    In(P<Expr>),
    If(P<Expr>),
}

#[derive(Clone)]
pub enum TypeAssertionKey {
    Is,
    Colon,
    As,
}
