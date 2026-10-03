use thin_vec::ThinVec;
use xc_span::{Identifier, Span};

use crate::expr::Expr;
use crate::func::Func;
use crate::generic::Generic;
use crate::module::{Constness, Safety};
use crate::ptr::P;
use crate::stmt::Block;
use crate::ty::Type;

#[derive(Debug, Clone)]
pub struct Trait {
    pub safety: Safety,
    pub constness: Constness,
    pub ident: Identifier,
    pub generics: Generic,
    /// Super-traits (`: A, B`), parsed as types to allow generic arguments.
    pub supertraits: ThinVec<P<Type>>,
    pub items: ThinVec<MemberItem>,
}

/// A member shared by trait bodies, impl blocks and class bodies.
#[derive(Debug, Clone)]
pub enum MemberItem {
    Type(TypeConstraintItem),
    Const(ConstConstraintItem),
    Func(P<Func>),
    Prop(PropConstraintItem),
}

/// `type Item = T;` / `type Iter : Bound;` / `type Output;`
#[derive(Debug, Clone)]
pub struct TypeConstraintItem {
    pub ident: Identifier,
    pub bound: Option<P<Type>>,
    pub value: Option<P<Type>>,
    pub span: Span,
}

/// `const N;` / `const N = expr;`
#[derive(Debug, Clone)]
pub struct ConstConstraintItem {
    pub ident: Identifier,
    pub value: Option<P<Expr>>,
    pub span: Span,
}

/// A property member: `let name: T;` / `let name => expr;` /
/// `let name: T { get { ... } }`.
#[derive(Debug, Clone)]
pub struct PropConstraintItem {
    pub ident: Identifier,
    pub ty: Option<P<Type>>,
    pub body: PropBody,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PropBody {
    /// `let x: T;`
    Semicolon,
    /// `let x => e;` or `let x = e;`
    Expr(P<Expr>),
    /// `{ get { ... } set { ... } }` — accessor bodies.
    Accessor(ThinVec<P<Block>>),
}
