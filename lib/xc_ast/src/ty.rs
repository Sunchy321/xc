use thin_vec::ThinVec;
use xc_span::{Span, Symbol};

use crate::{expr::Expr, ptr::P, Mutability};

#[derive(Clone, Debug)]
pub enum TypeKind {
    Void,
    Never,
    Bool,
    Int(Option<i8>, i128, i128),
    Float(Option<i8>),
    String,
    Char,
    SymbolType(Symbol),

    SelfType,

    Optional(P<Type>),
    Array(P<Type>),
    Struct(StructType),
    Dict(P<Type>, P<Type>),
    Reference(P<Type>, Mutability),

    Paren(P<Type>),
    Result(P<Type>, P<Type>),
    Typeof(P<Expr>),

    ImplType(P<Type>),
    DynType(P<Type>),
    Class(P<Type>),

    ImplicitThis,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub members: ThinVec<Member>,
}

#[derive(Clone, Debug)]
pub enum Member {
    Ordinal(P<Type>, bool),
    Named(Symbol, P<Type>, bool),
}
