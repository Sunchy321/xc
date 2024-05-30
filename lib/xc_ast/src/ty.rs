use thin_vec::ThinVec;
use xc_span::{Span, Symbol};

use crate::{expr::Expr, ptr::P};

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
    Tuple(ThinVec<P<Type>>),
    Object(ObjectType),
    Dict(P<Type>, P<Type>),

    Paren(P<Type>),
    Result(P<Type>, P<Type>),
    Typeof(P<Expr>),

    SomeType(P<Type>),
    AnyType(P<Type>),
    Class(P<Type>),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ObjectType {
    pub fields: ThinVec<Field>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub type_id: P<Type>,
    pub is_mut: bool,
}
