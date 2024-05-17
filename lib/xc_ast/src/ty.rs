use thin_vec::ThinVec;

use crate::{expr::Expr, ptr::P};

#[derive(Clone)]
pub enum TypeKind {
    Void,
    Never,
    Bool,
    Int(i8, i128, i128),
    Float(i8),
    String(String),
    Symbol(String),

    SelfType,

    Nullable(P<Type>),
    Array(P<Type>, usize),
    Tuple(ThinVec<Type>),
    Object(ObjectType),
    Dict(P<Type>, P<Type>),

    Paren(P<Type>),
    Throw(P<Type>, P<Type>),
    Typeof(P<Expr>)
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Clone)]
pub struct ObjectType {
    pub fields: ThinVec<Field>,
}

#[derive(Clone)]
pub struct Field {
    pub name: String,
    pub type_id: P<Type>,
    pub is_mut: bool,
}
