use xc_span::Symbol;

use crate::{expr::CastType, op::{Operator, OperatorKind}, ptr::P, ty::Type};



#[derive(Clone)]
pub enum UnqualID {
    /// `foo`
    Identifier(Symbol),
    /// `'then`
    SymbolLit(Symbol),
    /// `init`
    Init,
    /// `deinit`
    Deinit,
    /// `operator infix+`
    Operator(OperatorID),
    /// `operator ""s`
    Suffix(Symbol),
    /// `operator $`
    OperatorDollar,
    /// `operator if`
    OperatorIf,
    /// `operator is int`
    OperatorIs(P<Type>),
    /// `operator as int`
    OperatorAs(P<Type>, CastType),
}

#[derive(Clone)]
pub struct OperatorID {
    pub op: Operator,
    pub kind: OperatorKind,
    pub symbol: Symbol,
}