use xc_span::Symbol;

use crate::expr::CastType;
use crate::ptr::P;
use crate::ty::Type;

#[derive(Clone, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum OperatorKind {
    Prefix,
    Suffix,
    Infix,
}

#[derive(Clone, Debug)]
pub struct OperatorID {
    pub op: Symbol,
    pub kind: OperatorKind,
    pub symbol: Symbol,
}
