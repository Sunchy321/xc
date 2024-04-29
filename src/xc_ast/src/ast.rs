use thin_vec::ThinVec;

use crate::prefix_op::PrefixOp;
use crate::suffix_op::SuffixOp;
use crate::infix_op::InfixOp;
use crate::literal::Literal;
use crate::ptr::P;

#[derive(Clone)]
pub enum ExprKind {
    /// A literal (`0`, `true`, `"hello"`, etc.)
    Literal(Literal),
    /// Array literal (`[1, 2, 3]`)
    Array(ThinVec<P<Expr>>),
    /// Tuple literal (`(1, 2, 3)`)
    Tuple(ThinVec<P<Expr>>),
    /// Object literal (`{a: 1, b: 2}`)
    Object(ThinVec<(String, P<Expr>)>),
    /// Dictionary literal (`{| a: 1, b: 2 |}`)
    Dict(ThinVec<(String, P<Expr>)>),
    /// `nil`
    Nil,
    /// `this`
    This,
    /// `$`
    Dollar,

    /// Suffix exprs (`a?`)
    Suffix(SuffixOp, P<Expr>),
    /// Function calls (`foo(a, b, c)`)
    Call(P<Expr>, Arguments),
    /// Method calls (`foo.bar(a, b, c)`)
    MethodCall(Box<MethodCall>),
    /// Subscripting (`foo[bar]`)
    Subscript(P<Expr>, Arguments),
    /// Awaiting (`foo().await`)
    Await(P<Expr>),

    /// Prefix exprs (`-a`)
    Prefix(PrefixOp, P<Expr>),

    /// Infix exprs (`a + b`)
    Infix(InfixOp, P<Expr>, P<Expr>),
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone)]
pub struct Arguments {
    pub unnamed_args: ThinVec<Expr>,
    pub named_args: ThinVec<(String, Expr)>,
}

#[derive(Clone)]
pub struct MethodCall {
    pub receiver: P<Expr>,
    pub method_name: String,
    pub args: Arguments,
}