use thin_vec::ThinVec;

use crate::infix_op::InfixOp;
use crate::literal::Literal;
use crate::pattern::Pattern;
use crate::prefix_op::PrefixOp;
use crate::ptr::P;
use crate::suffix_op::SuffixOp;
use crate::ty::Type;

#[derive(Clone)]
pub enum ExprKind {
    /// Paren (`(e)`)
    Paren(P<Expr>),
    /// A literal (`0`, `true`, `"hello"`, etc.)
    Literal(Literal),
    /// Array literal (`[1, 2, 3]`)
    Array(ThinVec<ExprItem>),
    /// Tuple literal (`(1, 2, 3)`)
    Tuple(ThinVec<ExprItem>),
    /// Object literal (`{a: 1, b: 2}`)
    Object(ThinVec<(String, ExprItem)>),
    /// Dictionary literal (`{| a: 1, b: 2 |}`)
    Dict(ThinVec<(String, ExprItem)>),
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
    /// Casting (`a as int`)
    Cast(P<Expr>, P<Type>),
    /// Awaiting (`foo().await`)
    Await(P<Expr>),
    /// Catching (`foo().catch`)
    Catch(P<Expr>),

    /// Prefix exprs (`-a`)
    Prefix(PrefixOp, P<Expr>),
    /// Try exprs (`try foo()`)
    Try(P<Expr>),

    /// Infix exprs (`a + b`)
    Infix(InfixOp, P<Expr>, P<Expr>),
    /// Pattern match (`a is T`)
    Is(P<Expr>, P<Pattern>),
    /// Pattern not match (`a !is T`)
    NotIs(P<Expr>, P<Pattern>),
    /// Increment (`a++`)
    Increment(P<Expr>),
    /// Decrement (`a--`)
    Decrement(P<Expr>),
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone)]
pub enum ExprItem {
    Expr(Expr),
    ExpandExpr(Expr),
}

#[derive(Clone)]
pub struct Arguments {
    pub unnamed_args: ThinVec<ExprItem>,
    pub named_args: ThinVec<(String, Expr)>,
}

#[derive(Clone)]
pub struct MethodCall {
    pub receiver: P<Expr>,
    pub method_name: String,
    pub args: Arguments,
}
