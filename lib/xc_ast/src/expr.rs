use thin_vec::ThinVec;
use xc_error::ErrorGuaranteed;
use xc_span::{Span, Symbol};

use crate::literal::Literal;
use crate::op::{InfixOp, PrefixOp, SuffixOp};
use crate::pattern::Pattern;
use crate::ptr::P;
use crate::stmt::{Block, Cond};
use crate::ty::{Type, TypeKind};

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
    /// Struct literal (`{a: 1, b: 2}`)
    Struct(ThinVec<(Symbol, ExprItem)>),
    /// Dictionary literal (`{| a: 1, b: 2 |}`)
    Dict(ThinVec<(Symbol, ExprItem)>),
    /// `nil`
    Nil,
    /// `this`
    This,
    /// `$`
    Dollar,
    /// `$0`
    LambdaArgUnnamed(u32),
    /// `$id`
    LambdaArgNamed(Symbol),
    /// `.Yes`
    AnonEnumerator(Symbol),

    /// `{ some_expr }`
    Block(P<Block>),
    /// `if cond { then } else { else }`
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    /// `for pat in expr { body } else { else }`
    For(
        P<Expr>,
        P<Expr>,
        P<Block>,
        Option<P<Block>>,
        ForLoopKind,
        Option<Symbol>,
    ),
    /// `while cond { body } else { else }`
    While(P<Expr>, P<Expr>, Option<P<Expr>>, Option<Symbol>),

    /// `return expr`
    Return(Option<P<Expr>>),
    /// `throw expr`
    Throw(Option<P<Expr>>),
    /// `break expr`
    Break(Option<P<Expr>>, Option<Symbol>),
    /// `continue`
    Continue(Option<Symbol>),

    /// Suffix exprs (`a?`)
    Suffix(SuffixOp, P<Expr>),
    /// Function calls (`foo(a, b, c)`)
    Call(P<Expr>, Arguments),
    /// MemberAccess (`foo.bar`)
    MemberAccess(P<Expr>, Symbol),
    /// Method calls (`foo.bar(a, b, c)`)
    MethodCall(Box<MethodCall>),
    /// Subscripting (`foo[bar]`)
    Subscript(P<Expr>, Arguments),
    /// Casting (`a as int`)
    Cast(P<Expr>, P<Type>, CastType),
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

    /// Error
    Error(ErrorGuaranteed),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ForLoopKind {
    For,
    ForAwait,
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn to_type(&self) -> Option<P<Type>> {
        use ExprKind::*;

        let kind = match &self.kind {
            Paren(e) => e.to_type().map(TypeKind::Paren)?,

            _ => return None,
        };

        Some(P(Type {
            kind,
            span: self.span,
        }))
    }
}

#[derive(Clone)]
pub enum ExprItem {
    Expr(P<Expr>),
    ExpandExpr(P<Expr>),
}

#[derive(Clone)]
pub struct Arguments {
    pub unnamed_args: ThinVec<ExprItem>,
    pub named_args: ThinVec<(String, Expr)>,
}

impl Arguments {
    pub fn new() -> Self {
        Self {
            unnamed_args: ThinVec::new(),
            named_args: ThinVec::new(),
        }
    }

    pub fn from_expr_items(exprs: ThinVec<ExprItem>) -> Self {
        Self {
            unnamed_args: exprs,
            named_args: ThinVec::new(),
        }
    }

    pub fn from_expr_list(exprs: ThinVec<P<Expr>>) -> Self {
        Self {
            unnamed_args: exprs.into_iter().map(|e| ExprItem::Expr(e)).collect(),
            named_args: ThinVec::new(),
        }
    }
}

#[derive(Clone)]
pub struct MethodCall {
    pub receiver: P<Expr>,
    pub method_name: String,
    pub args: Arguments,
}

#[derive(Clone)]
pub enum CastType {
    Normal,
    Optional,
    Forced,
}
