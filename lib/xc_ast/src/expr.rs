use core::fmt;

use thin_vec::ThinVec;
use xc_error::ErrorGuaranteed;
use xc_span::{Identifier, Span, Symbol};

use crate::literal::Literal;
use crate::path::Path;
use crate::pattern::Pattern;
use crate::ptr::P;
use crate::stmt::Block;
use crate::ty::{Type, TypeKind};

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// Placeholder (`_`)
    Placeholder,
    /// Paren (`(e)`)
    Paren(P<Expr>),
    /// A literal (`0`, `true`, `"hello"`, etc.)
    Literal(Literal),
    /// Path (`a::b`)
    Path(Path),
    /// Array literal (`[1, 2, 3]`)
    Array(ThinVec<ExprItem>),
    /// Struct literal (`(a: 1, b: 2)`)
    Struct(ThinVec<StructItem>),
    /// Dictionary literal (`[ a: 1, b: 2 ]`)
    Dict(ThinVec<DictItem>),
    /// `nil`
    Nil,
    /// `this`
    This,
    /// `$`
    Dollar,
    /// `$0`
    LambdaArgOrdinal(u32),
    /// `$id`
    LambdaArgNamed(Symbol),
    /// `.Yes`
    AnonEnumerator(Symbol),

    /// `{ some_expr }`
    Block(P<Block>),
    /// `if cond { then } else { else }`
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    /// `match expr { pat => body }`
    Match(P<Expr>, ThinVec<MatchArm>),
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
    Suffix(Symbol, P<Expr>),
    /// Function calls (`foo(a, b, c)`)
    FuncCall(P<Expr>, Arguments, ThinVec<P<Block>>),
    /// MemberAccess (`foo.bar` or `foo.0`)
    MemberAccess(P<Expr>, Symbol),
    /// MemberDynamicAccess (`foo.('bar)`)
    MemberDynamicAccess(P<Expr>, P<Expr>),
    /// Method calls (`foo.bar(a, b, c)`)
    MethodCall(Box<MethodCall>),
    /// Subscripting (`foo[bar]`)
    Subscript(P<Expr>, Arguments),
    /// Casting (`a as int`)
    Cast(P<Expr>, P<Type>, CastType),
    /// Awaiting (`foo().await`)
    Await(P<Expr>),

    /// Prefix exprs (`-a`)
    Prefix(Symbol, P<Expr>),
    /// Try exprs (`try foo()`)
    Try(P<Expr>),

    /// Infix exprs (`a + b`)
    Infix(Symbol, ThinVec<P<Expr>>),
    /// Comparison exprs (`a < b <= c`)
    Comparison(ThinVec<Symbol>, ThinVec<P<Expr>>),
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

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pat: P<Pattern>,
    pub guard: P<Expr>,
    pub body: Option<P<Expr>>,
    pub span: Span,
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
    pub fn is_complete(&self) -> bool {
        use ExprKind::*;

        match self.kind {
            Block(..) | If(..) | Match(..) | While(..) | For(..) => true,
            _ => false,
        }
    }

    pub fn require_semi_to_be_stmt(&self) -> bool {
        !self.is_complete()
    }

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

    pub fn to_single_ident(&self) -> Option<Identifier> {
        use ExprKind::*;

        match &self.kind {
            Path(p) => p.to_single_ident(),
            _ => None,
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = format!("Expr[{:?}..{:?}]", self.span.lo.0, self.span.hi.0);

        write!(f, "{}", name)?;
        fmt::Debug::fmt(&self.kind, f)
    }
}

#[derive(Clone, Debug)]
pub enum ExprItem {
    Expr(P<Expr>),
    Expansion(P<Expr>),
    ExpansionPlaceHolder,
}

#[derive(Clone, Debug)]
pub enum StructItem {
    Ordinal(P<Expr>),
    Named(Symbol, P<Expr>),
    Expansion(P<Expr>),
    ExpansionPlaceHolder,
}

#[derive(Clone, Debug)]
pub enum DictItem {
    KeyValue(P<Expr>, P<Expr>),
    Expansion(P<Expr>),
}

#[derive(Clone, Debug)]
pub struct Arguments(pub ThinVec<Argument>);

#[derive(Clone, Debug)]
pub enum Argument {
    Ordinal(P<Expr>),
    Named(Symbol, P<Expr>),
    Expansion(P<Expr>),
}

impl Arguments {
    pub fn new() -> Self {
        Self(ThinVec::new())
    }

    pub fn from_expr_items(exprs: ThinVec<ExprItem>) -> Self {
        Self(
            exprs
                .into_iter()
                .map(|e| match e {
                    ExprItem::Expr(e) => Argument::Ordinal(e),
                    ExprItem::Expansion(e) => Argument::Expansion(e),
                    ExprItem::ExpansionPlaceHolder => {
                        todo!("ExpansionPlaceHolder in Arguments")
                    }
                })
                .collect(),
        )
    }

    pub fn from_expr_list(exprs: ThinVec<P<Expr>>) -> Self {
        Self(exprs.into_iter().map(Argument::Ordinal).collect())
    }

    pub fn from_struct_items(items: ThinVec<StructItem>) -> Self {
        Self(
            items
                .into_iter()
                .map(|i| match i {
                    StructItem::Ordinal(e) => Argument::Ordinal(e),
                    StructItem::Named(s, e) => Argument::Named(s, e),
                    StructItem::Expansion(e) => Argument::Expansion(e),
                    StructItem::ExpansionPlaceHolder => {
                        todo!("ExpansionPlaceHolder in Arguments")
                    }
                })
                .collect(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct MethodCall {
    pub receiver: P<Expr>,
    pub method_name: String,
    pub args: Arguments,
    pub blocks: ThinVec<P<Block>>,
}

#[derive(Clone, Debug)]
pub enum CastType {
    Normal,
    Optional,
    Forced,
}
