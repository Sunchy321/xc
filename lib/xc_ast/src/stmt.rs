use thin_vec::ThinVec;
use xc_span::Span;

use crate::expr::{Expr, ExprFlags};
use crate::pattern::Pattern;
use crate::ptr::P;

#[derive(Clone, Debug)]
pub enum StmtKind {
    Expr(P<Expr>),
    ExprSemi(P<Expr>),
    Empty
}

impl StmtKind {
    pub fn expr_flags(&self) -> ExprFlags {
        match self {
            StmtKind::Expr(expr) => expr.flags,
            StmtKind::ExprSemi(expr) => expr.flags,
            StmtKind::Empty => ExprFlags::empty(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn add_trailing_semicolon(mut self) -> Self {
        use StmtKind::*;

        self.kind = match self.kind {
            Expr(expr) => ExprSemi(expr),
            kind => kind
        };

        self
    }
}

#[derive(Clone, Debug)]
pub enum Cond {
    Expr(P<Expr>),
    Pattern(P<Pattern>),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: ThinVec<Stmt>,
    pub span: Span,
}

impl Block {
    pub fn expr_flags(&self) -> ExprFlags {
        self.stmts.iter().fold(ExprFlags::empty(), |flags, stmt| {
            flags | stmt.kind.expr_flags().difference(ExprFlags::CONTAIN_BOUND_EXPR | ExprFlags::CONTAIN_NIL_COALESCE)
        })
    }
}

#[derive(Clone, Debug)]
pub struct Catcher {
    pub pattern: Pattern,
    pub stmts: ThinVec<Stmt>,
}
