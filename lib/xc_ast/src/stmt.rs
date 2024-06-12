use thin_vec::ThinVec;
use xc_span::Span;

use crate::expr::Expr;
use crate::literal::Literal;
use crate::pattern::Pattern;
use crate::ptr::P;

#[derive(Clone, Debug)]
pub enum StmtKind {
    Expr(P<Expr>),
    ExprSemi(P<Expr>),
    Empty
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

#[derive(Clone, Debug)]
pub struct Catcher {
    pub pattern: Pattern,
    pub stmts: ThinVec<Stmt>,
}
