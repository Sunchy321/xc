use thin_vec::ThinVec;

use crate::expr::Expr;
use crate::literal::Literal;
use crate::pattern::Pattern;
use crate::ptr::P;

#[derive(Clone)]
pub enum StmtKind {
    Block(P<Block>, ThinVec<Catcher>, Option<Block>),
    If(P<Cond>, P<Expr>, Option<P<Expr>>),
    While {
        label: Option<P<Literal>>,
        cond: Cond,
        block: P<Block>,
        else_block: Option<P<Block>>,
    },
    For {
        label: Option<P<Literal>>,
        pat: P<Pattern>,
        expr: P<Expr>,
        block: P<Block>,
        else_block: Option<P<Block>>,
    },
    Break(Option<P<Literal>>, Option<P<Expr>>),
    Continue(Option<P<Literal>>, Option<P<Expr>>),
    Return(Option<P<Expr>>),
    Throw(Option<P<Expr>>),
}

#[derive(Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Clone)]
pub enum Cond {
    Expr(P<Expr>),
    Pattern(P<Pattern>),
}

#[derive(Clone)]
pub struct Block {
    pub stmts: ThinVec<Stmt>,
}

#[derive(Clone)]
pub struct Catcher {
    pub pattern: Pattern,
    pub stmts: ThinVec<Stmt>,
}
