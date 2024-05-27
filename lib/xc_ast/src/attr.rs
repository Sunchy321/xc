use xc_span::{Span, Symbol};

use crate::ast::DelimitedArgs;
use crate::expr::Expr;
use crate::ptr::P;
use crate::token::CommentKind;

#[derive(Clone, Debug)]
pub enum AttrKind {
    DocComment(CommentKind, Symbol),
    Normal(P<NormalAttr>),
}

#[derive(Clone, Copy, Debug)]
pub enum AttrStyle {
    Outer,
    Inner,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub kind: AttrKind,
    pub style: AttrStyle,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum NormalAttrArgs {
    Empty,
    Delimited(DelimitedArgs),
}

#[derive(Clone, Debug)]
pub struct NormalAttr {
    pub name: Symbol,
    pub args: NormalAttrArgs,
    // pub tokens:
}
