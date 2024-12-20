use thin_vec::ThinVec;
use xc_span::Span;

use crate::func::Func;
use crate::import::Import;
use crate::module::Visibility;
use crate::ptr::P;
use crate::r#trait::Trait;

#[derive(Clone, Debug)]
pub enum DeclKind {
    QualDecl(ThinVec<Qual>),
    Import(Import),
    Func(P<Func>),
    Trait(P<Trait>),
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum QualKind {
    Vis(Visibility),

    Async,
    Auto,
    Const,
    Extern,
    Partial,
    Static,
    Unsafe,
}

#[derive(Clone, Debug)]
pub struct Qual {
    pub kind: QualKind,
    pub span: Span,
}
