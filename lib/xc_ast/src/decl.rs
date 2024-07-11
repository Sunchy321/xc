use thin_vec::ThinVec;
use xc_span::Span;

use crate::import::Import;
use crate::module::Visibility;

#[derive(Clone, Debug)]
pub enum DeclKind {
    QualDecl(ThinVec<Qual>),
    Import(Import),
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub quals: ThinVec<Qual>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum QualKind {
    Vis(Visibility),

    Extern,
    Async,
    Static,
    Const,
    Unsafe,
    Partial,
}

#[derive(Clone, Debug)]
pub struct Qual {
    pub kind: QualKind,
    pub span: Span,
}
