#[derive(Clone, Debug)]
pub enum DeclKind {}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}
