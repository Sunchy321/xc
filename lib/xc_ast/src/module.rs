use xc_span::Span;

#[derive(Clone, Debug)]
pub struct Module {
    // pub decls: Vec<Decl>,
    // pub span: Span,
}

#[derive(Clone, Debug)]
pub enum VisKind {
    Public,
    Internal,
    Private,
    Inherited,
}

#[derive(Clone, Debug)]
pub struct Visibility {
    pub kind: VisKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Safety {
    Unsafe(Span),
    Default,
}

#[derive(Clone, Debug)]
pub enum Constness {
    Const(Span),
    Default,
}
