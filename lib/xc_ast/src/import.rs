use thin_vec::ThinVec;
use xc_span::{Identifier, Span, Symbol};

#[derive(Clone, Debug)]
pub enum ImportPathSegmentKind {
    Ident(Identifier),
    String(Symbol, Span),
}

#[derive(Clone, Debug)]
pub struct ImportPathSegment {
    pub kind: ImportPathSegmentKind,
}

impl ImportPathSegment {
    pub fn from_ident(ident: Identifier) -> Self {
        Self {
            kind: ImportPathSegmentKind::Ident(ident),
        }
    }

    pub fn from_string(string: Symbol, span: Span) -> Self {
        Self {
            kind: ImportPathSegmentKind::String(string, span),
        }
    }

    pub fn span(&self) -> Span {
        match &self.kind {
            ImportPathSegmentKind::Ident(ident) => ident.span,
            ImportPathSegmentKind::String(_, span) => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ImportPath {
    pub segments: ThinVec<ImportPathSegment>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ImportKind {}

#[derive(Clone, Debug)]
pub enum ImportItem {
    Star,
    Ident(Identifier),
    Operator,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub kind: ImportKind,
    pub path: ImportPath,
    pub items: Option<ThinVec<ImportItem>>,
    pub span: Span,
}
