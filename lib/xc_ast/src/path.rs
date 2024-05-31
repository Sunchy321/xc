use thin_vec::ThinVec;
use xc_span::{symbol::kw, Identifier, Span};

#[derive(Clone, Copy, Debug)]
pub enum PathStyle {
    Expr,
    Pattern,
    Type,
    Module,
}

impl PathStyle {
    pub fn has_generic_ambiguity(&self) -> bool {
        match self {
            PathStyle::Expr | PathStyle::Pattern => true,
            PathStyle::Type | PathStyle::Module => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PathSegment {
    pub ident: Identifier,
}

impl PathSegment {
    pub fn from_ident(ident: Identifier) -> Self {
        Self { ident }
    }

    pub fn path_root(span: Span) -> Self {
        Self::from_ident(Identifier::new(kw::PathRoot, span))
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: ThinVec<PathSegment>,
    pub span: Span,
}
