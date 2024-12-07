use thin_vec::ThinVec;
use xc_span::{Identifier, Span};

use crate::attr::AttrVec;
use crate::expr::Expr;
use crate::path::Path;
use crate::ptr::P;
use crate::ty::Type;

#[derive(Debug, Clone)]
pub enum GenericParamKind {
    Type { default: Option<P<Type>> },
    Const { ty: P<Type>, const_span: Span, default: Option<P<Expr>> },
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub ident: Identifier,
    pub attrs: AttrVec,
    pub kind: GenericParamKind,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub params: ThinVec<GenericParam>,
    pub where_clause: WhereClause,
    pub span: Span
}

impl Default for Generic {
    fn default() -> Self {
        Self {
            params: ThinVec::new(),
            where_clause: Default::default(),
            span: Span::DUMMY
        }
    }
}

#[derive(Debug, Clone)]
pub struct WhereClause {
    pub has_where_token: bool,
    pub predicates: ThinVec<WherePredicate>,
    pub span: Span
}

impl WhereClause {
    pub fn is_empty(&self) -> bool {
        !self.has_where_token && self.predicates.is_empty()
    }
}

impl Default for WhereClause {
    fn default() -> Self {
        Self {
            has_where_token: false,
            predicates: ThinVec::new(),
            span: Span::DUMMY
        }
    }
}

#[derive(Debug, Clone)]
pub struct WherePredicate {
    pub kind: WherePredicateKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum WherePredicateKind {
    Unimplemented
}

pub type GenericBounds = Vec<GenericBound>;

#[derive(Debug, Clone)]
pub enum GenericBound {
    Trait(Path, TraitBoundQualifiers),
}

#[derive(Debug, Clone)]
pub struct TraitBoundQualifiers {
    pub polarity: BoundPolarity,
}

#[derive(Debug, Clone)]
pub enum BoundPolarity {
    Positive,
    Negative(Span),
    Maybe(Span),
}