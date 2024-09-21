use xc_span::Span;

use crate::path::Path;

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