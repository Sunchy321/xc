use crate::module::{Constness, Safety};
use crate::ptr::P;
use crate::ty::Type;

#[derive(Debug, Clone)]
pub struct Trait {
    pub safety: Safety,
    pub constness: Constness,
}

#[derive(Debug, Clone)]
pub enum TraitBody {
    Alias(P<Type>),
}
