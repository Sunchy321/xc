pub mod ptr;

pub mod ast;

pub mod literal;
pub mod token;

pub mod path;

pub mod expr;
pub mod lambda;

pub mod id;
pub mod pattern;
pub mod ty;

pub mod stmt;

pub mod import;
pub mod func;
pub mod decl;
pub mod module;

pub mod tokenstream;
pub mod attr;

#[derive(Clone, Debug)]
pub enum Mutability {
    Immut,
    Mut,
}