use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_span::Symbol;

pub mod cursor;

pub mod parser;
pub mod expr;
pub mod stmt;
pub mod pat;
pub mod ty;

pub type PResult<T> = Result<T, Diagnostic>;

#[derive(Clone)]
pub(crate) enum ExpectTokenKind {
    Token(TokenKind),
    Keyword(Symbol),
    Operator,
    Identifier,
}

#[derive(Clone, Copy, Debug)]
pub enum TokenExpectType {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug)]
pub enum HasTrailing {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug)]
pub enum Recovered {
    Yes,
    No,
}

pub struct SequenceSeparator {
    sep: Option<TokenKind>,
    allow_trailing: bool,
}

impl SequenceSeparator {
    pub fn new(sep: TokenKind) -> Self {
        Self { sep: Some(sep), allow_trailing: true }
    }
}