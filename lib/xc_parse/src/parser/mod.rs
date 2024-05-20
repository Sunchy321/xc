use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_span::Symbol;

pub mod cursor;

pub mod parser;
pub mod diag;

pub mod expr;
pub mod stmt;
pub mod pat;
pub mod ty;

pub type ParseResult<'a, T> = Result<T, Diagnostic<'a>>;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub(crate) struct Restrictions: u16 {
        const NO_STRUCT_LITERAL = 1 << 1;
        const THEN_IS_KEYWORD = 1 << 2;
    }
}

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

pub(crate) enum ConsumeClosingDelim {
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