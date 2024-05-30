use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_span::Symbol;

pub mod cursor;
pub mod op;

pub mod diag;
pub mod parser;

pub mod expr;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod attr;
pub mod module;

pub mod tests;

pub type ParseResult<'a, T> = Result<T, Diagnostic<'a>>;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub(crate) struct Restrictions: u16 {
        const NO_STRUCT_LITERAL = 1 << 1;
        const THEN_IS_KEYWORD = 1 << 2;
        const IS_IN_LET = 1 << 3;
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

#[derive(Clone, Copy, Debug)]
pub(crate) enum ConsumeClosingDelim {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Case {
    Sensitive,
    Insensitive,
}

pub struct SequenceSeparator {
    sep: Option<TokenKind>,
    allow_trailing: bool,
}

impl SequenceSeparator {
    pub fn new(sep: TokenKind) -> Self {
        Self {
            sep: Some(sep),
            allow_trailing: true,
        }
    }
}
