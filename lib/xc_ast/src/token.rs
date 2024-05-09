use crate::literal::Literal;
use xc_span::{Span, Symbol};

#[derive(Clone)]
pub enum CommentKind {
    Line,
    Block,
}

#[derive(Clone)]
pub enum Delimiter {
    /// `(` `)`
    Paren,
    /// `[` `]`
    Bracket,
    /// `{` `}`
    Brace,
    /// `{|` `|}`
    DictBound,
}

#[derive(Clone)]
pub enum CustomOp {
    Op(Symbol),
}

impl CustomOp {
    pub fn as_str(&self) -> &str {
        match self {
            CustomOp::Op(op) => op.as_str(),
        }
    }
}

#[derive(Clone)]
pub enum TokenKind {
    /// @
    At,
    /// #
    Pound,
    /// $
    Dollar,
    /// ;
    Semicolon,
    /// :
    Colon,
    /// ,
    Comma,

    /// '(
    SymbolOpen,
    /// ::
    ColonColon,
    /// ->
    RightArrow,
    /// =>
    FatArrow,
    /// ...
    DotDotDot,

    Op(Symbol),
    OpenDelim(Delimiter),
    ClosedDelim(Delimiter),

    Literal(Literal),
    SymbolLit(Symbol),

    Identifier(Symbol),

    LambdaArgUnnamed(u32),
    LambdaArgNamed(String),

    Eof,
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

