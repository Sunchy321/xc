use crate::{literal::Literal, op::Operator};
use xc_span::{Identifier, Span, Symbol};

#[derive(Clone)]
pub enum CommentKind {
    Line,
    Block,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Delimiter {
    /// `(` `)`
    Paren,
    /// `[` `]`
    Bracket,
    /// `{` `}`
    Brace,
    /// `{|` `|}`
    DictBound,

    Invisible,
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

#[derive(Clone, PartialEq)]
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
    CloseDelim(Delimiter),

    Literal(Literal),

    Identifier(Symbol),

    LambdaArgUnnamed(u32),
    LambdaArgNamed(String),

    Eof,
}

impl TokenKind {
    pub fn is_punc(&self) -> bool {
        use TokenKind::*;

        match self {
            At | Pound | Dollar | Semicolon | Colon | Comma | SymbolOpen | ColonColon
            | RightArrow | FatArrow | DotDotDot | Op(_) | OpenDelim(_) | CloseDelim(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn is_keyword(&self, key: Symbol) -> bool {
        match self.to_identifier() {
            Some(id) => id.name == key,
            None => false,
        }
    }

    pub fn to_identifier(&self) -> Option<Identifier> {
        match &self.kind {
            &TokenKind::Identifier(name) => Some(Identifier::new(name, self.span)),
            _ => None,
        }
    }
}