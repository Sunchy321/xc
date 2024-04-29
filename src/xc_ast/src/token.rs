use crate::literal::Literal;

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
    Op(String)
}

impl CustomOp {
    pub fn as_str(&self) -> &str {
        match self {
            CustomOp::Op(op) => op.as_str(),
        }
    }
}

#[derive(Clone)]
pub enum OpKind {
    /// `+!`
    Successor,
    /// `-!`
    Predecessor,
    /// `?`
    Null,

    /// `!`
    Not,
    /// `'~`
    BitNot,

    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Modulo,

    /// `+`
    Plus,
    /// `-`
    Subtract,

    /// `'&`
    BitAnd,
    /// `'^`
    BitXor,
    /// `'|`
    BitOr,

    /// `..`
    Range,
    /// `..=`
    ClosedRange,

    /// `~`
    Concat,

    /// `??`
    NullCoalescing,

    /// `==`
    Equal,
    /// `!=`
    NotEqual,
    /// `<`
    Less,
    /// `!<`
    NotLess,
    /// `>`
    Greater,
    /// `!>`
    NotGreater,
    /// `<=`
    LessEqual,
    /// `>=`
    GreaterEqual,
    /// `<>`
    LessGreater,

    /// `&`
    And,
    /// `|`
    Or,

    /// `=`
    Assign,
    /// `+=`
    PlusAssign,
    /// `-=`
    SubtractAssign,
    /// `*=`
    MultiplyAssign,
    /// `/=`
    DivideAssign,
    /// `%=`
    ModuloAssign,
    /// `'&=`
    BitAndAssign,
    /// `'^=`
    BitXorAssign,
    /// `'|=`
    BitOrAssign,
    /// `??=`
    NullCoalescingAssign,
    /// `++`
    Increment,
    /// `--`
    Decrement,
    /// `~>`
    ConcatLeft,
    /// `<~`
    ConcatRight,

    /// `;`
    Semicolon,

    Custom(CustomOp),
}

#[derive(Clone)]
pub enum TokenKind {
    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `,`
    Comma,
    /// `@`
    At,

    Op(OpKind),
    OpenDelim(Delimiter),
    ClosedDelim(Delimiter),

    Literal(Literal),

    Identifier(String),
}

pub struct Span {

}

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn is_bool(str: &String) -> bool {
    str == "true" || str == "false"
}