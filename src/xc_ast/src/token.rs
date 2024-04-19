pub enum CommentKind {
    Line,
    Block,
}

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
    /// `>`
    Greater,
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

    Custom(&str),
}

pub enum LiteralKind {
    Bool,
    Integer,
    Floating,
    String,
    Symbol,
}

pub struct Literal {
    pub kind: LiteralKind,
    pub value: String,
    pub suffix: Option<String>,
}

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
}

pub struct Span {

}

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}