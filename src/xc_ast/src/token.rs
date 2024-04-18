pub enum CommentKind {
    Line,
    Block,
}

pub enum Delimiter {
    Paren,
    Bracket,
    Brace,
    DictBound,
}

pub enum LiteralKind {
    Bool,
    Integer,
    Floating,
    String,
    Symbol,
}