use crate::token::CustomOp;

#[derive(Clone)]
pub enum InfixOpKind {
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

    /// `shl`
    LeftShift,
    /// `shr`
    RightShift,

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

    /// `cmp`
    Cmp,
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
    /// `<>`
    LessGreater,

    /// `in`
    In,
    /// `!in`
    NotIn,

    /// `is`
    Is,
    /// `!is`
    NotIs,

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
    /// `shl_eq`
    LeftShiftAssign,
    /// `shr_eq`
    RightShiftAssign,
    /// `'&=`
    BitAndAssign,
    /// `'^=`
    BitXorAssign,
    /// `'|=`
    BitOrAssign,
    /// `??=`
    NullCoalescingAssign,
    /// `<~`
    ConcatLeft,
    /// `~>`
    ConcatRight,

    Custom(CustomOp)
}

#[derive(Clone)]
pub struct InfixOp {
    pub kind: InfixOpKind,
}

impl InfixOpKind {
    pub fn as_str(&self) -> &str {
        use InfixOpKind::*;
        match self {
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            Plus => "+",
            Subtract => "-",
            LeftShift => "shl",
            RightShift => "shr",
            BitAnd => "'&",
            BitXor => "'^",
            BitOr => "'|",
            Range => "..",
            ClosedRange => "..=",
            Concat => "~",
            NullCoalescing => "??",
            Cmp => "cmp",
            Equal => "==",
            NotEqual => "!=",
            Less => "<",
            NotLess => "!<",
            Greater => ">",
            NotGreater => "!>",
            LessGreater => "<>",
            In => "in",
            NotIn => "!in",
            Is => "is",
            NotIs => "!is",
            And => "&",
            Or => "|",
            Assign => "=",
            PlusAssign => "+=",
            SubtractAssign => "-=",
            MultiplyAssign => "*=",
            DivideAssign => "/=",
            ModuloAssign => "%=",
            LeftShiftAssign => "shl_eq",
            RightShiftAssign => "shr_eq",
            BitAndAssign => "'&=",
            BitXorAssign => "'^=",
            BitOrAssign => "'|=",
            NullCoalescingAssign => "??=",
            ConcatLeft => "<~",
            ConcatRight => "~>",
            Custom(op) => op.as_str(),
        }
    }
}