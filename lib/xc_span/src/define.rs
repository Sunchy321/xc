use xc_macros::define_symbols;

define_symbols! {
    Keywords {
        Underscore: "_",

        Any: "any",
        As: "as",
        Async: "async",
        Await: "await",
        Auto: "auto",
        Bool: "bool",
        Break: "break",
        Catch: "catch",
        Char: "char",
        Class: "class",
        Cmp: "cmp",
        Const: "const",
        Continue: "continue",
        Deinit: "deinit",
        Defer: "defer",
        Do: "do",
        Dyn: "dyn",
        Else: "else",
        Enum: "enum",
        Extern: "extern",
        False: "false",
        Finally: "finally",
        Float: "float",
        For: "for",
        Func: "func",
        If: "if",
        Impl: "impl",
        Import: "import",
        In: "in",
        Init: "init",
        Infer: "infer",
        Int: "int",
        Internal: "internal",
        Is: "is",
        Lazy: "lazy",
        Let: "let",
        Macro: "macro",
        Match: "match",
        Mut: "mut",
        Never: "never",
        Nil: "nil",
        Operator: "operator",
        Partial: "partial",
        Private: "private",
        Public: "public",
        Return: "return",
        SelfKeyword: "self",
        Shl: "shl",
        ShlEq: "shl_eq",
        Shr: "shr",
        ShrEq: "shr_eq",
        Some: "some",
        Static: "static",
        String: "string",
        This: "this",
        Throw: "throw",
        True: "true",
        Try: "try",
        Type: "type",
        Typeof: "typeof",
        Uint: "uint",
        Void: "void",
        While: "while",

        DidSet: "didSet",
        Get: "get",
        Infix: "infix",
        Prefix: "prefix",
        Root: "root",
        Set: "set",
        Suffix: "suffix",
        Super: "super",
        Then: "then",
        WillSet: "willSet",
    }

    Operators {
        Predecessor: "+!",
        Successor: "-!",
        Not: "!",
        BitNegate: "'~",
        NullChain: "?",
        Multiply: "*",
        Divide: "/",
        Modulo: "%",
        Plus: "+",
        Subtract: "-",
        BitAnd: "'&",
        BitXor: "'^",
        BitOr: "'|",
        Range: "..",
        ClosedRange: "..=",
        Concat: "~",
        NullCoalescing: "??",
        Equal: "==",
        NotEqual: "!=",
        Less: "<",
        NotLess: "!<",
        LessEqual: "<=",
        Greater: ">",
        NotGreater: "!>",
        GreaterEqual: ">=",
        LessGreater: "<>",
        And: "&",
        Or: "|",
        Assign: "=",
        PlusAssign: "+=",
        SubtractAssign: "-=",
        MultiplyAssign: "*=",
        DivideAssign: "/=",
        ModuloAssign: "%=",
        BitAndAssign: "'&=",
        BitXorAssign: "'^=",
        BitOrAssign: "'|=",
        NullCoalescingAssign: "??=",
        Increment: "++",
        Decrement: "--",
        ConcatLeft: "~>",
        ConcatRight: "<~",
    }
}