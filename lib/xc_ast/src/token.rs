use crate::{literal::Literal, ty::TypeKind};
use xc_span::{symbol::kw, Identifier, Span, Symbol};

#[derive(Clone, Debug)]
pub enum CommentKind {
    Line,
    Block,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// `(` `)`
    Paren,
    /// `[` `]`
    Bracket,
    /// `{` `}`
    Brace,

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

#[derive(Clone, Debug, PartialEq)]
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
    LambdaArgNamed(Symbol),

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

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn dummy() -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span::DUMMY,
        }
    }

    pub fn to_identifier(&self) -> Option<Identifier> {
        match &self.kind {
            &TokenKind::Identifier(name) => Some(Identifier::new(name, self.span)),
            _ => None,
        }
    }

    pub fn is_identifier_and(&self, pred: impl FnOnce(Identifier) -> bool) -> bool {
        self.to_identifier().map_or(false, pred)
    }

    pub fn is_reserved_ident(&self) -> bool {
        self.is_identifier_and(|id| id.name.is_reserved())
    }

    pub fn is_path_start(&self) -> bool {
        match self.kind {
            TokenKind::ColonColon => true,
            TokenKind::Identifier(_) => !self.is_reserved_ident(),
            _ => false,
        }
    }

    pub fn is_keyword(&self, key: Symbol) -> bool {
        match self.to_identifier() {
            Some(id) => id.name == key,
            None => false,
        }
    }

    pub fn can_begin_expr(&self) -> bool {
        use TokenKind::*;

        match self.kind {
            At => false,
            Pound => false,
            Dollar => true,
            Semicolon => false,
            Colon => false,
            Comma => false,

            SymbolOpen => true,
            ColonColon => true,
            RightArrow => false,
            FatArrow => false,
            DotDotDot => false,

            Op(..) => true,

            OpenDelim(..) => true,
            CloseDelim(..) => false,

            Literal(..) => true,

            Identifier(name) => ident_can_begin_expr(name),

            LambdaArgUnnamed(..) => true,
            LambdaArgNamed(..) => true,

            Eof => false,
        }
    }

    pub fn to_builtin_type(&self) -> Option<TypeKind> {
        match self.kind {
            TokenKind::Identifier(sym) => {
                use TypeKind::*;

                let kind = match sym {
                    kw::Never => Never,
                    kw::Void => Void,
                    kw::Bool => Bool,
                    kw::Int => Int(None, 0, 0),
                    kw::Float => Float(None),
                    kw::String => String,
                    kw::Char => Char,
                    _ => return None,
                };

                Some(kind)
            }
            _ => None,
        }
    }
}

fn ident_can_begin_expr(name: Symbol) -> bool {
    !name.is_reserved()
        || [
            kw::Break,
            kw::Continue,
            kw::Do,
            kw::False,
            kw::For,
            kw::If,
            kw::Let,
            kw::Match,
            kw::Nil,
            kw::Operator,
            kw::Return,
            kw::This,
            kw::Throw,
            kw::True,
            kw::Try,
            kw::While,
        ]
        .contains(&name)
}
