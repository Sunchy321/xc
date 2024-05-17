use xc_ast::{expr::{CastType, Expr, ExprKind, PrimaryExpr}, ptr::P, token::{Delimiter, TokenKind}, ty::Type};
use xc_span::Symbol;

use super::parser::Parser;

impl Parser {
    pub fn parse_expr(&mut self) {
        unimplemented!()
    }

    pub fn collect_tokens(&mut self) {
        unimplemented!()
    }

    pub fn collect_atom(&mut self) -> Option<ExprAtom> {
        use TokenKind::*;

        let atom = match self.token.kind {
            At | Pound | Semicolon | Colon | Comma | RightArrow | FatArrow | DotDotDot => return None,
            Dollar | SymbolOpen | ColonColon => ExprAtom::Primary(self.parse_primary_expr()),

            OpenDelim(delim) => match delim {
                Delimiter::Paren | Delimiter::Bracket | Delimiter::Brace | Delimiter::DictBound => ExprAtom::Primary(self.parse_primary_expr()),
                Delimiter::Invisible => return None,
            }

            CloseDelim(_) => return None,

            Literal(_) => ExprAtom::Primary(self.parse_primary_expr()),

            Identifier(_) => ExprAtom::Primary(self.parse_primary_expr()),
            LambdaArgNamed(_) => ExprAtom::Primary(self.parse_primary_expr()),
            LambdaArgSelf => ExprAtom::Primary(self.parse_primary_expr()),

            Eof => return None,

            Op(op) => {
                self.next();
                ExprAtom::Op(op)
            }
        };

        Some(atom)
    }

    pub fn parse_primary_expr(&mut self) -> P<Expr> {
        use TokenKind::*;

        let kind = match self.token.kind {
            Dollar => PrimaryExpr::Dollar,
            At | Pound | Semicolon | Colon | Comma => unreachable!()
        };

        self.make_expr(ExprKind::Primary(kind))
    }

    pub fn
}

#[derive(Clone)]
pub enum ExprAtom {
    Primary(P<Expr>),

    MemberAccess(Symbol),
    Cast(P<Type>, CastType),
    DotAwait,
    DotCatch,

    Op(Symbol),
}

impl ExprAtom {
    pub fn can_be_primary_expr(&self) -> bool {
        match self {
            Self::MemberAccess(_) => true,
            Self::Cast(..) | Self::DotAwait | Self::DotCatch => false,
            _ => false
        }
    }

    pub fn can_be_suffix_op(&self) -> bool {
        match self {
            Self::MemberAccess(_) => true,
            Self::Cast(..) | Self::DotAwait | Self::DotCatch => false,
            _ => false
        }
    }

    pub fn can_be_prefix_op(&self) -> bool {
        match self {
            Self::MemberAccess(_) => false,
            Self::Cast(..) | Self::DotAwait | Self::DotCatch => true,
            _ => false
        }
    }

    pub fn can_be_infix_op(&self) -> bool {
        match self {
            Self::MemberAccess(_) => false,
            Self::Cast(..) | Self::DotAwait | Self::DotCatch => false,
            _ => false
        }
    }
}