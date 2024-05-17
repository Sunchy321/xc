use std::fmt::Result;

use xc_ast::{token::{Delimiter, TokenKind}, ty::{Type, TypeKind}};
use xc_span::Span;

use super::{parser::Parser, HasTrailing};

impl Parser {
    pub fn parse_type(&mut self) -> Result<P<Type>, ()> {
        use TokenKind::*;

        let lo = self.token.span;

        let kind = if self.check(&OpenDelim(Delimiter::Paren)) {
            self.parse_type_tuple_or_paren(lo)
        } else {
            return Err(())
        };
    }

    pub fn parse_type_tuple_or_paren(&mut self, lo: Span) -> Result<TypeKind, ()>{
        let (types, trailing) = self.parse_paren_comma_seq(|p| {
            let ty = p.parse_type()?;
            Ok(ty)
        });

        if types.len() == 1 && matches!(trailing, HasTrailing::No) {
            let ty = types.into_iter().next().unwrap();

            Ok(TypeKind::Paren(ty))
        } else {
            Ok(TypeKind::Tuple(types))
        }
    }

    pub fn parse_typeof(&mut self) -> Result<TypeKind, ()> {
        self.expect(&TokenKind::OpenDelim(Delimiter::Paren));
        let expr = unimplemented!();
        self.expect(&TokenKind::CloseDelim(Delimiter::Paren));
        Ok(TypeKind::Typeof(expr))
    }
}