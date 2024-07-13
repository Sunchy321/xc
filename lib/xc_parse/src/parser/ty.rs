use xc_ast::literal::LiteralKind;
use xc_ast::module::VisKind;
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, IdentIsRaw, TokenKind};
use xc_ast::ty::{Type, TypeKind};
use xc_span::symbol::{kw, op};
use xc_span::{Span, Symbol};

use super::{parser::Parser, HasTrailing, ParseResult};

impl<'a> Parser<'a> {
    pub fn make_type(&self, kind: TypeKind, span: Span) -> P<Type> {
        P(Type { kind, span })
    }

    pub fn parse_type(&mut self) -> ParseResult<'a, P<Type>> {
        self.parse_type_result()
    }

    pub fn parse_type_result(&mut self) -> ParseResult<'a, P<Type>> {
        let lo = self.token.span;
        let ty = self.parse_type_prefix()?;

        if self.eat_keyword_noexpect(kw::Throw) {
            let err_ty = self.parse_type_prefix()?;

            let span = lo.to(self.prev_token.span);

            Ok(self.make_type(TypeKind::Result(ty, err_ty), span))
        } else {
            Ok(ty)
        }
    }

    pub fn parse_type_prefix(&mut self) -> ParseResult<'a, P<Type>> {
        use TokenKind::*;

        let lo = self.token.span;

        let kind = if self.eat_keyword(kw::Class) {
            self.next();
            TypeKind::Class(self.parse_type_suffix()?)
        } else if self.eat_keyword(kw::Impl) {
            self.next();
            TypeKind::ImplType(self.parse_type_suffix()?)
        } else if self.eat_keyword(kw::Dyn) {
            self.next();
            TypeKind::DynType(self.parse_type_suffix()?)
        } else {
            return self.parse_type_suffix();
        };

        let span = lo.to(self.prev_token.span);

        Ok(self.make_type(kind, span))
    }

    pub fn parse_type_suffix(&mut self) -> ParseResult<'a, P<Type>> {
        use TokenKind::*;
        use TypeKind::*;

        let lo = self.token.span;

        let mut ty = self.parse_type_primary()?;

        loop {
            match self.token.kind {
                Op(sym) if sym == op::Question => {
                    self.next();
                    let span = lo.to(self.prev_token.span);
                    ty = self.make_type(Optional(ty), span);
                }

                OpenDelim(Delimiter::Bracket) => {
                    self.next();

                    if matches!(self.token.kind, CloseDelim(Delimiter::Bracket)) {
                        self.next();
                        let span = lo.to(self.prev_token.span);
                        ty = self.make_type(Array(ty), span);
                    } else {
                        let index_ty = self.parse_type()?;
                        self.expect(&CloseDelim(Delimiter::Bracket))?;
                        let span = lo.to(self.prev_token.span);
                        ty = self.make_type(Dict(ty, index_ty), span);
                    }
                }

                _ => break,
            }
        }

        Ok(ty)
    }

    pub fn parse_type_primary(&mut self) -> ParseResult<'a, P<Type>> {
        use TokenKind::*;

        let lo = self.token.span;

        let kind = if let Some(kind) = self.token.to_builtin_type() {
            self.next();
            kind
        } else {
            match self.token.kind {
                OpenDelim(Delimiter::Paren) => self.parse_type_tuple_or_paren(lo)?,
                SymbolOpen => unimplemented!("symbol type"),

                Literal(sym) => match sym.kind {
                    LiteralKind::SymbolLit => TypeKind::SymbolType(sym.value),

                    _ => {
                        let msg = format!("expected type, found {}", unimplemented!());
                        let mut err = self.diag_ctx().struct_span_error(self.token.span, msg);

                        err.span_label(self.token.span, "expected type");
                        return Err(err);
                    }
                },

                _ => unimplemented!(),
            }
        };

        let span = lo.to(self.prev_token.span);
        let ty = self.make_type(kind, span);
        Ok(ty)
    }

    pub fn parse_type_tuple_or_paren(&mut self, lo: Span) -> ParseResult<'a, TypeKind> {
        let (types, trailing) = self.parse_paren_comma_seq(|p| {
            let ty = p.parse_type()?;
            Ok(ty)
        })?;

        if types.len() == 1 && matches!(trailing, HasTrailing::No) {
            let ty = types.into_iter().next().unwrap();

            Ok(TypeKind::Paren(ty))
        } else {
            Ok(TypeKind::Tuple(types))
        }
    }
}
