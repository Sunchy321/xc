use thin_vec::{thin_vec, ThinVec};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::ty::{Member, StructType, Type, TypeKind};
use xc_ast::Mutability;
use xc_span::symbol::{kw, op};
use xc_span::{Span, Symbol};

use super::{
    parser::Parser, ConsumeClosingDelim, HasTrailing, ParseResult, SequenceSeparator,
    TokenExpectType,
};

impl<'a> Parser<'a> {
    pub fn make_type(&self, kind: TypeKind, span: Span) -> P<Type> {
        P(Type { kind, span })
    }

    pub fn parse_type(&mut self) -> ParseResult<'a, P<Type>> {
        let lo = self.token.span;
        let mut ty = self.parse_type_result()?;

        // Function types: `T -> U`, `(T, ...) mut -> U`, `(T) throw(E) -> U`.
        loop {
            let has_quals = self.token.is_keyword(kw::Mut)
                || self.token.is_keyword(kw::Once)
                || self.token.is_keyword(kw::Async)
                || self.token.is_keyword(kw::Unsafe)
                || self.token.is_keyword(kw::Throw);

            if has_quals {
                let snapshot = self.create_snapshot();

                while self.eat_keyword_noexpect(kw::Mut)
                    || self.eat_keyword_noexpect(kw::Once)
                    || self.eat_keyword_noexpect(kw::Async)
                    || self.eat_keyword_noexpect(kw::Unsafe)
                {}

                if self.eat_keyword_noexpect(kw::Throw)
                    && self.check_noexpect(&TokenKind::OpenDelim(Delimiter::Paren))
                {
                    self.next();
                    self.consume_block(Delimiter::Paren, ConsumeClosingDelim::Yes);
                }

                if !self.check_noexpect(&TokenKind::RightArrow) {
                    self.restore_snapshot(snapshot);
                    break;
                }
            }

            let arrow_lo = self.token.span;

            if self.eat(&TokenKind::RightArrow) {
                if self.paren_sep_is_semi {
                    return Err(self.diag_ctx().struct_span_error(
                        arrow_lo,
                        "function type parameters are separated by `,`, not `;`",
                    ));
                }

                let ret = self.parse_type()?;
                let span = lo.to(self.prev_token.span);
                ty = self.make_type(TypeKind::Fn(ty, ret), span);
            } else {
                break;
            }
        }

        Ok(ty)
    }

    pub fn parse_type_result(&mut self) -> ParseResult<'a, P<Type>> {
        let lo = self.token.span;
        let ty = self.parse_type_prefix()?;

        // `T !! E` — the `!!` token may arrive as one operator or two `!`s.
        let is_bang_bang = match self.token.kind {
            TokenKind::Op(sym) if sym.as_str() == "!!" => {
                self.next();
                true
            }
            TokenKind::Op(sym) if sym == op::Exclamation => {
                if self.look_ahead(1, |t| matches!(t.kind, TokenKind::Op(s2) if s2 == op::Exclamation)) {
                    self.next();
                    self.next();
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if is_bang_bang {
            let err_ty = self.parse_type_prefix()?;
            let span = lo.to(self.prev_token.span);
            return Ok(self.make_type(TypeKind::Result(ty, err_ty), span));
        }

        // Legacy `T throw E` spelling.
        if self.eat_keyword_noexpect(kw::Throw) {
            let err_ty = self.parse_type_prefix()?;
            let span = lo.to(self.prev_token.span);
            Ok(self.make_type(TypeKind::Result(ty, err_ty), span))
        } else {
            Ok(ty)
        }
    }

    pub fn parse_type_prefix(&mut self) -> ParseResult<'a, P<Type>> {
        let lo = self.token.span;

        let kind = if self.eat_keyword_noexpect(kw::Class) {
            TypeKind::Class(self.parse_type_suffix()?)
        } else if self.eat_keyword_noexpect(kw::Impl) {
            TypeKind::ImplType(self.parse_type_suffix()?)
        } else if self.eat_keyword_noexpect(kw::Dyn) {
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

                // `T??` — two optional suffixes, lexed as a single `??`.
                Op(sym) if sym == op::NullCoalescing => {
                    self.next();
                    let span = lo.to(self.prev_token.span);
                    ty = self.make_type(Optional(ty), span);
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

                // `T&` — a shared reference.
                Op(sym) if sym == op::Amp => {
                    self.next();
                    let span = lo.to(self.prev_token.span);
                    ty = self.make_type(Reference(ty, Mutability::Immut), span);
                }

                // `T mut&` — a mutable reference.
                Identifier(sym, _) if sym == kw::Mut => {
                    if self.look_ahead(1, |t| matches!(t.kind, Op(sym) if sym == op::Amp)) {
                        self.next();
                        self.next();
                        let span = lo.to(self.prev_token.span);
                        ty = self.make_type(Reference(ty, Mutability::Mut), span);
                    } else {
                        break;
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

        // Reset the field-list separator record; only a parenthesized field
        // list sets it below.
        self.paren_sep_is_semi = false;

        let (mut kind, mut text) = if let Some(kind) = self.token.to_builtin_type() {
            let text = self
                .token
                .to_identifier()
                .map(|(ident, _)| ident.as_str().to_string())
                .unwrap_or_default();

            self.next();
            (kind, text)
        } else if self.eat_keyword_noexpect(kw::SelfKeyword) {
            (TypeKind::SelfType, "self".to_string())
        } else if self.eat_keyword_noexpect(kw::Typeof) {
            self.expect(&OpenDelim(Delimiter::Paren))?;
            let expr = self.parse_expr()?;
            self.expect(&CloseDelim(Delimiter::Paren))?;
            (TypeKind::Typeof(expr), "typeof".to_string())
        } else {
            match self.token.kind {
                OpenDelim(Delimiter::Paren) => (self.parse_type_struct_or_paren()?, String::new()),

                // `<T as Trait>::Assoc` — the qualified projection form.
                Op(sym) if sym == op::Less => {
                    self.next();

                    let base = self.parse_type()?;

                    if !self.eat_keyword_noexpect(kw::As) {
                        return Err(self.expected_tokens_error("expected `as`"));
                    }

                    let (trait_seg, _) = self
                        .token
                        .to_identifier()
                        .ok_or_else(|| self.expected_tokens_error("expected"))?;
                    self.next();

                    if !self.eat_greater() {
                        return Err(self.expected_tokens_error("expected `>`"));
                    }

                    if !self.eat(&TokenKind::ColonColon) {
                        return Err(self.expected_tokens_error("expected `::`"));
                    }

                    let (assoc, _) = self
                        .token
                        .to_identifier()
                        .ok_or_else(|| self.expected_tokens_error("expected"))?;
                    self.next();

                    (
                        TypeKind::Qualified(base, trait_seg.name, assoc.name),
                        String::new(),
                    )
                }

                Identifier(..) => {
                    let (ident, _) = self.token.to_identifier().unwrap();

                    let text = ident.as_str().to_string();
                    let name = ident.name;

                    self.next();
                    (TypeKind::Named(name), text)
                }

                _ => return Err(self.expected_tokens_error("expected")),
            }
        };

        // Generic arguments (`Foo<T, U>`, `int<8>`) and qualified paths
        // (`core::Iterator`, `T::Target`).
        let mut args = ThinVec::new();
        let mut has_args = false;

        if self.token.kind == Op(op::Less) {
            self.next();
            has_args = true;

            if !self.eat_greater() {
                let (parsed, ..) = self.parse_seq_before(
                    &[&TokenKind::Op(op::Greater)],
                    SequenceSeparator::new(TokenKind::Comma),
                    TokenExpectType::Yes,
                    |this| this.parse_generic_arg(),
                )?;

                args = parsed;

                if !self.eat_greater() {
                    return Err(self.expected_tokens_error("expected"));
                }
            }
        }

        if !text.is_empty() {
            let mut t = text;

            while self.check_noexpect(&ColonColon) {
                self.next();

                let (ident, _) = self
                    .token
                    .to_identifier()
                    .ok_or_else(|| self.expected_tokens_error("expected"))?;

                t.push_str("::");
                t.push_str(ident.as_str());

                self.next();
            }

            if has_args {
                kind = TypeKind::Generic(Symbol::intern(&t), args);
            } else if t.contains("::") {
                kind = TypeKind::Named(Symbol::intern(&t));
            }

            text = t;
        }

        let _ = text;

        let span = lo.to(self.prev_token.span);
        let ty = self.make_type(kind, span);
        Ok(ty)
    }

    /// A generic argument: a type, a lifetime, a const value, or a named
    /// binding such as `Output = T` (the name is dropped).
    fn parse_generic_arg(&mut self) -> ParseResult<'a, P<Type>> {
        if let TokenKind::Lifetime(sym) = self.token.kind {
            let span = self.token.span;
            self.next();
            Ok(self.make_type(TypeKind::Named(sym), span))
        } else if let TokenKind::Literal(lit) = self.token.kind {
            let span = self.token.span;
            self.next();
            Ok(self.make_type(TypeKind::Named(lit.value), span))
        } else if self.token.is_identifier()
            && self.look_ahead(1, |t| matches!(t.kind, TokenKind::Op(sym) if sym == op::Assign))
        {
            self.next();
            self.next();
            self.parse_type()
        } else {
            self.parse_type()
        }
    }

    pub fn parse_type_struct_or_paren(&mut self) -> ParseResult<'a, TypeKind> {
        let (members, trailing, sep_is_semi) = self.parse_paren_sep_seq(|this| {
            let is_mut = this.eat_keyword(kw::Mut);

            // Parameter qualifiers inside a function type (`lazy`, ...).
            let _lazy = this.eat_keyword(kw::Lazy);

            // `(name): Type` — a named parameter in a function type.
            let key = if this.eat(&TokenKind::OpenDelim(Delimiter::Paren)) {
                let key = this.parse_identifier()?;
                this.expect(&TokenKind::CloseDelim(Delimiter::Paren))?;

                if this.eat(&TokenKind::Colon) {
                    Some(key.name)
                } else {
                    return Err(this.expected_tokens_error("expected `:`"));
                }
            } else {
                this.parse_key()?
            };

            let item = if let Some(key) = key {
                Member::Named(key, this.parse_type()?, is_mut)
            } else {
                // `...T` — an expansion member.
                let _expansion = this.eat(&TokenKind::DotDotDot);

                Member::Ordinal(this.parse_type()?, is_mut)
            };

            Ok(item)
        })?;

        // Record the separator kind so a following `->` can reject `;`.
        self.paren_sep_is_semi = sep_is_semi;

        let kind = if members.len() == 1 && matches!(trailing, HasTrailing::No) {
            match members.into_iter().next().unwrap() {
                Member::Ordinal(ty, _) => TypeKind::Paren(ty),
                ty => TypeKind::Struct(StructType { members: thin_vec![ty] }),
            }
        } else {
            TypeKind::Struct(StructType { members })
        };

        Ok(kind)
    }
}
