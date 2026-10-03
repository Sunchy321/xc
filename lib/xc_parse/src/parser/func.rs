use thin_vec::{thin_vec, ThinVec};
use xc_ast::decl::{Qual, QualKind};
use xc_ast::func::{
    Func, FuncBody, FuncHeader, FuncParam, FuncReturn, FuncSignature, FuncTail, FuncThrow, ThisParam, ThisParamKind
};
use xc_ast::module::{Constness, Safety};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, IdentIsRaw, TokenKind};
use xc_ast::ty::{Type, TypeKind};
use xc_ast::Mutability;
use xc_span::symbol::{kw, op};
use xc_span::{Identifier, Span};

use super::attr::{ForceCollect, TrailingToken};
use super::parser::Parser;
use super::{ParseResult, SequenceSeparator};

impl<'a> Parser<'a> {
    pub(crate) fn parse_func(&mut self, quals: ThinVec<Qual>, _lo: Span) -> ParseResult<'a, Func> {
        // The `func` keyword is optional here: call sites such as `init`
        // constructors and impl members dispatch before it is consumed.
        self.eat_keyword(kw::Func);

        let ident = self.parse_func_name()?;

        let generics = self.parse_generic()?;

        let mut header = FuncHeader {
            safety: Safety::Default,
            constness: Constness::Default
        };

        for qual in quals {
            match qual.kind {
                QualKind::Unsafe => header.safety = Safety::Unsafe(qual.span),
                QualKind::Const => header.constness = Constness::Const(qual.span),
                _ => {}
            }
        }

        let sig = self.parse_func_signature(header)?;

        // A legacy `where` clause before the body is skipped.
        if matches!(self.token.to_identifier(), Some((ident, _)) if ident.as_str() == "where") {
            while !(self.check_noexpect(&TokenKind::OpenDelim(Delimiter::Brace))
                || self.check_noexpect(&TokenKind::Semicolon)
                || self.check_noexpect(&TokenKind::FatArrow)
                || self.token.kind == TokenKind::Eof)
            {
                self.next();
            }
        }

        let body = self.parse_func_body(true)?;

        Ok(Func { ident, generics, sig, body })
    }

    /// A parameter name: any single token that denotes an identifier,
    /// including keywords used as names (`(or)`, `(if)`, `(then)`).
    fn parse_param_name(&mut self) -> ParseResult<'a, Identifier> {
        if let Some((ident, _)) = self.token.to_identifier() {
            let span = self.token.span;
            self.next();
            return Ok(Identifier::new(ident.name, span));
        }

        self.parse_identifier()
    }

    fn parse_func_name(&mut self) -> ParseResult<'a, Identifier> {        if self.token.is_keyword(kw::Init) {
            let span = self.token.span;
            self.next();
            return Ok(Identifier::new(kw::Init, span));
        }

        // Operator spellings that may be used as function names (`compl`, ...).
        if let Some((ident, IdentIsRaw::No)) = self.token.to_identifier() {
            if ident.name == kw::Compl
                || ident.name == kw::Shl
                || ident.name == kw::Shr
                || ident.name == kw::Cmp
                || ident.name == kw::And
                || ident.name == kw::Or
                || ident.name == kw::Xor
            {
                let span = self.token.span;
                self.next();
                return Ok(Identifier::new(ident.name, span));
            }
        }

        self.parse_identifier()
    }

    fn parse_func_signature(&mut self, header: FuncHeader) -> ParseResult<'a, P<FuncSignature>> {
        Ok(P(FuncSignature {
            header,
            params: self.parse_func_params()?,
            tail: self.parse_func_tail()?,
        }))
    }

    pub(crate) fn parse_func_params(&mut self) -> ParseResult<'a, ThinVec<FuncParam>> {
        let mut first_param = true;

        let (params, _) = self.parse_paren_comma_seq(|this| {
            let result = this.parse_func_param(first_param);

            first_param = false;

            result
        })?;

        Ok(params)
    }

    fn parse_func_param(&mut self, _first_param: bool) -> ParseResult<'a, FuncParam> {
        let lo = self.token.span;

        let attrs = self.parse_outer_attrs()?;

        self.collect_tokens(attrs, ForceCollect::No, |this, attrs| {
            if let Some(mut param) = this.parse_self_param()? {
                param.attrs = attrs;

                return Ok((param, TrailingToken::None));
            }

            // Parameter qualifiers (`lazy`, `mut`, `ref`, `once`, ...).
            while this.eat_keyword_noexpect(kw::Lazy)
                || this.eat_keyword_noexpect(kw::Mut)
                || this.eat_keyword_noexpect(kw::Ref)
                || this.eat_keyword_noexpect(kw::Once)
            {}

            let mut param = this.parse_func_param_normal(lo)?;

            param.attrs = attrs;

            Ok((param, TrailingToken::None))
        })
    }

    fn parse_self_param(&mut self) -> ParseResult<'a, Option<FuncParam>> {
        let expect_this_keyword = |this: &mut Self| match this.token.to_identifier() {
            Some((ident, IdentIsRaw::No)) => {
                this.next();
                ident
            }
            _ => unreachable!(),
        };

        let is_isolated_this = |this: &Self, n| {
            this.is_keyword_ahead(n, &[kw::This])
                && this.look_ahead(n + 1, |t| t.kind != TokenKind::ColonColon)
        };

        let is_isolated_mut_this =
            |this: &Self, n| this.is_keyword_ahead(n, &[kw::Mut]) && is_isolated_this(this, n + 1);

        let parse_self_param_maybe_typed = |this: &mut Self, m| {
            let this_key = expect_this_keyword(this);
            let hi = this.prev_token.span;

            let this_kind = if this.eat(&TokenKind::Colon) {
                ThisParamKind::Explicit(m, this.parse_type()?)
            } else {
                ThisParamKind::Value(m)
            };

            Ok((this_kind, this_key, hi))
        };

        let lo = self.token.span;

        let (this_kind, this_key, hi) = match self.token.kind {
            TokenKind::Op(op) if op == op::Amp => {
                let this_kind = if is_isolated_this(self, 1) {
                    // `&this`
                    self.next();
                    ThisParamKind::Reference(Mutability::Immut)
                } else if is_isolated_mut_this(self, 1) {
                    // `&mut this`
                    self.next();
                    self.next();
                    ThisParamKind::Reference(Mutability::Mut)
                } else {
                    return Ok(None);
                };

                (this_kind, expect_this_keyword(self), self.prev_token.span)
            }

            TokenKind::Identifier(..) if is_isolated_this(self, 0) => {
                parse_self_param_maybe_typed(self, Mutability::Immut)?
            }

            TokenKind::Identifier(..) if is_isolated_mut_this(self, 0) => {
                parse_self_param_maybe_typed(self, Mutability::Mut)?
            }

            _ => return Ok(None),
        };

        let this_param = ThisParam {
            kind: this_kind,
            span: lo.to(hi),
        };

        let param = FuncParam::from_this_param(thin_vec![], this_param, this_key);

        Ok(Some(param))
    }

    fn parse_func_param_normal(&mut self, lo: Span) -> ParseResult<'a, FuncParam> {
        // A type-first variadic parameter: `T[]... values`.
        {
            let snapshot = self.create_snapshot();

            if let Ok(ty) = self.parse_type()
                && self.eat(&TokenKind::DotDotDot)
                && let Some((ident, _)) = self.token.to_identifier()
            {
                self.next();

                let span = lo.to(self.prev_token.span);

                return Ok(FuncParam {
                    attrs: thin_vec![],
                    name: None,
                    ident,
                    ty,
                    default: None,
                    span,
                });
            }

            self.restore_snapshot(snapshot);
        }

        // A parameter-list macro such as `#expandParameter(F::Parameter)`.
        if self.check(&TokenKind::Pound) {
            self.next();

            let (ident, _) = self
                .token
                .to_identifier()
                .ok_or_else(|| self.expected_tokens_error("expected a macro name"))?;
            self.next();

            if self.check(&TokenKind::OpenDelim(Delimiter::Paren))
                || self.check(&TokenKind::OpenDelim(Delimiter::Bracket))
            {
                let _ = self.parse_token_tree();
            }

            let ty = P(Type {
                kind: TypeKind::Named(ident.name),
                span: lo.to(self.prev_token.span),
            });

            return Ok(FuncParam {
                attrs: thin_vec![],
                name: None,
                ident,
                ty,
                default: None,
                span: lo.to(self.prev_token.span),
            });
        }

        let (name_ident, param_ident) = if self.eat(&TokenKind::OpenDelim(Delimiter::Paren)) {
            let name_ident = self.parse_param_name()?;

            self.expect(&TokenKind::CloseDelim(Delimiter::Paren))?;

            let param_ident = if self.check_identifier() {
                self.parse_identifier()?
            } else {
                name_ident.clone()
            };

            (Some(name_ident), param_ident)
        } else if self.token.is_keyword(kw::Underscore) {
            // `_: T` — a discarded parameter.
            let span = self.token.span;
            self.next();
            (None, Identifier::new(kw::Underscore, span))
        } else {
            let param_ident = self.parse_param_name()?;

            // `name ident: T` — a named parameter without parentheses.
            if self.token.to_identifier().is_some() {
                let ident = self.parse_param_name()?;
                (Some(param_ident), ident)
            } else {
                (None, param_ident)
            }
        };

        // `name: Type` or `name = default` (inferred type).
        let (ty, default) = if self.eat(&TokenKind::Colon) {
            let ty = self.parse_type()?;

            let default = if self.eat_operator(op::Assign) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            (ty, default)
        } else if self.eat_operator(op::Assign) {
            let default = self.parse_expr()?;

            let ty = P(Type {
                kind: TypeKind::ImplicitThis,
                span: lo.to(self.prev_token.span),
            });

            (ty, Some(default))
        } else {
            return Err(self.expected_tokens_error("expected `:` or `=`"));
        };

        let span = lo.to(self.prev_token.span);

        Ok(FuncParam {
            attrs: thin_vec![],
            name: name_ident,
            ident: param_ident,
            ty,
            default,
            span,
        })
    }

    fn parse_func_tail(&mut self) -> ParseResult<'a, FuncTail> {
        // parse throw specifiers.
        let throw = if self.eat_keyword(kw::Throw) {
            if self.eat(&TokenKind::OpenDelim(Delimiter::Paren)) {
                if self.eat(&TokenKind::CloseDelim(Delimiter::Paren)) {
                    // throw() — explicit nothrow
                    FuncThrow::Type(ThinVec::new())
                } else {
                    // throw(E1, E2, ...) — explicit type list
                    let (types, _) = self.parse_seq_to_end(
                        &TokenKind::CloseDelim(Delimiter::Paren),
                        SequenceSeparator::new(TokenKind::Comma),
                        |this| this.parse_type(),
                    )?;
                    FuncThrow::Type(types.into_iter().map(|ty| ty.into_inner()).collect())
                }
            } else {
                FuncThrow::Inferred
            }
        } else {
            FuncThrow::None
        };

        // `-> T`
        let ret = if self.eat(&TokenKind::RightArrow) {
            let ty = self.parse_type()?;
            FuncReturn::Type(ty)
        } else {
            FuncReturn::Inferred
        };

        Ok(FuncTail { throw, ret })
    }

    /// `in_decl` marks declaration contexts (function members and top-level
    /// declarations), where `=> Expression ;` and bodyless declarations
    /// require their trailing semicolon. Lambda expressions stay lenient: a
    /// `;` after their body belongs to the enclosing statement.
    pub(crate) fn parse_func_body(&mut self, in_decl: bool) -> ParseResult<'a, FuncBody> {
        let body = if self.check(&TokenKind::Semicolon) {
            self.expect_semi()?;
            FuncBody::Semicolon
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_block().map(FuncBody::Block)?
        } else if self.eat(&TokenKind::FatArrow) {
            let expr = self.parse_expr()?;

            if in_decl {
                self.expect_semi()?;
            } else {
                self.eat(&TokenKind::Semicolon);
            }

            FuncBody::Arrow(expr)
        } else if in_decl {
            return Err(self.expected_tokens_error("expected"));
        } else {
            FuncBody::Semicolon
        };

        Ok(body)
    }
}
