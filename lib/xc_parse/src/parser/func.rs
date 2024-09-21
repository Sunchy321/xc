use thin_vec::{thin_vec, ThinVec};
use xc_ast::decl::{Qual, QualKind};
use xc_ast::func::{
    Func, FuncBody, FuncHeader, FuncParam, FuncReturn, FuncSignature, FuncTail, FuncThrow, ThisParam, ThisParamKind
};
use xc_ast::module::{Constness, Safety};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, IdentIsRaw, TokenKind};
use xc_ast::Mutability;
use xc_span::symbol::{kw, op};
use xc_span::{Identifier, Span};

use super::attr::{ForceCollect, TrailingToken};
use super::parser::Parser;
use super::ParseResult;

impl<'a> Parser<'a> {
    pub(crate) fn parse_func(&mut self, quals: ThinVec<Qual>, _lo: Span) -> ParseResult<'a, Func> {
        self.expect_keyword(kw::Func)?;

        let ident = self.parse_func_name()?;

        // TODO: Generic

        let mut header = FuncHeader {
            safety: Safety::Default,
            constness: Constness::Default
        };

        for qual in quals {
            match qual.kind {
                QualKind::Unsafe => header.safety = Safety::Unsafe(qual.span),
                QualKind::Const => header.constness = Constness::Const(qual.span),
                _ => todo!()
            }
        }

        let sig = self.parse_func_signature(header)?;

        // TODO: require_body
        let body = self.parse_func_body(false)?;

        Ok(Func { ident, sig, body })
    }

    fn parse_func_name(&mut self) -> ParseResult<'a, Identifier> {
        self.parse_identifier()
    }

    fn parse_func_signature(&mut self, header: FuncHeader) -> ParseResult<'a, P<FuncSignature>> {
        Ok(P(FuncSignature {
            header,
            params: self.parse_func_params()?,
            tail: self.parse_func_tail()?,
        }))
    }

    fn parse_func_params(&mut self) -> ParseResult<'a, ThinVec<FuncParam>> {
        let mut first_param = true;

        let (params, _) = self.parse_paren_comma_seq(|this| {
            let result = this.parse_func_param(first_param).or_else(|err| todo!());

            first_param = false;

            result
        })?;

        Ok(params)
    }

    fn parse_func_param(&mut self, first_param: bool) -> ParseResult<'a, FuncParam> {
        let lo = self.token.span;

        let attrs = self.parse_attr_list()?;

        self.collect_tokens(attrs, ForceCollect::No, |this, attrs| {
            if let Some(mut param) = this.parse_self_param()? {
                param.attrs = attrs;

                let res = if first_param { Ok(param) } else { todo!() }?;

                return Ok((res, TrailingToken::None));
            }

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
            TokenKind::Op(op) if op == op::And => {
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
        let (name_ident, param_ident) = if self.eat(&TokenKind::OpenDelim(Delimiter::Paren)) {
            let name_ident = self.parse_identifier()?;

            self.expect(&TokenKind::CloseDelim(Delimiter::Paren))?;

            let param_ident = if self.check_identifier() {
                self.parse_identifier()?
            } else {
                name_ident.clone()
            };

            (Some(name_ident), param_ident)
        } else {
            let param_ident = self.parse_identifier()?;

            (None, param_ident)
        };

        self.expect(&TokenKind::Colon)?;

        let ty = self.parse_type()?;

        let span = lo.to(self.prev_token.span);

        Ok(FuncParam {
            attrs: thin_vec![],
            name: name_ident,
            ident: param_ident,
            ty,
            span,
        })
    }

    fn parse_func_tail(&mut self) -> ParseResult<'a, FuncTail> {
        // parse throw specifiers.
        let throw = if self.eat_keyword(kw::Throw) {
            if self.eat(&TokenKind::OpenDelim(Delimiter::Paren)) {
                todo!()
            } else {
                FuncThrow::Inferred
            }
        } else {
            FuncThrow::None
        };

        let ret = if self.eat(&TokenKind::RightArrow) {
            let ty = self.parse_type()?;
            FuncReturn::Type(ty)
        } else {
            FuncReturn::Inferred
        };

        Ok(FuncTail { throw, ret })
    }

    fn parse_func_body(&mut self, require_body: bool) -> ParseResult<'a, FuncBody> {
        let has_semi = if require_body {
            self.token.kind == TokenKind::Semicolon
        } else {
            self.check(&TokenKind::Semicolon)
        };

        let body = if has_semi {
            self.expect_semi()?;
            FuncBody::Semicolon
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_block_impl(false).map(FuncBody::Block)?
        } else if self.eat(&TokenKind::FatArrow) {
            let expr = self.parse_expr()?;

            if !self.eat(&TokenKind::Semicolon) {
                todo!()
            }

            FuncBody::Arrow(expr)
        } else {
            todo!()
        };

        Ok(body)
    }
}
