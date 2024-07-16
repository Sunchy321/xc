use core::arch;
use std::mem;

use thin_vec::{thin_vec, ThinVec};
use xc_ast::decl::{Decl, DeclKind, Qual, QualKind};
use xc_ast::func::{
    Func, FuncBody, FuncParam, FuncReturn, FuncSignature, FuncTail, FuncThrow, ThisParam,
    ThisParamKind,
};
use xc_ast::id::OperatorKind;
use xc_ast::import::{Import, ImportItem, ImportKind, ImportPathSegment};
use xc_ast::literal::LiteralKind;
use xc_ast::module::{Module, VisKind, Visibility};
use xc_ast::ptr::P;
use xc_ast::stmt::Block;
use xc_ast::token::{Delimiter, IdentIsRaw, Token, TokenKind};
use xc_ast::{ty, Mutability};
use xc_span::symbol::{kw, op};
use xc_span::{Identifier, Span};

use crate::parser::attr::{self, ForceCollect, TrailingToken};

use super::{parser::Parser, ParseResult};
use super::{Case, SequenceSeparator, TokenExpectType};

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> ParseResult<'a, Module> {
        todo!()
    }

    pub fn parse_decl(&mut self) -> ParseResult<'a, Option<Decl>> {
        let lo = self.token.span;

        let mut quals = self.parse_qual_list()?;

        let kind = if self.eat(&TokenKind::ColonColon) {
            let quals = mem::replace(&mut quals, thin_vec![]);

            Some(DeclKind::QualDecl(quals))
        } else {
            self.parse_decl_kind(Case::Sensitive)?
        };

        if let Some(kind) = kind {
            let span = lo.to(self.prev_token.span);

            let decl = Decl { kind, quals, span };

            Ok(Some(decl))
        } else {
            Ok(None)
        }
    }

    fn parse_qual_list(&mut self) -> ParseResult<'a, ThinVec<Qual>> {
        let mut quals = thin_vec![];

        while let Some(qual) = self.parse_qual()? {
            quals.push(qual);
        }

        Ok(quals)
    }

    fn parse_qual(&mut self) -> ParseResult<'a, Option<Qual>> {
        let lo = self.token.span;

        macro_rules! check_qual {
            ($ident:ident) => {
                if self.eat_keyword(kw::$ident) {
                    return Some(QualKind::$ident);
                }
            };
        }

        let kind = if let Some(vis) = self.parse_vis()? {
            QualKind::Vis(vis)
        } else {
            let mut check_kind = || {
                check_qual!(Extern);
                check_qual!(Async);
                check_qual!(Static);
                check_qual!(Const);
                check_qual!(Unsafe);
                check_qual!(Partial);

                None
            };

            if let Some(kind) = check_kind() {
                kind
            } else {
                return Ok(None);
            }
        };

        let span = lo.to(self.prev_token.span);

        let qual = Qual { kind, span };

        Ok(Some(qual))
    }

    fn parse_vis(&mut self) -> ParseResult<'a, Option<Visibility>> {
        let vis = if self.eat_keyword(kw::Public) {
            Visibility {
                kind: VisKind::Public,
                span: self.prev_token.span,
            }
        } else if self.eat_keyword(kw::Internal) {
            Visibility {
                kind: VisKind::Internal,
                span: self.prev_token.span,
            }
        } else if self.eat_keyword(kw::Private) {
            Visibility {
                kind: VisKind::Private,
                span: self.prev_token.span,
            }
        } else {
            return Ok(None);
        };

        Ok(Some(vis))
    }

    fn parse_decl_kind(&mut self, case: Case) -> ParseResult<'a, Option<DeclKind>> {
        let lo = self.token.span;

        let info = if self.eat_keyword_case(kw::Import, case) {
            self.parse_import(lo)?
        } else if self.eat_keyword_case(kw::Func, case) {
            self.parse_func(lo)?
        } else {
            return Ok(None);
        };

        Ok(Some(info))
    }

    fn parse_import(&mut self, lo: Span) -> ParseResult<'a, DeclKind> {
        let (path, ..) = self.parse_seq_before(
            &[&TokenKind::Colon, &TokenKind::Semicolon],
            SequenceSeparator::new(TokenKind::Op(op::Dot)),
            TokenExpectType::Yes,
            |this| this.parse_import_path(),
        )?;

        let import = match self.token.kind {
            TokenKind::Colon => {
                self.next();

                let (items, ..) = self.parse_seq_before_end(
                    &TokenKind::Semicolon,
                    SequenceSeparator::new(TokenKind::Comma),
                    |this| this.parse_import_item(),
                )?;

                Import {
                    kind: ImportKind::Normal,
                    path,
                    items: Some(items),
                    span: lo.to(self.prev_token.span),
                }
            }

            TokenKind::Semicolon => {
                self.next();

                Import {
                    kind: ImportKind::Full,
                    path,
                    items: None,
                    span: lo.to(self.prev_token.span),
                }
            }

            _ => {
                todo!()
            }
        };

        Ok(DeclKind::Import(import))
    }

    fn parse_import_path(&mut self) -> ParseResult<'a, ImportPathSegment> {
        let segment = match self.token.kind {
            TokenKind::Identifier(ident, ..) => {
                let span = self.token.span;

                self.next();

                ImportPathSegment::from_ident(Identifier::new(ident, span))
            }

            TokenKind::Literal(lit)
                if matches!(
                    lit.kind,
                    LiteralKind::String | LiteralKind::RawString { .. }
                ) =>
            {
                let span = self.token.span;

                self.next();

                ImportPathSegment::from_string(lit.value, span)
            }

            _ => todo!(),
        };

        Ok(segment)
    }

    fn parse_import_item(&mut self) -> ParseResult<'a, ImportItem> {
        let item = if self.eat_operator(op::Multiply) {
            ImportItem::Star
        } else if self.eat_keyword(kw::Operator) {
            let kind = if self.eat_keyword(kw::Infix) {
                Some(OperatorKind::Infix)
            } else if self.eat_keyword(kw::Prefix) {
                Some(OperatorKind::Prefix)
            } else if self.eat_keyword(kw::Suffix) {
                Some(OperatorKind::Suffix)
            } else {
                None
            };

            match self.token.kind {
                TokenKind::Op(op) => {
                    self.next();

                    ImportItem::Operator(op, kind)
                }

                _ => ImportItem::OperatorAll,
            }
        } else if self.check_identifier() {
            let ident = self.parse_identifier()?;

            if self.eat_keyword(kw::As) {
                let alias = self.parse_identifier()?;

                ImportItem::Ident(ident, Some(alias))
            } else {
                ImportItem::Ident(ident, None)
            }
        } else {
            todo!()
        };

        Ok(item)
    }

    fn check_func_front_matter(&mut self, case: Case) -> bool {
        self.check_keyword_case(kw::Func, case)
    }

    fn parse_func(&mut self, lo: Span) -> ParseResult<'a, DeclKind> {
        let func_span = lo;

        let ident = self.parse_func_name()?;

        // TODO: Generic

        let sig = self.parse_func_header()?;

        // TODO: require_body
        let body = self.parse_func_body(false)?;

        let hi = self.prev_token.span;

        let span = lo.to(hi);

        let func = Func { sig, body };

        Ok(DeclKind::Func(P(func)))
    }

    fn parse_func_name(&mut self) -> ParseResult<'a, Identifier> {
        self.parse_identifier()
    }

    fn parse_func_header(&mut self) -> ParseResult<'a, P<FuncSignature>> {
        Ok(P(FuncSignature {
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

        self.expect(&TokenKind::Colon);

        let ty = self.parse_type()?;

        let span = lo.to(self.prev_token.span);

        Ok(FuncParam {
            attrs: thin_vec![],
            name: name_ident,
            ident: param_ident,
            ty,
            span
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

    fn parse_type_decl(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }

    fn parse_trait(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }

    fn parse_impl(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }
}
