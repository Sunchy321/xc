use std::mem;

use thin_vec::{thin_vec, ThinVec};
use xc_ast::ast::DelimitedArgs;
use xc_ast::decl::{
    Decl, DeclKind, EnumDecl, EnumVariant, ImplBody, ImplDecl, ImplItem, MacroDecl, ModuleDecl,
    Qual, QualKind, TypeDecl, TypeDeclBody,
};
use xc_ast::id::OperatorKind;
use xc_ast::import::{Import, ImportItem, ImportKind, ImportPathSegment};
use xc_ast::literal::LiteralKind;
use xc_ast::module::{Module, VisKind, Visibility};
use xc_ast::path::{Path, PathSegment};
use xc_ast::ptr::P;
use xc_ast::r#trait::{MemberItem, PropBody};
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::tokenstream::TokenTree;
use xc_ast::ty::{Member, StructType, TypeKind};
use xc_span::symbol::{kw, op};
use xc_span::{Identifier, Span};

use super::{parser::Parser, ParseResult, Restrictions};
use super::{Case, SequenceSeparator, TokenExpectType};

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> ParseResult<'a, Module> {
        todo!()
    }

    pub fn parse_decl(&mut self) -> ParseResult<'a, Option<Decl>> {
        let lo = self.token.span;

        let attrs = self.parse_outer_attrs()?;

        let mut quals = self.parse_qual_list()?;

        let kind = if self.eat(&TokenKind::ColonColon) {
            let quals = mem::replace(&mut quals, thin_vec![]);

            Some(DeclKind::QualDecl(quals))
        } else {
            self.parse_decl_kind(quals, Case::Sensitive)?
        };

        if let Some(kind) = kind {
            let span = lo.to(self.prev_token.span);

            let decl = Decl {
                attrs,
                kind,
                span,
            };

            Ok(Some(decl))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn parse_qual_list(&mut self) -> ParseResult<'a, ThinVec<Qual>> {
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
                check_qual!(Async);
                check_qual!(Const);
                check_qual!(Extern);
                check_qual!(Linear);
                check_qual!(Partial);
                check_qual!(Static);
                check_qual!(Unsafe);

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

    fn parse_decl_kind(
        &mut self,
        quals: ThinVec<Qual>,
        case: Case,
    ) -> ParseResult<'a, Option<DeclKind>> {
        let lo = self.token.span;

        let info = if self.check_keyword_case(kw::Module, case) {
            self.parse_module_decl(quals)?
        } else if self.check_keyword_case(kw::Import, case) {
            self.parse_import(quals, lo)?
        } else if self.check_keyword_case(kw::Func, case) {
            DeclKind::Func(P(self.parse_func(quals, lo)?))
        } else if self.check_keyword_case(kw::Trait, case) {
            DeclKind::Trait(P(self.parse_trait(quals, lo)?))
        } else if self.check_keyword_case(kw::Type, case) {
            self.parse_type_decl(quals, false)?
        } else if self.check_keyword_case(kw::Class, case) {
            self.parse_type_decl(quals, true)?
        } else if self.check_keyword_case(kw::Enum, case) {
            self.parse_enum_decl(quals)?
        } else if self.check_keyword_case(kw::Impl, case) {
            self.parse_impl(quals)?
        } else if self.check_keyword_case(kw::Macro, case) {
            self.parse_macro_decl(quals)?
        } else {
            return Ok(None);
        };

        Ok(Some(info))
    }

    fn parse_module_decl(&mut self, _quals: ThinVec<Qual>) -> ParseResult<'a, DeclKind> {
        let lo = self.token.span;

        self.expect_keyword(kw::Module)?;

        let path = if self.check_noexpect(&TokenKind::OpenDelim(Delimiter::Brace)) {
            None
        } else {
            let (segments, ..) = self.parse_seq_before(
                &[
                    &TokenKind::Semicolon,
                    &TokenKind::OpenDelim(Delimiter::Brace),
                    &TokenKind::Op(op::Assign),
                ],
                SequenceSeparator::new(TokenKind::Op(op::Dot)),
                TokenExpectType::Yes,
                |this| this.parse_module_path_segment(),
            )?;

            Some(Path {
                segments,
                span: lo.to(self.prev_token.span),
            })
        };

        // `module name = this.path;`
        let alias = if self.eat_operator(op::Assign) {
            let lo = self.token.span;

            let (segments, ..) = self.parse_seq_before(
                &[&TokenKind::Semicolon],
                SequenceSeparator::new(TokenKind::Op(op::Dot)),
                TokenExpectType::Yes,
                |this| this.parse_module_path_segment(),
            )?;

            Some(Path {
                segments,
                span: lo.to(self.prev_token.span),
            })
        } else {
            None
        };

        let body = if alias.is_some() {
            self.expect_semi()?;
            None
        } else if self.eat(&TokenKind::OpenDelim(Delimiter::Brace)) {
            let mut decls = ThinVec::new();

            loop {
                if self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
                    break;
                }
                if self.token.kind == TokenKind::Eof {
                    break;
                }

                match self.parse_decl()? {
                    Some(decl) => decls.push(decl),
                    None => return Err(self.expected_tokens_error("expected")),
                }
            }

            Some(decls)
        } else {
            self.expect_semi()?;
            None
        };

        Ok(DeclKind::Module(P(ModuleDecl {
            path,
            alias,
            body,
            span: lo.to(self.prev_token.span),
        })))
    }

    fn parse_module_path_segment(&mut self) -> ParseResult<'a, PathSegment> {
        match self.token.to_identifier() {
            Some((ident, _)) => {
                self.next();
                Ok(PathSegment::from_ident(ident))
            }
            None => Err(self.expected_tokens_error("expected")),
        }
    }

    fn parse_import(&mut self, _quals: ThinVec<Qual>, lo: Span) -> ParseResult<'a, DeclKind> {
        self.expect_keyword(kw::Import)?;

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

                self.expect_semi()?;

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
                return Err(self.expected_tokens_error("expected `:` or `;`"));
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

            _ => return Err(self.expected_tokens_error("expected")),
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
            return Err(self.expected_tokens_error("expected"));
        };

        Ok(item)
    }

    /// `type Name<...> { ... }` / `type Name = T;` and the `class` sugar.
    fn parse_type_decl(
        &mut self,
        _quals: ThinVec<Qual>,
        is_class: bool,
    ) -> ParseResult<'a, DeclKind> {
        if is_class {
            self.expect_keyword(kw::Class)?;
        } else {
            self.expect_keyword(kw::Type)?;
        }

        // `type Name<...>` — the name precedes its generic specification.
        let ident = self.parse_identifier()?;
        let generics = self.parse_generic()?;

        // `class C : A, B` — traits to implement automatically.
        let mut traits = ThinVec::new();

        if self.eat(&TokenKind::Colon) {
            let (parsed, ..) = self.parse_seq_before(
                &[
                    &TokenKind::OpenDelim(Delimiter::Brace),
                    &TokenKind::Semicolon,
                    &TokenKind::Op(op::Assign),
                    &TokenKind::OpenDelim(Delimiter::Paren),
                ],
                SequenceSeparator::new(TokenKind::Comma),
                TokenExpectType::Yes,
                |this| this.parse_type(),
            )?;

            traits = parsed;
        }

        let body = if self.eat_operator(op::Assign) {
            TypeDeclBody::Alias(self.parse_type()?)
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            let (fields, members) = self.parse_class_body()?;
            TypeDeclBody::Struct { fields, members }
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Paren)) {
            // `class C(T, U);` / `class C(x: T, y: U);` — the parenthesized
            // structural form; ordinal and named fields are both allowed.
            let (members, _) = self.parse_paren_comma_seq(|this| {
                let is_mut = this.eat_keyword(kw::Mut);

                let member = if this.check_identifier()
                    && this.look_ahead(1, |t| matches!(t.kind, TokenKind::Colon))
                {
                    let key = this.parse_identifier()?;
                    this.expect(&TokenKind::Colon)?;

                    Member::Named(key.name, this.parse_type()?, is_mut)
                } else {
                    Member::Ordinal(this.parse_type()?, is_mut)
                };

                Ok(member)
            })?;

            TypeDeclBody::Struct {
                fields: StructType { members },
                members: ThinVec::new(),
            }
        } else {
            return Err(self.expected_tokens_error("expected"));
        };

        self.eat(&TokenKind::Semicolon);

        Ok(DeclKind::Type(P(TypeDecl {
            is_class,
            ident,
            generics,
            traits,
            body,
        })))
    }

    /// A class body: `{ name: T, let x: T, func f() {}, ... }` — the braced
    /// form holds named fields (plain or `let`-spelled), properties and
    /// methods. The parenthesized form `class C(T, x: T)` is the plain
    /// structural spelling and holds fields only.
    fn parse_class_body(&mut self) -> ParseResult<'a, (StructType, ThinVec<MemberItem>)> {
        let mut fields = ThinVec::new();
        let mut members = ThinVec::new();

        self.expect(&TokenKind::OpenDelim(Delimiter::Brace))?;

        // Fields are separated by `,` or `;` — one kind per body.
        let mut separator_is_semi: Option<bool> = None;

        loop {
            if self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
                break;
            }
            if self.token.kind == TokenKind::Eof {
                break;
            }

            // Methods and qualified members delegate to the member parser.
            let is_member = self.token.is_keyword(kw::Func)
                || self.token.is_keyword(kw::Init)
                || self.token.is_keyword(kw::Static)
                || self.token.is_keyword(kw::Const)
                || self.token.is_keyword(kw::Unsafe)
                || self.token.is_keyword(kw::Async)
                || self.token.is_keyword(kw::Public)
                || self.token.is_keyword(kw::Internal)
                || self.token.is_keyword(kw::Private)
                || self.token.is_keyword(kw::Operator);

            if is_member {
                members.push(self.parse_member_item()?);
                continue;
            }

            // `let` members: plain declarations (`let x: T;`, `let mut x:
            // T;`) are ordinary named fields; accessor blocks, `=> expr` and
            // `= expr` forms are properties and delegate to the member
            // parser.
            if self.token.is_keyword(kw::Let) {
                let snapshot = self.create_snapshot();

                self.next();
                let is_mut = self.eat_keyword(kw::Mut);
                let key = self.parse_identifier()?;
                let ty = if self.eat(&TokenKind::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let is_property = self.check(&TokenKind::OpenDelim(Delimiter::Brace))
                    || self.check(&TokenKind::FatArrow)
                    || self.check(&TokenKind::Op(op::Assign));

                if is_property {
                    self.restore_snapshot(snapshot);

                    members.push(self.parse_member_item()?);
                    continue;
                }

                let ty = ty.ok_or_else(|| {
                    self.diag_ctx().struct_span_error(
                        key.span,
                        "a `let` field declaration requires a type — `let name: T`",
                    )
                })?;

                fields.push(Member::Named(key.name, ty, is_mut));

                if self.check(&TokenKind::CloseDelim(Delimiter::Brace)) {
                    continue;
                }

                let sep_span = self.token.span;

                let this_is_semi = if self.eat(&TokenKind::Comma) {
                    false
                } else if self.eat(&TokenKind::Semicolon) {
                    true
                } else {
                    return Err(self.diag_ctx().struct_span_error(
                        sep_span,
                        "expected `,` or `;` between fields",
                    ));
                };

                match separator_is_semi {
                    None => separator_is_semi = Some(this_is_semi),
                    Some(prev) if prev != this_is_semi => {
                        return Err(self.diag_ctx().struct_span_error(
                            sep_span,
                            "cannot mix `,` and `;` in one field list",
                        ));
                    }
                    _ => {}
                }

                continue;
            }

            // Any other token in a braced body is an ordinal field, which the
            // braced form does not accept.
            return Err(self.diag_ctx().struct_span_error(
                self.token.span,
                "the braced class form holds `let` fields, properties and methods — use `class C(T)` for ordinal fields",
            ));
        }

        Ok((StructType { members: fields }, members))
    }

    fn parse_enum_decl(&mut self, _quals: ThinVec<Qual>) -> ParseResult<'a, DeclKind> {
        self.expect_keyword(kw::Enum)?;

        let ident = self.parse_identifier()?;
        let generics = self.parse_generic()?;

        let base = if self.eat(&TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let (variants, _) = self.parse_delim_comma_seq(Delimiter::Brace, |this| {
            let attrs = this.parse_outer_attrs()?;
            let ident = this.parse_identifier()?;

            let payload = if this.check(&TokenKind::OpenDelim(Delimiter::Paren)) {
                let lo = this.token.span;
                let kind = this.parse_type_struct_or_paren()?;
                let span = lo.to(this.prev_token.span);
                Some(this.make_type(kind, span))
            } else if this.check(&TokenKind::OpenDelim(Delimiter::Bracket)) {
                let lo = this.token.span;
                this.next();
                let elem = this.parse_type()?;
                this.expect(&TokenKind::CloseDelim(Delimiter::Bracket))?;
                let span = lo.to(this.prev_token.span);
                Some(this.make_type(TypeKind::Array(elem), span))
            } else {
                None
            };

            let value = if this.eat_operator(op::Assign) {
                Some(this.parse_expr()?)
            } else {
                None
            };

            Ok(EnumVariant {
                attrs,
                ident,
                payload,
                value,
            })
        })?;

        Ok(DeclKind::Enum(P(EnumDecl {
            ident,
            generics,
            base,
            variants,
        })))
    }

    fn parse_impl(&mut self, _quals: ThinVec<Qual>) -> ParseResult<'a, DeclKind> {
        self.expect_keyword(kw::Impl)?;

        let generics = self.parse_generic()?;
        let self_ty = self.parse_type()?;

        let trait_ty = if self.eat(&TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Legacy `where` clauses; the language no longer has `where`, so the
        // tokens are skipped up to the body.
        if matches!(self.token.to_identifier(), Some((ident, _)) if ident.as_str() == "where") {
            while !(self.check_noexpect(&TokenKind::OpenDelim(Delimiter::Brace))
                || self.check_noexpect(&TokenKind::Semicolon)
                || self.token.kind == TokenKind::Eof)
            {
                self.next();
            }
        }

        let condition = if self.eat_keyword(kw::If) {
            let expr = self.with_res(Restrictions::NO_TRAILING_BLOCK, |this| this.parse_expr())?;

            Some(expr)
        } else {
            None
        };

        let body = if self.eat_operator(op::Assign) {
            self.expect_keyword(kw::Never)?;
            self.expect_semi()?;
            ImplBody::Never
        } else if self.eat(&TokenKind::Semicolon) {
            ImplBody::Semicolon
        } else {
            self.expect(&TokenKind::OpenDelim(Delimiter::Brace))?;

            let mut items = ThinVec::new();

            loop {
                if self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
                    break;
                }
                if self.token.kind == TokenKind::Eof {
                    break;
                }

                items.push(self.parse_impl_item()?);
            }

            ImplBody::Block(items)
        };

        Ok(DeclKind::Impl(P(ImplDecl {
            generics,
            self_ty,
            trait_ty,
            condition,
            body,
        })))
    }

    fn parse_impl_item(&mut self) -> ParseResult<'a, ImplItem> {
        let item = self.parse_member_item()?;

        Ok(match item {
            MemberItem::Func(f) => ImplItem::Func(f),
            MemberItem::Type(t) => ImplItem::Type(t),
            MemberItem::Const(c) => ImplItem::Const(c),
            MemberItem::Prop(p) => ImplItem::Prop(p),
        })
    }

    fn parse_macro_decl(&mut self, _quals: ThinVec<Qual>) -> ParseResult<'a, DeclKind> {
        self.expect_keyword(kw::Macro)?;

        let ident = self.parse_identifier()?;

        let TokenTree::Delimited(tokens, delim, span, _) = self.parse_token_tree() else {
            return Err(self.expected_tokens_error("expected"));
        };

        let body = DelimitedArgs {
            delim_span: span,
            delim,
            tokens,
        };

        Ok(DeclKind::Macro(P(MacroDecl { ident, body })))
    }
}
