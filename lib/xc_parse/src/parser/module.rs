use std::mem;

use thin_vec::{thin_vec, ThinVec};
use xc_ast::decl::{Decl, DeclKind, Qual, QualKind};
use xc_ast::id::OperatorKind;
use xc_ast::import::{Import, ImportItem, ImportKind, ImportPathSegment};
use xc_ast::literal::LiteralKind;
use xc_ast::module::{Module, VisKind, Visibility};
use xc_ast::ptr::P;
use xc_ast::token::TokenKind;
use xc_span::symbol::{kw, op};
use xc_span::{Identifier, Span};

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
            self.parse_decl_kind(quals, Case::Sensitive)?
        };

        if let Some(kind) = kind {
            let span = lo.to(self.prev_token.span);

            let decl = Decl { kind, span };

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

    fn parse_decl_kind(&mut self, quals: ThinVec<Qual>, case: Case) -> ParseResult<'a, Option<DeclKind>> {
        let lo = self.token.span;

        let info = if self.check_keyword_case(kw::Module, case) {
            self.parse_module_decl(quals)?
        } else if self.check_keyword_case(kw::Import, case) {
            self.parse_import(quals, lo)?
        } else if self.check_keyword_case(kw::Func, case) {
            DeclKind::Func(P(self.parse_func(quals, lo)?))
        } else if self.check_keyword_case(kw::Trait, case) {
            DeclKind::Trait(P(self.parse_trait(quals, lo)?))
        } else {
            return Ok(None);
        };

        Ok(Some(info))
    }

    fn parse_module_decl(&mut self, quals: ThinVec<Qual>) -> ParseResult<'a, DeclKind> {
        

        todo!()
    }

    fn parse_import(&mut self, _quals: ThinVec<Qual>, lo: Span) -> ParseResult<'a, DeclKind> {
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

    fn parse_type_decl(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }

    fn parse_impl(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }
}
