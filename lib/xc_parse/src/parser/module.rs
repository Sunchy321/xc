use std::mem;

use thin_vec::{thin_vec, ThinVec};
use xc_ast::decl::{Decl, DeclKind, Qual, QualKind};
use xc_ast::module::{Module, VisKind, Visibility};
use xc_ast::token::TokenKind;
use xc_span::symbol::kw;

use super::Case;
use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> ParseResult<'a, Module> {
        todo!()
    }

    pub fn parse_decl(&mut self) -> ParseResult<'a, Option<Decl>> {
        let lo = self.token.span;

        let mut quals = self.parse_qual_list()?;

        let kind = if self.eat(&TokenKind::ColonColon) {
            let quals = mem::replace(&mut quals, thin_vec![]);

            DeclKind::QualDecl(quals)
        } else {
            self.parse_decl_kind()?
        };

        let span = lo.to(self.prev_token.span);

        let decl = Decl { kind, quals, span };

        Ok(Some(decl))
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
            return Ok(None)
        };

        Ok(Some(vis))
    }

    fn parse_decl_kind(&mut self) -> ParseResult<'a, DeclKind> {
        todo!()
    }

    fn parse_import(&mut self) -> ParseResult<'a, Decl> {
        todo!()
    }

    fn check_func_front_matter(&mut self, case: Case) -> bool {
        todo!()
    }

    fn parse_func(&mut self) -> ParseResult<'a, Decl> {
        todo!()
    }
}
