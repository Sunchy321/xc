use thin_vec::ThinVec;
use xc_ast::decl::Qual;
use xc_ast::token::TokenKind;
use xc_ast::r#trait::Trait;
use xc_span::symbol::{kw, op};
use xc_span::Span;

use super::parser::Parser;
use super::ParseResult;

impl<'a> Parser<'a> {
    pub(crate) fn parse_trait(&mut self, quals: ThinVec<Qual>, _lo: Span) -> ParseResult<'a, Trait> {
        self.expect_keyword(kw::Trait)?;

        let ident = self.parse_identifier()?;

        // TODO: parse generics

        let has_colon = self.eat(&TokenKind::Colon);
        let colon_span = self.prev_token.span;



        if self.eat_operator(op::Assign) {

        }

        todo!()
    }
}
