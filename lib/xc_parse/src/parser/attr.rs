use thin_vec::ThinVec;
use xc_ast::token::TokenKind;

use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(super) fn parse_attr_list(&mut self) -> ParseResult<'a, ()> {
        let mut attrs = ThinVec::new();

        loop {
            let attr = if self.check(&TokenKind::At) {
                Some(self.parse_attr()?)
            } else {
                None
            };

            if let Some(attr) = attr {
                attrs.push(attr);
            } else {
                break;
            }
        }

        unimplemented!()
    }

    pub fn parse_attr(&mut self) -> ParseResult<'a, ()> {
        let lo = self.token.span;

        unimplemented!()
    }
}