use thin_vec::ThinVec;
use xc_ast::attr::Attribute;
use xc_ast::token::TokenKind;

use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(super) fn parse_attr_list(&mut self) -> ParseResult<'a, ThinVec<Attribute>> {
        // TODO: parse attribute
        return Ok(ThinVec::new());

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

    pub fn collect_tokens<R>(
        &mut self,
        attrs: ThinVec<Attribute>,
        force_collect: ForceCollect,
        f: impl FnOnce(&mut Self, ThinVec<Attribute>) -> ParseResult<'a, (R, TrailingToken)>,
    ) -> ParseResult<'a, R> {
        // TODO: Complete function

        let (ret, trailing) = f(self, attrs)?;

        Ok(ret)
    }
}

#[derive(Clone, Debug)]
pub enum ForceCollect {
    Yes,
    No,
}

#[derive(Clone, Debug)]
pub enum TrailingToken {
    None,
    Semicolon,
    Greater,
    MaybeComma,
}
