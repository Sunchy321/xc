use xc_ast::generic::{BoundPolarity, GenericBound, GenericBounds, TraitBoundQualifiers};
use xc_ast::path::PathStyle;
use xc_ast::token::{Delimiter, TokenKind};
use xc_span::symbol::{kw, op};

use super::parser::Parser;
use super::{AllowPlus, ParseResult};

impl<'a> Parser<'a> {
    pub(crate) fn parse_generic_bounds(&mut self) -> ParseResult<'a, GenericBounds>{
        self.parse_generic_bounds_impl(AllowPlus::Yes)
    }

    fn parse_generic_bounds_impl(&mut self, allow_plus: AllowPlus) -> ParseResult<'a, GenericBounds> {
        let mut bounds = vec![];

        while self.can_begin_bound() {
            bounds.push(self.parse_generic_bound()?);

            if allow_plus == AllowPlus::No || !self.eat_operator(op::Plus)  {
                break;
            }
        }
        
        Ok(bounds)
    }

    fn can_begin_bound(&mut self) -> bool {
        self.check_path()
            || self.check_operator(op::Exclamation)
            || self.check_operator(op::Question)
            || self.check(&TokenKind::OpenDelim(Delimiter::Paren))
    }

    fn parse_generic_bound(&mut self) -> ParseResult<'a, GenericBound> {
        let lo = self.token.span;

        let quals = self.parse_trait_bound_qualifiers()?;

        let path = self.parse_path(PathStyle::Type);


        todo!()
    }

    fn parse_trait_bound_qualifiers(&mut self) -> ParseResult<'a, TraitBoundQualifiers> {
        let polarity = if self.eat_operator(op::Question) {
            BoundPolarity::Maybe(self.prev_token.span)
        } else if self.eat_operator(op::Exclamation) {
            BoundPolarity::Negative(self.prev_token.span)
        } else {
            BoundPolarity::Positive
        };

        Ok(TraitBoundQualifiers { polarity })
    }
}
