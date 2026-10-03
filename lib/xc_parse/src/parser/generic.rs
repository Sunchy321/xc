use thin_vec::ThinVec;
use xc_ast::generic::{
    BoundPolarity, Generic, GenericBound, GenericBounds, GenericConstraint, GenericParam,
    GenericParamKind, TraitBoundQualifiers, WhereClause,
};
use xc_ast::path::PathStyle;
use xc_ast::token::{Delimiter, TokenKind};
use xc_span::symbol::{kw, op};

use super::parser::Parser;
use super::{AllowPlus, ParseResult, SequenceSeparator, TokenExpectType};

impl<'a> Parser<'a> {
    pub(crate) fn parse_generic(&mut self) -> ParseResult<'a, Generic> {
        let lo = self.token.span;

        let (params, span) = if self.token.kind == TokenKind::Op(op::Less) {
            self.next();

            let (params, ..) = self.parse_seq_before(
                &[&TokenKind::Op(op::Greater)],
                SequenceSeparator::new(TokenKind::Comma),
                TokenExpectType::Yes,
                |this| this.parse_generic_param(),
            )?;

            let span = lo.to(self.prev_token.span);

            if !self.eat_greater() {
                return Err(self.expected_tokens_error("expected"));
            }

            (params, span)
        } else {
            (ThinVec::new(), self.prev_token.span.shrink_to_hi())
        };

        let generic = Generic {
            params,
            where_clause: WhereClause::default(),
            span,
        };

        Ok(generic)
    }

    fn parse_generic_param(&mut self) -> ParseResult<'a, GenericParam> {
        let attrs = self.parse_outer_attrs()?;
        let lo = self.token.span;

        let (ident, kind, variadic) = if self.eat_keyword(kw::Const) {
            let ident = self.parse_identifier()?;

            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;

            let const_span = lo.to(self.prev_token.span);

            let default = if self.eat_operator(op::Assign) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            (
                ident,
                GenericParamKind::Const {
                    ty,
                    const_span,
                    default,
                },
                false,
            )
        } else {
            let ident = self.parse_identifier()?;

            let variadic = self.eat(&TokenKind::DotDotDot);

            let default = if self.eat_operator(op::Assign) {
                Some(self.parse_type()?)
            } else {
                None
            };

            (ident, GenericParamKind::Type { default }, variadic)
        };

        let constraints = self.parse_generic_constraints()?;

        Ok(GenericParam {
            ident,
            attrs,
            kind,
            variadic,
            constraints,
        })
    }

    /// Parse the constraints attached to a generic parameter:
    /// `: type`, `: Type`, `impl Type` and `if Expression`.
    fn parse_generic_constraints(&mut self) -> ParseResult<'a, ThinVec<GenericConstraint>> {
        let mut constraints = ThinVec::new();

        loop {
            if self.eat(&TokenKind::Colon) {
                if self.eat_keyword_noexpect(kw::Type) {
                    constraints.push(GenericConstraint::Kind(self.prev_token.span));
                } else {
                    constraints.push(GenericConstraint::Subtype(self.parse_type()?));
                }
            } else if self.eat_keyword(kw::Impl) {
                constraints.push(GenericConstraint::Impl(self.parse_type()?));
            } else if self.eat_keyword(kw::If) {
                // `if Expression` — the condition stops at `>`.
                let expr = self.with_res(super::Restrictions::NO_GT, |this| this.parse_expr())?;
                constraints.push(GenericConstraint::Condition(expr));
            } else {
                break;
            }
        }

        Ok(constraints)
    }

    pub(crate) fn parse_generic_bounds(&mut self) -> ParseResult<'a, GenericBounds> {
        self.parse_generic_bounds_impl(AllowPlus::Yes)
    }

    fn parse_generic_bounds_impl(&mut self, allow_plus: AllowPlus) -> ParseResult<'a, GenericBounds> {
        let mut bounds = vec![];

        while self.can_begin_bound() {
            bounds.push(self.parse_generic_bound()?);

            if allow_plus == AllowPlus::No || !self.eat_operator(op::Plus) {
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
        let quals = self.parse_trait_bound_qualifiers()?;

        let path = self.parse_path(PathStyle::Type)?;

        Ok(GenericBound::Trait(path, quals))
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
