use xc_ast::expr::ExprKind;
use xc_ast::path::Path;
use xc_ast::pattern::{
    ArrayPatternItem, ObjectPatternItem, Pattern, PatternKind, TuplePatternItem,
};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::Mutability;
use xc_span::symbol::kw;
use xc_span::{Identifier, Span};

use super::HasTrailing;
use super::{parser::Parser, ParseResult, Restrictions};

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        if self.check(&TokenKind::OpenDelim(Delimiter::Paren)) {
            self.parse_pattern_tuple()
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Bracket)) {
            self.parse_pattern_array()
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_pattern_struct()
        } else if self.eat_keyword(kw::Let) {
            self.parse_pattern_let(lo)
        } else {
            self.parse_pattern_primary()
        }
    }

    pub fn parse_pattern_tuple(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        let (pats, trailing) = self.parse_delim_comma_seq(Delimiter::Paren, |this| {
            if this.eat(&TokenKind::DotDotDot) {
                if this.check(&TokenKind::Comma) {
                    Ok(TuplePatternItem::Expand(None))
                } else {
                    let pat = P(this.parse_pattern()?);
                    Ok(TuplePatternItem::Expand(Some(pat)))
                }
            } else {
                let pat = P(this.parse_pattern()?);
                Ok(TuplePatternItem::Item(pat))
            }
        })?;

        if pats.len() == 1 && trailing == HasTrailing::No {
            match &pats[0] {
                TuplePatternItem::Item(pat) => return Ok(pat.clone().into_inner()),
                _ => {}
            }
        }

        let span = lo.to(self.prev_token.span);

        Ok(Pattern::new(PatternKind::Tuple(pats), span))
    }

    pub fn parse_pattern_array(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        let (pats, _) = self.parse_delim_comma_seq(Delimiter::Bracket, |this| {
            if this.eat(&TokenKind::DotDotDot) {
                if this.check(&TokenKind::Comma) {
                    Ok(ArrayPatternItem::Expand(None))
                } else {
                    let pat = P(this.parse_pattern()?);
                    Ok(ArrayPatternItem::Expand(Some(pat)))
                }
            } else {
                let pat = P(this.parse_pattern()?);
                Ok(ArrayPatternItem::Item(pat))
            }
        })?;

        let span = lo.to(self.prev_token.span);

        Ok(Pattern::new(PatternKind::Array(pats), span))
    }

    pub fn parse_pattern_struct(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        let (fields, _) = self.parse_delim_comma_seq(Delimiter::Brace, |this| {
            if this.eat(&TokenKind::DotDotDot) {
                if this.check(&TokenKind::Comma) {
                    Ok(ObjectPatternItem::Expand(None))
                } else {
                    let pat = P(this.parse_pattern()?);
                    Ok(ObjectPatternItem::Expand(Some(pat)))
                }
            } else if this.check_identifier()
                && this.look_ahead(1, |t| {
                    matches!(
                        t.kind,
                        TokenKind::Comma | TokenKind::CloseDelim(Delimiter::Brace)
                    )
                })
            {
                let ident = this.parse_identifier()?;

                let name = ident.clone();

                let ident_span = ident.span;

                let kind = if this.restrictions.contains(Restrictions::PAT_IN_LET) {
                    this.create_pattern_binding(ident)
                } else {
                    let expr =
                        this.make_expr(ExprKind::Path(Path::from_single_ident(ident)), ident_span);

                    PatternKind::Expr(expr)
                };

                Ok(ObjectPatternItem::Field(
                    name,
                    P(Pattern::new(kind, ident_span)),
                ))
            } else {
                let key = this.parse_identifier()?;
                this.expect(&TokenKind::Colon)?;
                let pat = P(this.parse_pattern()?);
                Ok(ObjectPatternItem::Field(key, pat))
            }
        })?;

        let span = lo.to(self.prev_token.span);

        Ok(Pattern::new(PatternKind::Object(fields), span))
    }

    pub fn parse_pattern_let(&mut self, lo: Span) -> ParseResult<'a, Pattern> {
        let mut res = Restrictions::PAT_IN_LET;

        if self.eat_keyword(kw::Mut) {
            res |= Restrictions::PAT_IN_LET_MUT;
        }

        self.with_res(res, |this| {
            let pat = this.parse_pattern()?;

            let span = lo.to(this.prev_token.span);

            Ok(Pattern::new(pat.kind, span))
        })
    }

    pub fn parse_pattern_primary(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        let kind = if self.eat_keyword(kw::Underscore) {
            PatternKind::Wildcard
        } else {
            let expr = self.parse_expr()?;

            if self.restrictions.contains(Restrictions::PAT_IN_LET)
                && let Some(ident) = expr.to_single_ident()
            {
                self.create_pattern_binding(ident)
            } else {
                PatternKind::Expr(expr)
            }
        };

        let span = lo.to(self.prev_token.span);

        Ok(Pattern::new(kind, span))
    }

    fn create_pattern_binding(&mut self, ident: Identifier) -> PatternKind {
        if self.restrictions.contains(Restrictions::PAT_IN_LET_MUT) {
            PatternKind::Bind(ident, Mutability::Mut)
        } else {
            PatternKind::Bind(ident, Mutability::Immut)
        }
    }
}
