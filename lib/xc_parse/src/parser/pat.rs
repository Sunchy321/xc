use xc_ast::pattern::{ArrayPatternItem, Pattern, PatternKind, StructPatternItem};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::Mutability;
use xc_span::symbol::kw;
use xc_span::{Identifier, Span};

use super::HasTrailing;
use super::{parser::Parser, ParseResult, Restrictions};

impl<'a> Parser<'a> {
    fn make_pattern(&self, kind: PatternKind, span: Span) -> P<Pattern> {
        P(Pattern {
            kind,
            assert: None,
            span,
        })
    }

    pub fn parse_pattern(&mut self) -> ParseResult<'a, P<Pattern>> {
        let lo = self.token.span;

        let kind = if self.check(&TokenKind::OpenDelim(Delimiter::Paren)) {
            self.parse_pattern_struct()?
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Bracket)) {
            self.parse_pattern_array()?
        } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_pattern_struct()?
        } else if self.eat_keyword(kw::Let) {
            return self.parse_pattern_let(lo);
        } else {
            self.parse_pattern_primary()?
        };

        let span = lo.to(self.prev_token.span);

        Ok(self.make_pattern(kind, span))
    }

    pub fn parse_pattern_struct(&mut self) -> ParseResult<'a, PatternKind> {
        let (pats, trailing) = self.parse_delim_comma_seq(Delimiter::Paren, |this| {
            let item = if this.eat(&TokenKind::DotDotDot) {
                if this.check(&TokenKind::Comma) {
                    StructPatternItem::Expand(None)
                } else {
                    StructPatternItem::Expand(Some(this.parse_pattern()?))
                }
            } else if let Some(key) = this.parse_key()? {
                let pat = this.parse_pattern()?;

                StructPatternItem::Named(key, pat)
            } else {
                StructPatternItem::Ordinal(this.parse_pattern()?)
            };

            Ok(item)
        })?;

        if pats.len() == 1 && trailing == HasTrailing::No {
            match &pats[0] {
                StructPatternItem::Ordinal(pat) => return Ok(PatternKind::Paren(pat.clone())),
                _ => {}
            }
        }

        Ok(PatternKind::Struct(pats))
    }

    pub fn parse_pattern_array(&mut self) -> ParseResult<'a, PatternKind> {
        let (pats, _) = self.parse_delim_comma_seq(Delimiter::Bracket, |this| {
            let item = if this.eat(&TokenKind::DotDotDot) {
                if this.check(&TokenKind::Comma) {
                    ArrayPatternItem::Expand(None)
                } else {
                    ArrayPatternItem::Expand(Some(this.parse_pattern()?))
                }
            } else {
                ArrayPatternItem::Item(this.parse_pattern()?)
            };

            Ok(item)
        })?;

        Ok(PatternKind::Array(pats))
    }

    pub fn parse_pattern_let(&mut self, lo: Span) -> ParseResult<'a, P<Pattern>> {
        let mut res = Restrictions::PAT_IN_LET;

        if self.eat_keyword(kw::Mut) {
            res |= Restrictions::PAT_IN_LET_MUT;
        }

        self.with_res(res, |this| {
            let pat = this.parse_pattern()?;

            let span = lo.to(this.prev_token.span);

            Ok(this.make_pattern(pat.kind.clone(), span))
        })
    }

    pub fn parse_pattern_primary(&mut self) -> ParseResult<'a, PatternKind> {
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

        Ok(kind)
    }

    fn create_pattern_binding(&mut self, ident: Identifier) -> PatternKind {
        if self.restrictions.contains(Restrictions::PAT_IN_LET_MUT) {
            PatternKind::Bind(ident, Mutability::Mut)
        } else {
            PatternKind::Bind(ident, Mutability::Immut)
        }
    }
}
