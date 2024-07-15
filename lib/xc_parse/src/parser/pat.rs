use xc_ast::pattern::{Pattern, PatternKind};
use xc_span::Span;
use xc_span::symbol::kw;

use super::{parser::Parser, ParseResult, Restrictions};

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        if self.eat_keyword(kw::Let) {
            self.parse_pattern_let(lo)
        } else {
            self.parse_pattern_primary()
        }
    }

    pub fn parse_pattern_let(&mut self, lo: Span) -> ParseResult<'a, Pattern> {
        let mut res = Restrictions::PAT_IN_LET;

        if self.eat_keyword(kw::Mut) {
            res |= Restrictions::PAT_IN_LET_MUT;
        }

        self.with_res(res, |this| {
            let pat = this.parse_pattern()?;

            let hi = this.prev_token.span;
            let span = lo.to(hi);

            Ok(Pattern::new(pat.kind, span))
        })
    }

    pub fn parse_pattern_primary(&mut self) -> ParseResult<'a, Pattern> {
        let lo = self.token.span;

        let kind = if self.eat_keyword(kw::Underscore) {
            PatternKind::Wildcard
        } else if self.restrictions.contains(Restrictions::PAT_IN_LET)
            && let Ok(ident) = self.parse_identifier()
        {
            PatternKind::Bind(ident)
        } else {
            let expr = self.parse_expr()?;

            PatternKind::Expr(expr)
        };

        let span = lo.to(self.prev_token.span);

        Ok(Pattern::new(kind, span))
    }
}
