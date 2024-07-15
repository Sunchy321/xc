use xc_ast::pattern::{Pattern, PatternKind};
use xc_span::symbol::kw;

use super::{parser::Parser, ParseResult, Restrictions};

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> ParseResult<'a, Pattern> {
        self.parse_pattern_primary()
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
