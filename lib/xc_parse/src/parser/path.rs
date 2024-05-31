use thin_vec::ThinVec;
use xc_ast::token::TokenKind;
use xc_ast::path::{Path, PathSegment, PathStyle};

use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(crate) fn parse_path(&mut self, style: PathStyle) -> ParseResult<'a, Path> {
        let lo = self.token.span;
        let mut segments = ThinVec::new();

        if self.eat(&TokenKind::ColonColon) {
            segments.push(PathSegment::path_root(lo.shrink_to_lo()));
        }

        self.parse_path_segments(&mut segments, style)?;

        Ok(Path { segments, span: lo.to(self.prev_token.span) })
    }

    pub(crate) fn parse_path_segments(
        &mut self,
        segments: &mut ThinVec<PathSegment>,
        style: PathStyle
    ) -> ParseResult<'a, ()> {
        loop {
            let segment = self.parse_path_segment(style)?;

            if style.has_generic_ambiguity() {
                // TODO
            }

            segments.push(segment);

            if !self.eat(&TokenKind::ColonColon) {
                return Ok(());
            }
        }
    }

    pub(crate) fn parse_path_segment(
        &mut self,
        style: PathStyle
    ) -> ParseResult<'a, PathSegment> {
        let ident = self.parse_identifier()?;

        Ok(PathSegment::from_ident(ident))
    }
}
