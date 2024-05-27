use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> ParseResult<'a, !> {}

    pub fn parse_decl(&mut self) -> ParserResult<'a, !> {}
}
