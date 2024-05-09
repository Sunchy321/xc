use super::lexer::Lexer;

impl<'src> Lexer<'src> {
    pub(crate) fn raise_error(&self) -> ! {
        panic!("raise error");
    }

    pub fn report_unterminated_block_comment(&self, start: u32) {

    }
}