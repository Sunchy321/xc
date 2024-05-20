use xc_ast::{token::Delimiter, tokenstream::TokenStream};
use xc_error::diag::Diagnostic;

use super::{lexer::Lexer, tokentree::TokenTreeReader};

impl<'a, 'src> Lexer<'a, 'src> {
    pub(crate) fn raise_error(&self) -> ! {
        panic!("raise error");
    }

    pub fn report_unterminated_block_comment(&self, start: u32) {

    }
}

impl<'a, 'src> TokenTreeReader<'a, 'src> {
    pub fn raise_unclosed_delim(&mut self, tts: TokenStream, errs: Vec<Diagnostic<'a>>) -> Vec<Diagnostic<'a>> {
        // TODO: unimplemented

        errs
    }

    pub fn raise_unexpected_close_delim(&mut self, delim: Delimiter) -> Diagnostic<'a> {
        unimplemented!()
    }

    pub fn raise_unexpected_eof(&mut self) -> Diagnostic<'a> {
        unimplemented!()
    }
}