use xc_ast::{token::Delimiter, tokenstream::TokenStream};
use xc_error::diag::Diagnostic;

use super::{lexer::Lexer, tokentree::TokenTreeReader};

impl<'src> Lexer<'src> {
    pub(crate) fn raise_error(&self) -> ! {
        panic!("raise error");
    }

    pub fn report_unterminated_block_comment(&self, start: u32) {

    }
}

impl<'src> TokenTreeReader<'src> {
    pub fn raise_unclosed_delim(&mut self, tts: TokenStream, errs: Vec<Diagnostic>) -> Vec<Diagnostic> {
        // TODO: unimplemented

        errs
    }

    pub fn raise_unexpected_close_delim(&mut self, delim: Delimiter) -> Diagnostic {
        unimplemented!()
    }

    pub fn raise_unexpected_eof(&mut self) -> Diagnostic {
        unimplemented!()
    }
}