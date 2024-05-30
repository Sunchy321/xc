use lexer::Lexer;
use tokentree::TokenTreeReader;
use xc_ast::tokenstream::TokenStream;
use xc_error::diag::Diagnostic;
use xc_lexer::{cursor::Cursor, strip_shebang};
use xc_span::{BytePos, Pos};

use crate::session::ParseSession;

pub mod lexer;
pub mod error;
pub mod tokentree;

pub(crate) fn parse_token_tree<'a, 'src>(
    session: &'a ParseSession,
    mut src: &'src str,
    mut start_pos: BytePos,
) -> Result<TokenStream, Vec<Diagnostic<'a>>> {
    // skips `#!`
    if let Some(shebang_len) = strip_shebang(src) {
        src = &src[shebang_len..];
        start_pos = start_pos + BytePos::from_usize(shebang_len);
    }

    let cursor = Cursor::new(src);

    let lexer = Lexer {
        session,
        src,
        cursor,
        start_pos,
        pos: start_pos
    };

    let (stream, res) = TokenTreeReader::parse_all_token_tree(lexer);

    match res {
        Ok(()) => Ok(stream),

        _ => todo!()
    }
}

pub fn nfc_normalize(string: &str) -> String {
    use unicode_normalization::{is_nfc_quick, IsNormalized, UnicodeNormalization};
    match is_nfc_quick(string.chars()) {
        IsNormalized::Yes => string.to_string(),
        _ => string.chars().nfc().collect()
    }
}