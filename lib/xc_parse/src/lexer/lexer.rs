use xc_ast::{literal::{Literal, LiteralKind}, token::*};
use xc_lexer::{cursor::Cursor, unescape::{check_raw_string, unescape_string}, Base};
use xc_span::{BytePos, Span, Symbol};

use super::nfc_normalize;

pub struct Lexer<'src> {
    src: &'src str,
    cursor: Cursor<'src>,
    start_pos: BytePos,
    pos: BytePos,
}

impl<'src> Lexer<'src> {
    fn str_from(&self, start: BytePos) -> &'src str {
        self.str_from_to(start, self.pos)
    }

    fn str_from_to(&self, start: BytePos, end: BytePos) -> &'src str {
        &self.src[self.src_index(start)..self.src_index(end)]
    }

    fn symbol_from_to(&self, start: BytePos, end: BytePos) -> Symbol {
        Symbol::intern(self.str_from_to(start, end))
    }

    fn src_index(&self, pos: BytePos) -> usize {
        (pos - self.start_pos).to_usize()
    }

    fn make_span(&self, start: BytePos, end: BytePos) -> Span {
        Span::new(start, end)
    }

    fn make_span_end(&self, start: BytePos) -> Span {
        self.make_span(start, self.pos)
    }

    pub fn next_token(&mut self) -> (Token, bool) {
        let mut preceded_by_whitespace = false;

        loop {
            let str_before = self.cursor.as_str();

            let token = self.cursor.next_token();
            let start = self.pos;

            self.pos += BytePos(token.len);

            let kind = match token.kind {
                xc_lexer::TokenKind::LineComment => {
                    preceded_by_whitespace = true;
                    continue;
                }

                xc_lexer::TokenKind::BlockComment { terminated } => {
                    if !terminated {
                        self.report_unterminated_block_comment(start.0);
                    }

                    preceded_by_whitespace = true;
                    continue;
                }

                xc_lexer::TokenKind::Whitespace => {
                    preceded_by_whitespace = true;
                    continue;
                }

                xc_lexer::TokenKind::Identifier => self.proc_identifier(start),

                xc_lexer::TokenKind::SymbolLit { terminated } => {
                    if !terminated {
                        self.raise_error();
                    }

                    self.proc_symbol(start)
                }

                xc_lexer::TokenKind::Literal { kind, suffix_start } => {
                    let suffix_start = start + BytePos(suffix_start);
                    let (kind, symbol) = self.proc_lexer_literal(start, suffix_start, kind);
                    let suffix = if suffix_start < self.pos {
                        let string = self.str_from(suffix_start);
                        if string == "_" {
                            // self.psess
                            //     .dcx
                            //     .emit_err(errors::UnderscoreLiteralSuffix { span: self.mk_sp(suffix_start, self.pos) });
                            None
                        } else if string.starts_with("_") {
                            Some(Symbol::intern(&string[1..]))
                        } else {
                            Some(Symbol::intern(string))
                        }
                    } else {
                        None
                    };

                    TokenKind::Literal(Literal { kind, value: symbol, suffix })
                }

                xc_lexer::TokenKind::OpenParen => TokenKind::OpenDelim(Delimiter::Paren),
                xc_lexer::TokenKind::CloseParen => TokenKind::ClosedDelim(Delimiter::Paren),
                xc_lexer::TokenKind::OpenBracket => TokenKind::OpenDelim(Delimiter::Bracket),
                xc_lexer::TokenKind::CloseBracket => TokenKind::ClosedDelim(Delimiter::Bracket),
                xc_lexer::TokenKind::OpenBrace => TokenKind::OpenDelim(Delimiter::Brace),
                xc_lexer::TokenKind::CloseBrace => TokenKind::ClosedDelim(Delimiter::Brace),
                xc_lexer::TokenKind::OpenDict => TokenKind::OpenDelim(Delimiter::DictBound),
                xc_lexer::TokenKind::CloseDict => TokenKind::ClosedDelim(Delimiter::DictBound),

                xc_lexer::TokenKind::At => TokenKind::At,
                xc_lexer::TokenKind::Pound => TokenKind::Pound,
                xc_lexer::TokenKind::Dollar => TokenKind::Dollar,
                xc_lexer::TokenKind::Semicolon => TokenKind::Semicolon,
                xc_lexer::TokenKind::Colon => TokenKind::Colon,
                xc_lexer::TokenKind::Comma => TokenKind::Comma,

                xc_lexer::TokenKind::SymbolOpen => TokenKind::SymbolOpen,
                xc_lexer::TokenKind::ColonColon => TokenKind::ColonColon,
                xc_lexer::TokenKind::RightArrow => TokenKind::RightArrow,
                xc_lexer::TokenKind::FatArrow => TokenKind::FatArrow,
                xc_lexer::TokenKind::DotDotDot => TokenKind::DotDotDot,

                xc_lexer::TokenKind::Operator => self.proc_operator(start),

                xc_lexer::TokenKind::LambdaArg => {
                    let id = self.str_from(start + BytePos(1));

                    match id.parse::<u32>() {
                        Ok(n) => TokenKind::LambdaArgUnnamed(n),
                        Err(_) => TokenKind::LambdaArgNamed(nfc_normalize(id)),
                    }
                },

                xc_lexer::TokenKind::Unknown => panic!("Unknown token: {:?}", str_before), // TODO: error handling

                xc_lexer::TokenKind::Eof => TokenKind::Eof,
            };

            let span = self.make_span_end(start);

            return (Token { kind, span }, preceded_by_whitespace)
        }
    }

    fn proc_identifier(&self, start: BytePos) -> TokenKind {
        let sym = Symbol::intern(&nfc_normalize(self.str_from(start)));

        TokenKind::Identifier(sym)
    }

    fn proc_operator(&self, start: BytePos) -> TokenKind {
        let sym = Symbol::intern(self.str_from(start));

        TokenKind::Op(sym)
    }

    fn proc_symbol(&self, start: BytePos) -> TokenKind {
        let sym = Symbol::intern(self.str_from(start + BytePos(1)));

        TokenKind::SymbolLit(sym)
    }

    fn proc_lexer_literal(
        &self,
        start: BytePos,
        end: BytePos,
        kind: xc_lexer::LiteralKind,
    ) -> (LiteralKind, Symbol) {
        match kind {
            xc_lexer::LiteralKind::Integer { base, is_empty } => {
                let mut kind = LiteralKind::Integer;
                if is_empty {
                    let span = self.make_span(start, end);
                    // let guar = self.dcx().emit_err(errors::NoDigitsLiteral { span });
                    kind = LiteralKind::Error;
                } else if matches!(base, Base::Binary) {
                    let base = base as u32;
                    let s = self.str_from_to(start + BytePos(2), end);
                    for (idx, c) in s.char_indices() {
                        let span = self.make_span(
                            start + BytePos::from_usize(2 + idx),
                            start + BytePos::from_usize(2 + idx + c.len_utf8()),
                        );
                        if c != '_' && c.to_digit(base).is_none() {
                            // let guar =
                                // self.dcx().emit_err(errors::InvalidDigitLiteral { span, base });
                            kind = LiteralKind::Error;
                        }
                    }
                }

                (kind, self.symbol_from_to(start, end))
            }

            xc_lexer::LiteralKind::Floating { base, empty_exponent } => {
                let mut kind = LiteralKind::Floating;
                if empty_exponent {
                    // let span = self.mk_sp(start, self.pos);
                    // let guar = self.dcx().emit_err(errors::EmptyExponentFloat { span });
                    kind = LiteralKind::Error;
                }

                match base {
                    Base::Binary => {
                        kind = LiteralKind::Error;
                    }
                    _ => { }
                }

                (kind, self.symbol_from_to(start, end))
            }

             xc_lexer::LiteralKind::String { terminated } => {
                if !terminated {
                    self.raise_error();
                }

                self.proc_string(LiteralKind::String, start, end, 1, 1)
            }

            xc_lexer::LiteralKind::RawString { at_count } => {
                if let Some(at_count) = at_count {
                    self.proc_raw_string(LiteralKind::RawString { at_count }, start, end, 1 + at_count, 1 + at_count)
                } else {
                    self.raise_error();
                }
            }
        }
    }

    fn proc_string(
        &self,
        mut kind: LiteralKind,
        start: BytePos,
        end: BytePos,
        prefix: u32,
        postfix: u32,
    ) -> (LiteralKind, Symbol) {
        let content_start = start + BytePos(prefix);
        let content_end = end - BytePos(postfix);
        let content = self.str_from_to(content_start, content_end);

        unescape_string(content, &mut |range, result| {
            if let Err(err) = result {
                let full_span = self.make_span(start, end);
                let (start, end) = (range.start as u32, range.end as u32);
                let lo = content_start + BytePos(start);
                let hi = lo + BytePos(end - start);
                let span = self.make_span(lo, hi);

                kind = LiteralKind::Error;
            }
        });

        let sym = if !matches!(kind, LiteralKind::Error) {
            Symbol::intern(content)
        } else {
            self.symbol_from_to(start, end)
        };

        (kind, sym)
    }

    fn proc_raw_string(
        &self,
        mut kind: LiteralKind,
        start: BytePos,
        end: BytePos,
        prefix: u32,
        postfix: u32,
    ) -> (LiteralKind, Symbol) {
        let content_start = start + BytePos(prefix);
        let content_end = end - BytePos(postfix);
        let content = self.str_from_to(content_start, content_end);

        check_raw_string(content, &mut |range, result| {
            if let Err(err) = result {
                let full_span = self.make_span(start, end);
                let (start, end) = (range.start as u32, range.end as u32);
                let lo = content_start + BytePos(start);
                let hi = lo + BytePos(end - start);
                let span = self.make_span(lo, hi);

                kind = LiteralKind::Error;
            }
        });

        let sym = if !matches!(kind, LiteralKind::Error) {
            Symbol::intern(content)
        } else {
            self.symbol_from_to(start, end)
        };

        (kind, sym)
    }
}