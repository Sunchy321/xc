use std::{any::Any, mem};

use thin_vec::ThinVec;
use xc_ast::{token::{Delimiter, Token, TokenKind}, tokenstream::Spacing};
use xc_error::diag::Diagnostic;
use xc_span::Symbol;

use super::{cursor::TokenCursor, ExpectTokenKind, HasTrailing, Recovered, SequenceSeparator, TokenExpectType};

#[derive(Clone)]
pub struct Parser {
    pub token: Token,
    pub spacing: Spacing,
    pub prev_token: Token,

    expected_tokens: Vec<ExpectTokenKind>,

    token_cursor: TokenCursor,
    next_call_count: usize,
}

impl Parser {
    pub fn next(&mut self) {
        let mut next_token = self.token_cursor.next();
        self.next_call_count += 1;
        // TODO: break token?

        if next_token.0.span.is_dummy() {

        }

        self.prev_token = mem::replace(&mut self.token, next_token.0);
        self.spacing = next_token.1;
    }

    pub fn look_ahead<R>(&self, dist: usize, looker: impl FnOnce(&Token) -> R) -> R {
        if dist == 0 {
            return looker(&self.token);
        }
    }

    pub fn eat(&mut self, kind: &TokenKind) -> bool {
        let correct = self.check(kind);

        if correct {
            self.next()
        }

        correct
    }

    pub fn eat_keyword(&mut self, key: Symbol) -> bool {
        if self.check_keyword(key) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn check(&self, kind: &TokenKind) -> bool {
        let correct = self.token.kind == *kind;
        if !correct {
            self.expected_tokens.push(ExpectTokenKind::Token(kind.clone()));
        }
        correct
    }

    pub fn check_noexpect(&self, kind: &TokenKind) -> bool {
        self.token.kind == *kind
    }

    pub fn check_keyword(&self, key: Symbol) -> bool {
        self.token.is_keyword(key)
    }

    pub fn expect(&mut self, kind: &TokenKind) -> Result<Recovered, Diagnostic> {
        if self.expected_tokens.is_empty() {
            if self.token.kind == *kind {
                self.next();
                Ok(Recovered::No)
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    }

    pub fn expect_any_of(&mut self, kinds: &[&TokenKind], expect: TokenExpectType) -> bool {
        kinds.iter().any(|kind| match expect {
            TokenExpectType::Yes => self.check(kind),
            TokenExpectType::No => self.check_noexpect(kind)
        })
    }

    pub fn parse_paren_comma_seq<T>(
        &mut self,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, bool), ()> {
        self.parse_delim_comma_seq(Delimiter::Paren, f)
    }


    pub fn parse_delim_comma_seq<T>(
        &mut self,
        delim: Delimiter,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, bool), ()>{
        self.parse_delim_seq(
            &TokenKind::OpenDelim(Delimiter::Paren),
            &TokenKind::CloseDelim(Delimiter::Paren),
            SequenceSeparator::new(TokenKind::Comma),
            f,
        )
    }

    pub fn parse_delim_seq<T>(
        &mut self,
        bra: &TokenKind,
        ket: &TokenKind,
        sep: SequenceSeparator,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, HasTrailing), ()>{
        self.expect(bra);
        self.parse_seq_to_end(ket, sep, f)
    }

    pub fn parse_seq_to_end<T>(
        &mut self,
        end: &TokenKind,
        sep: SequenceSeparator,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, HasTrailing), ()>{
        let (val, trailing, recovered) = self.parse_seq_before_end(end, sep, f)?;

        if matches!(recovered, Recovered::Yes) {
            self.eat(end);
        }

        Ok((val, trailing))
    }

    pub fn parse_seq_before_end<T>(
        &mut self,
        end: &TokenKind,
        sep: SequenceSeparator,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, HasTrailing, Recovered), ()>{
        self.parse_seq_before(&[end], sep, TokenExpectType::Yes,  f)
    }

    pub fn parse_seq_before<T>(
        &mut self,
        kinds: &[&TokenKind],
        sep: SequenceSeparator,
        expect: TokenExpectType,
        f: impl FnMut(&mut Parser) -> Result<T, ()>,
    ) -> Result<(ThinVec<T>, HasTrailing, Recovered), ()>{
        use TokenKind::*;

        let mut first = true;
        let mut recovered = Recovered::No;
        let mut trailing = Recovered::No;
        let mut v = ThinVec::new();

        while !self.expect_any_of(kinds, expect) {
            if let CloseDelim(..) | Eof = self.token.kind {
                break;
            }

            if let Some(t) = &sep.sep {
                if first {
                    first = false;
                } else {
                    match self.expect(t) {
                        Ok(Recovered::No) => {
                            unimplemented!()
                        }
                        Ok(Recovered::Yes) => {
                            recovered = Recovered::Yes;
                            unimplemented!()
                        }
                        Err(mut err) => {
                            unimplemented!()
                        }
                    }
                }
            }

            if sep.allow_trailing && self.expect_any_of(kinds, expect) {
                trailing = HasTrailing::Yes;
                break;
            }

            let t = f(self)?;
            v.push(t);
        }

        Ok((v, trailing, recovered))
    }
}