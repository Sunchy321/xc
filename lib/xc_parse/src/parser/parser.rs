use std::mem;
use std::path::Path;

use thin_vec::ThinVec;
use xc_ast::{literal::LiteralKind, token::{Delimiter, Token, TokenKind}, tokenstream::{Spacing, TokenTree}};
use xc_error::diag::Diagnostic;
use xc_span::{Span, Symbol};

use super::{cursor::TokenCursor, ConsumeClosingDelim, ExpectTokenKind, HasTrailing, ParseResult, Recovered, Restrictions, SequenceSeparator, TokenExpectType};

use crate::session::ParseSession;

#[derive(Clone)]
pub struct Parser<'a> {
    pub session: &'a ParseSession,
    pub token: Token,
    pub spacing: Spacing,
    pub prev_token: Token,

    restrictions: Restrictions,
    expected_tokens: Vec<ExpectTokenKind>,

    token_cursor: TokenCursor,
    next_call_count: usize,
}

impl<'a> Parser<'a> {
    pub fn from_file<'s>(session: &'s ParseSession, path: &Path, span: Option<Span>) -> Parser<'s> {
        let source_file = session.source_map();

        unimplemented!()
    }



    pub(crate) fn with_res<T>(&mut self, res: Restrictions, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.restrictions;
        self.restrictions = res;
        let result = f(self);
        self.restrictions = old;
        result
    }

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
        use TokenKind::*;

        if dist == 0 {
            return looker(&self.token);
        }

        if let Some(&(_, delim, span, _)) = self.token_cursor.stack.last() {
            if delim != Delimiter::Invisible {
                let tree_curser = &self.token_cursor.tree_cursor;

                let all_normal = (0..dist).all(|i| {
                    let token = tree_curser.look_ahead(i);
                    !matches!(token, Some(TokenTree::Delimited(_, Delimiter::Invisible, _, _)))
                });

                if all_normal {
                    return match tree_curser.look_ahead(dist - 1) {
                        Some(tree) => {
                            match tree {
                                TokenTree::Token(token, _) => looker(&token),
                                TokenTree::Delimited(_, delim, dspan, _) => looker(&Token {
                                    kind: OpenDelim(*delim),
                                    span: dspan.open,
                                })
                            }
                        }
                        None => {
                            looker(&Token {
                                kind: CloseDelim(delim),
                                span: span.close
                            })
                        }
                    }
                }
            }
        }

        let mut cursor = self.token_cursor.clone();
        let mut i = 0;
        let mut token = Token::dummy();

        while i < dist {
            token = cursor.next().0;

            if matches!(token.kind, OpenDelim(Delimiter::Invisible) | CloseDelim(Delimiter::Invisible)) {
                continue;
            }

            i += 1;
        }

        looker(&token)
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

    pub fn eat_keyword_noexpect(&mut self, key: Symbol) -> bool {
        if self.token.is_keyword(key) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn eat_symbol(&mut self) -> Option<Symbol> {
        match self.token.kind {
            TokenKind::Literal(lit) => {
                match lit.kind {
                    LiteralKind::SymbolLit => {
                        self.next();
                        Some(lit.value)
                    }

                    _ => None,
                }
            }

            _ => None,
        }
    }

    pub fn consume_block(&mut self, delim: Delimiter, consume_close: ConsumeClosingDelim) {
        use TokenKind::*;

        let mut depth = 0;

        loop {
            if self.eat(&OpenDelim(delim)) {
                depth += 1;
            } else if self.check(&CloseDelim(delim)) {
                if depth == 0 {
                    if let ConsumeClosingDelim::Yes = consume_close {
                        self.next();
                    }

                    return;
                } else {
                    self.next();
                    depth -= 1;
                    continue;
                }
            } else if self.token.kind == Eof {
                return;
            } else {
                self.next();
            }
        }
    }

    pub fn check(&mut self, kind: &TokenKind) -> bool {
        let correct = self.token.kind == *kind;
        if !correct {
            self.expected_tokens.push(ExpectTokenKind::Token(kind.clone()));
        }
        correct
    }

    pub fn check_noexpect(&self, kind: &TokenKind) -> bool {
        self.token.kind == *kind
    }

    pub fn check_keyword(&mut self, key: Symbol) -> bool {
        self.expected_tokens.push(ExpectTokenKind::Keyword(key));
        self.token.is_keyword(key)
    }

    pub fn expect(&mut self, kind: &TokenKind) -> Result<Recovered, Diagnostic<'a>> {
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
        f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing)> {
        self.parse_delim_comma_seq(Delimiter::Paren, f)
    }


    pub fn parse_delim_comma_seq<T>(
        &mut self,
        delim: Delimiter,
        f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing)>{
        self.parse_delim_seq(
            &TokenKind::OpenDelim(delim),
            &TokenKind::CloseDelim(delim),
            SequenceSeparator::new(TokenKind::Comma),
            f,
        )
    }

    pub fn parse_delim_seq<T>(
        &mut self,
        bra: &TokenKind,
        ket: &TokenKind,
        sep: SequenceSeparator,
        f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing)>{
        self.expect(bra);
        self.parse_seq_to_end(ket, sep, f)
    }

    pub fn parse_seq_to_end<T>(
        &mut self,
        end: &TokenKind,
        sep: SequenceSeparator,
        f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing)>{
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
        f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing, Recovered)>{
        self.parse_seq_before(&[end], sep, TokenExpectType::Yes, f)
    }

    pub fn parse_seq_before<T>(
        &mut self,
        kinds: &[&TokenKind],
        sep: SequenceSeparator,
        expect: TokenExpectType,
        mut f: impl FnMut(&mut Parser<'a>) -> ParseResult<'a, T>,
    ) -> ParseResult<'a, (ThinVec<T>, HasTrailing, Recovered)>{
        use TokenKind::*;

        let mut first = true;
        let mut trailing = HasTrailing::No;
        let mut recovered = Recovered::No;
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