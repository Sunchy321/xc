use std::str::Chars;

use crate::EOF;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    prev: char,
    rest: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            chars: input.chars(),
            prev: EOF,
            rest: input.len(),
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    pub(crate) fn prev(&self) -> char {
        self.prev
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub(crate) fn curr_len(&self) -> u32 {
        (self.rest - self.chars.as_str().len()) as u32
    }

    pub(crate) fn reset_len(&mut self) {
        self.rest = self.chars.as_str().len();
    }

    pub fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    pub(crate) fn peek_second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF)
    }

    pub(crate) fn next(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        self.prev = c;

        Some(c)
    }

    pub(crate) fn eats(&mut self, mut pred: impl FnMut(char) -> bool) -> u32 {
        let mut count = 0u32;

        while pred(self.peek()) && !self.is_eof() {
            count += 1;
            self.next();
        }

        count
    }
}

