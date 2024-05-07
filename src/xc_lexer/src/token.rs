use self::TokenKind::*;
use crate::cursor::Cursor;
use crate::helper::*;
use crate::Base;

#[derive(Clone)]
pub enum TokenKind {
    LineComment,
    BlockComment { terminated: bool },

    Whitespace,

    Identifier,

    Integer { base: Base, is_empty: bool },

    Slash,

    Unknown,
    Eof,
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl<'a> Cursor<'a> {
    pub fn next_token(&mut self) -> Token {
        let first = match self.next() {
            Some(c) => c,
            None => return Token { kind: Eof, len: 0 },
        };

        let kind = match first {
            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },

            c if is_whitespace(c) => self.whitespace(),

            c if is_id_start(c) => self.identifier(),

            c @ '0'..='9' => self.numeric(c),

            _ => Unknown,
        };

        let token = Token {
            kind,
            len: self.curr_len(),
        };
        self.reset_len();
        token
    }

    fn line_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.peek() == '/');

        self.next();
        self.eats(|c| c != '\n');

        LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.peek() == '*');

        self.next();

        let mut depth = 1usize;

        while let Some(c) = self.next() {
            match c {
                '/' => {
                    if self.peek() == '*' {
                        self.next();
                        depth += 1;
                    }
                }

                '*' => {
                    if self.peek() == '/' {
                        self.next();
                        depth -= 1;

                        if depth == 0 {
                            break;
                        }
                    }
                }

                _ => (),
            }
        }

        BlockComment {
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eats(is_whitespace);
        Whitespace
    }

    fn identifier(&mut self) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));
        self.eats(is_id_continue);
        Identifier
    }

    fn numeric(&mut self, first: char) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        let mut base = Base::Decimal;
        if first == '0' {
            match self.peek() {
                'b' => {
                    base = Base::Binary;
                    self.next();
                    if !self.eat_decimals() {
                        return Integer {
                            base,
                            is_empty: true,
                        };
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.next();
                    if !self.eat_hexadecimals() {
                        return Integer {
                            base,
                            is_empty: true,
                        };
                    }
                }
                '0'..='9' | '_' => {
                    self.eat_decimals();
                }
                '.' | 'e' | 'E' => {}
                _ => return Integer { base, is_empty: false }
            }
        } else {
            self.eat_decimals();
        }

        match self.peek() {

        }
    }

    fn eat_decimals(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => match self.peek_second() {
                    '0'..='9' => {
                        has_digits = true;
                        self.next();
                        self.next();
                    }
                    _ => break,
                },
                '0'..='9' => {
                    has_digits = true;
                    self.next();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimals(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '_' => match self.peek_second() {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        has_digits = true;
                        self.next();
                        self.next();
                    }
                },
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.next();
                }
                _ => break,
            }
        }
        has_digits
    }
}
