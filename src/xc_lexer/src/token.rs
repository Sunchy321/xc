use self::LiteralKind::*;
use self::TokenKind::*;
use crate::cursor::Cursor;
use crate::helper::*;
use crate::Base;
use crate::EOF;

#[derive(Clone)]
pub enum TokenKind {
    LineComment,
    BlockComment {
        terminated: bool,
    },

    Whitespace,

    Identifier,
    Symbol {
        terminated: bool,
    },

    Operator,

    Literal {
        kind: LiteralKind,
        suffix_start: u32,
    },

    LambdaArgUnnamed,
    LambdaArgNamed,

    /// @
    At,
    /// #
    Pound,
    /// $
    Dollar,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// ;
    Semicolon,
    /// :
    Colon,
    /// ,
    Comma,

    /// {|
    OpenDict,
    /// |}
    CloseDict,
    /// '(
    SymbolOpen,
    /// ::
    ColonColon,

    Unknown,
    Eof,
}

#[derive(Clone, Debug)]
pub enum LiteralKind {
    Integer { base: Base, is_empty: bool },
    Floating { base: Base, empty_exponent: bool },
    String { terminated: bool },
    RawString { at_count: Option<u32> },
    MultilineString { quote_count: Option<u32> },
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

#[derive(Clone)]
pub enum RawStrError {
    /// Non `"` characters after `@`, e.g. `@@@a"abc"@@@`
    InvalidStarter { bad_char: char },
    /// The string was not terminated, e.g. `@@@"abc"@@`.
    NoTerminator {
        expected: u32,
        found: u32,
        possible_terminator_offset: Option<u32>,
    },
    /// Too many `@`s exist, e.g. `@"abc"@@`
    TooManyTerminator { found: u32 },
    /// Non-terminated string
    OpenInterploration,
}

#[derive(Clone)]
pub enum MultilineStrError {
    /// The string was not terminated, e.g. `""""abc"""`.
    NoTerminator {
        expected: u32,
        found: u32,
        possible_terminator_offset: Option<u32>,
    },
    /// Too many `"`s exist, e.g. `""""abc"""""`
    TooManyTerminator { found: u32 },
    /// Non-terminated string
    OpenInterploration,
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
                _ => self.operator(),
            },

            c if is_whitespace(c) => self.whitespace(),

            c if is_id_start(c) => self.identifier(),

            c @ '0'..='9' => {
                let kind = self.numeric(c);
                let suffix_start = self.curr_len();
                self.eat_suffix();
                Literal { kind, suffix_start }
            }

            '\'' => match self.peek() {
                c if is_id_start(c) => {
                    self.eat_identifier();
                    Symbol { terminated: true }
                }

                '(' => {
                    self.next();
                    SymbolOpen
                }

                _ => self.operator(),
            },

            '#' => Pound,
            '(' => OpenParen,
            ')' => CloseParen,
            '[' => OpenBracket,
            ']' => CloseBracket,
            ';' => Semicolon,
            ',' => Comma,
            '}' => CloseBrace,

            ':' => match self.peek() {
                ':' => {
                    self.next();
                    ColonColon
                }

                _ => Colon,
            },

            '{' => match self.peek() {
                '|' => {
                    self.next();
                    OpenDict
                }

                _ => OpenBrace,
            },

            '|' => match self.peek() {
                '}' => {
                    self.next();
                    CloseDict
                }

                _ => self.operator(),
            },

            '$' => match self.peek() {
                '0'..='9' => {
                    self.eats(|c| c.is_digit(10));
                    LambdaArgUnnamed
                }

                c if is_id_start(c) => {
                    self.eat_identifier();
                    LambdaArgNamed
                }

                _ => Dollar,
            },

            '@' => match self.peek() {
                '@' | '"' => {
                    let at_count = self.raw_string();
                    let suffix_start = self.curr_len();
                    if at_count.is_ok() {
                        self.eat_suffix();
                    }
                    let kind = RawString {
                        at_count: at_count.ok(),
                    };
                    Literal { kind, suffix_start }
                }

                _ => At,
            },

            '"' => {
                if self.peek() == '"' && self.peek_second() == '"' {
                    let quote_count = self.multiline_string();
                    let suffix_start = self.curr_len();
                    if quote_count.is_ok() {
                        self.eat_suffix();
                    }
                    let kind = MultilineString {
                        quote_count: quote_count.ok(),
                    };
                    Literal { kind, suffix_start }
                } else {
                    let terminated = self.double_quoted_string();
                    let suffix_start = self.curr_len();
                    if terminated {
                        self.eat_suffix();
                    }
                    let kind = String { terminated };
                    Literal { kind, suffix_start }
                }
            }

            c if is_operator_part(c) => self.operator(),

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

    fn operator(&mut self) -> TokenKind {
        debug_assert!(is_operator_part(self.prev()));
        self.eats(is_operator_part);
        Operator
    }

    fn double_quoted_string(&mut self) -> bool {
        debug_assert!(self.prev() == '"');
        while let Some(c) = self.next() {
            match c {
                '"' => {
                    return true;
                }

                '\\' if self.peek() == '(' => {
                    self.next();
                    let res = self.eat_interplorations();

                    if !res {
                        return false;
                    }
                }

                '\\' if self.peek() == '\\' || self.peek() == '"' => {
                    // Bump again to skip escaped character.
                    self.next();
                }

                '\n' => return false,

                _ => (),
            }
        }

        // End of file reached.
        false
    }

    fn raw_string(&mut self) -> Result<u32, RawStrError> {
        debug_assert!(self.prev() == '@');
        let mut possible_terminator_offset = None;
        let mut at_max_count = 0;

        // Count opening '@' symbols.
        let at_start_count = self.eats(|c| c == '@') + 1;

        // Check that string is started.
        match self.next() {
            Some('"') => (),
            c => {
                let c = c.unwrap_or(EOF);
                return Err(RawStrError::InvalidStarter { bad_char: c });
            }
        }

        // Skip the string contents and on each '#' character met, check if this is
        // a raw string termination.
        loop {
            self.eats(|c| c != '"' && c != '\\' && c != '\n');

            if self.peek() == '\n' || self.is_eof() {
                return Err(RawStrError::NoTerminator {
                    expected: at_start_count,
                    found: at_max_count,
                    possible_terminator_offset,
                });
            }

            // interplorations for raw strings. e.g. `@"1 + 1 = \@(1+1)"@`
            if self.peek() == '\\' {
                self.next();

                let at_eaten = self.eats(|c| c == '@');

                if at_eaten == at_start_count && self.peek() == '(' {
                    self.next();

                    if !self.eat_interplorations() {
                        return Err(RawStrError::OpenInterploration);
                    }
                }

                continue;
            }

            // Eat closing double quote.
            self.next();

            // Eating closing `@` symbols. Too many `@` is eaten and becomes an error.
            let at_end_count = self.eats(|c| c == '@');

            if at_end_count == at_start_count {
                return Ok(at_start_count);
            } else if at_end_count > at_start_count {
                return Err(RawStrError::TooManyTerminator {
                    found: at_end_count,
                });
            }

            if at_end_count > at_max_count {
                // Keep track of possible terminators to give a hint about
                // where there might be a missing terminator
                possible_terminator_offset = Some(self.curr_len() - at_end_count);
                at_max_count = at_end_count;
            }
        }
    }

    fn multiline_string(&mut self) -> Result<u32, MultilineStrError> {
        debug_assert!(self.prev() == '"');
        let mut possible_terminator_offset = None;
        let mut quote_max_count = 0;

        // Count opening '"' symbols.
        let quote_start_count = self.eats(|c| c == '"') + 1;

        // Skip the string contents and on each '#' character met, check if this is
        // a raw string termination.
        loop {
            self.eats(|c| c != '"' && c != '\\' && c != '\n');

            if self.peek() == '\n' || self.is_eof() {
                return Err(MultilineStrError::NoTerminator {
                    expected: quote_start_count,
                    found: quote_max_count,
                    possible_terminator_offset,
                });
            }

            // interplorations for raw strings. e.g. `@"1 + 1 = \@(1+1)"@`
            if self.peek() == '\\' {
                self.next();

                let at_eaten = self.eats(|c| c == '@');

                if at_eaten == quote_start_count && self.peek() == '(' {
                    self.next();

                    if !self.eat_interplorations() {
                        return Err(MultilineStrError::OpenInterploration);
                    }
                }

                continue;
            }

            // Eating closing `"` symbols. Too many `"` is eaten and becomes an error.
            let at_end_count = self.eats(|c| c == '"');

            if at_end_count == quote_start_count {
                return Ok(quote_start_count);
            } else if at_end_count > quote_start_count {
                return Err(MultilineStrError::TooManyTerminator {
                    found: at_end_count,
                });
            }

            if at_end_count > quote_max_count {
                // Keep track of possible terminators to give a hint about
                // where there might be a missing terminator
                possible_terminator_offset = Some(self.curr_len() - at_end_count);
                quote_max_count = at_end_count;
            }
        }
    }

    fn numeric(&mut self, first: char) -> LiteralKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');

        if first == '0' {
            match self.peek() {
                'b' => {
                    self.next();
                    self.binary()
                }
                'x' => {
                    self.next();
                    self.hexadecimal()
                }
                _ => self.decimal(),
            }
        } else {
            self.decimal()
        }
    }

    fn binary(&mut self) -> LiteralKind {
        debug_assert!(self.prev() == 'b');
        let eaten = self.eat_decimals();
        Integer {
            base: Base::Binary,
            is_empty: !eaten,
        }
    }

    fn hexadecimal(&mut self) -> LiteralKind {
        debug_assert!(self.prev() == 'x');
        let eaten = self.eat_hexadecimals();

        if !eaten {
            return Integer {
                base: Base::Hexadecimal,
                is_empty: true,
            };
        }

        let mut eaten_dots = false;

        if self.peek() == '.' && self.peek_second().is_digit(16) {
            self.next();
            self.eat_hexadecimals();
            eaten_dots = true;
        }

        let mut eaten_exp = false;
        let mut empty_exp = false;

        if self.peek() == 'p' || self.peek() == 'P' {
            self.next();
            empty_exp = !self.eat_exponent();
            eaten_exp = true;
        }

        if eaten_dots || eaten_exp {
            Floating {
                base: Base::Hexadecimal,
                empty_exponent: empty_exp,
            }
        } else {
            Integer {
                base: Base::Hexadecimal,
                is_empty: false,
            }
        }
    }

    fn decimal(&mut self) -> LiteralKind {
        debug_assert!('0' < self.prev() && self.prev() <= '9');
        let eaten = self.eat_decimals();

        if !eaten {
            return Integer {
                base: Base::Decimal,
                is_empty: false,
            };
        }

        let mut eaten_dots = false;

        if self.peek() == '.' && self.peek_second().is_digit(10) {
            self.next();
            self.eat_decimals();
            eaten_dots = true;
        }

        let mut eaten_exp = false;
        let mut empty_exp = false;

        if self.peek() == 'e' || self.peek() == 'E' {
            self.next();
            empty_exp = !self.eat_exponent();
            eaten_exp = true;
        }

        if eaten_dots || eaten_exp {
            Floating {
                base: Base::Decimal,
                empty_exponent: empty_exp,
            }
        } else {
            Integer {
                base: Base::Decimal,
                is_empty: false,
            }
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
                    _ => break,
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

    fn eat_exponent(&mut self) -> bool {
        debug_assert!(
            self.prev() == 'e' || self.prev() == 'E' || self.prev() == 'p' || self.prev() == 'P'
        );

        if self.peek() == '+' || self.peek() == '-' {
            self.next();
        }

        if !self.peek().is_digit(10) {
            return false;
        }

        self.eat_decimals();
        true
    }

    fn eat_suffix(&mut self) {
        self.eat_identifier();
    }

    fn eat_identifier(&mut self) {
        if !is_id_start(self.peek()) {
            return;
        }

        self.next();
        self.eats(is_id_continue);
    }

    fn eat_interplorations(&mut self) -> bool {
        debug_assert!(self.prev() == '(');

        let mut depth = 1usize;

        loop {
            let t = self.next_token();
            match t {
                Token {
                    kind: OpenParen, ..
                } => depth += 1,
                Token {
                    kind: CloseParen, ..
                } => {
                    depth -= 1;
                    if depth == 0 {
                        return true;
                    }
                }
                Token { kind: Eof, .. } => return false,
                _ => (),
            }
        }
    }
}
