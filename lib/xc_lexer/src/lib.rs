pub mod cursor;
pub mod helper;
pub mod token;
pub mod unescape;

pub use token::*;

pub(crate) const EOF: char = '\0';

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Base {
    Binary,
    Decimal,
    Hexadecimal
}

pub fn strip_shebang(input: &str) -> Option<usize> {
    if let Some(input_tail) = input.strip_prefix("#!") {
        Some(2 + input_tail.lines().next().unwrap_or_default().len())
    } else {
        None
    }
}