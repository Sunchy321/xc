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