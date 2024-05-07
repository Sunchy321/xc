pub mod cursor;
pub mod helper;
pub mod token;

pub(crate) const EOF: char = '\0';

#[derive(Clone, Debug)]
pub enum Base {
    Binary,
    Decimal,
    Hexadecimal
}