pub mod cursor;
pub mod helper;
pub mod token;

#[derive(Clone)]
pub enum Base {
    Binary,
    Decimal,
    Hexadecimal
}