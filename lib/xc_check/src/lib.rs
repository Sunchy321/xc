pub mod check;
pub mod parse;

pub use check::{check, Program};
pub use parse::{parse_file, parse_program};
