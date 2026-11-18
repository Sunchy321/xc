pub mod check;
pub mod diagnostic;
pub mod parse;
pub mod type_info;

pub use check::{CheckAnalysis, Program, analyze, check, check_with_diagnostics};
pub use diagnostic::{CheckDiagnostic, DiagnosticCode, DiagnosticSeverity};
pub use parse::{parse_file, parse_program, parse_program_with_diagnostics};
pub use type_info::{TypeInfo, TypedExpression};

mod borrow;
mod borrow_summary;
mod infer;
mod types;
