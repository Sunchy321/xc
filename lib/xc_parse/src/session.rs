use xc_error::diag_ctx::DiagnosticContext;

use crate::parser::op::OpContext;

pub struct ParseSession {
    pub diag_ctx: DiagnosticContext,
    pub op_ctx: OpContext,
}
