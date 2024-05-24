use std::rc::Rc;

use xc_error::diag_ctx::DiagnosticContext;
use xc_span::source_map::SourceMap;

use crate::parser::op::OpContext;

pub struct ParseSession {
    pub diag_ctx: DiagnosticContext,
    pub op_ctx: OpContext,

    source_map: Rc<SourceMap>,
}

impl ParseSession {
    pub fn new() -> Self {

        Self {
            diag_ctx: DiagnosticContext::new(),
            op_ctx: OpContext::new(),
            source_map: Rc::new(SourceMap::new()),
        }
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }
}
