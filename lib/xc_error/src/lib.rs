pub mod level;
pub mod msg;
pub mod diag;
pub mod diag_ctx;

use msg::DiagnosticMessage;
use xc_span::Span;

pub struct MultiSpan {
    primary: Vec<Span>,
    labels: Vec<(Span, DiagnosticMessage)>,
}

impl MultiSpan {
    pub fn new() -> Self {
        Self { primary: vec![], labels: vec![] }
    }

    pub fn get_primary(&self) -> Option<Span> {
        self.primary.first().cloned()
    }
}