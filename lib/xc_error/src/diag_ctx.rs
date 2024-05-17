use std::cell::RefCell;

use crate::diag::Diagnostic;
use crate::level::Level;
use crate::msg::DiagnosticMessage;
use crate::MultiSpan;

pub struct DiagnosticContext {
    inner: RefCell<DiagnosticContextInner>
}

impl DiagnosticContext {
    pub fn create_error(&self, msg: impl Into<DiagnosticMessage>) -> Diagnostic<'_> {
        Diagnostic::new(self, Level::Error, msg)
    }

    pub fn create_span_error(&self, span: impl Into<MultiSpan>, msg: impl Into<DiagnosticMessage>) -> Diagnostic<'_> {
        self.create_error(msg).with_span(span)
    }
}

struct DiagnosticContextInner {
}