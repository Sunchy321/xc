use std::cell::RefCell;

use crate::diag::Diagnostic;
use crate::level::Level;
use crate::msg::DiagnosticMessage;
use crate::{ErrorGuaranteed, IntoDiagnostic, MultiSpan};

pub struct DiagnosticContext {
    inner: RefCell<DiagnosticContextInner>
}

impl DiagnosticContext {
    pub fn new() -> Self {
        Self { inner: RefCell::new(DiagnosticContextInner::new()) }
    }

    pub fn struct_error(&self, msg: impl Into<DiagnosticMessage>) -> Diagnostic<'_> {
        Diagnostic::new(self, Level::Error, msg)
    }

    pub fn struct_span_error(&self, span: impl Into<MultiSpan>, msg: impl Into<DiagnosticMessage>) -> Diagnostic<'_> {
        self.struct_error(msg).with_span(span)
    }

    pub fn create_error<'a>(&'a self, err: impl IntoDiagnostic<'a>) -> Diagnostic<'a> {
        err.into_diag(self, Level::Error)
    }

    pub fn emit_error<'a>(&'a self, err: impl IntoDiagnostic<'a>) -> ErrorGuaranteed {
        self.create_error(err).emit()
    }
}

struct DiagnosticContextInner {
}

impl DiagnosticContextInner {
    fn new() -> Self {
        Self {}
    }
}