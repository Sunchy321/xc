pub mod level;
pub mod msg;
pub mod diag;
pub mod diag_ctx;

use diag::Diagnostic;
use diag_ctx::DiagnosticContext;
use level::Level;
use msg::DiagnosticMessage;
use xc_span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorGuaranteed(());

pub struct MultiSpan {
    primary: Vec<Span>,
    labels: Vec<(Span, DiagnosticMessage)>,
}

impl MultiSpan {
    pub fn new() -> Self {
        Self { primary: vec![], labels: vec![] }
    }

    pub fn from_span(span: Span) -> Self {
        Self { primary: vec![span], labels: vec![] }
    }

    pub fn get_primary(&self) -> Option<Span> {
        self.primary.first().cloned()
    }

    pub fn push_span_label(&mut self, span: Span, label: impl Into<DiagnosticMessage>) {
        self.labels.push((span, label.into()));
    }
}

impl From<Span> for MultiSpan {
    fn from(span: Span) -> MultiSpan {
        MultiSpan::from_span(span)
    }
}

pub trait IntoDiagnostic<'a> {
    #[must_use]
    fn into_diag(self, ctx: &'a DiagnosticContext, level: Level) -> Diagnostic<'a>;
}

impl IntoDiagnostic<'_> for &str {
    fn into_diag(self, ctx: &DiagnosticContext, level: Level) -> Diagnostic<'_> {
        unimplemented!()
    }
}