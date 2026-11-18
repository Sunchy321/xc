use std::fmt;

use xc_span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticCode {
    Syntax,
    Name,
    Unsupported,
    Type,
    Internal,
}

impl DiagnosticCode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Syntax => "x.syntax",
            Self::Name => "x.name",
            Self::Unsupported => "x.unsupported",
            Self::Type => "x.type",
            Self::Internal => "x.internal",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CheckDiagnostic {
    pub code: DiagnosticCode,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub message: String,
}

impl CheckDiagnostic {
    pub fn new(code: DiagnosticCode, span: Span, message: impl Into<String>) -> Self {
        Self {
            code,
            severity: DiagnosticSeverity::Error,
            span,
            message: message.into(),
        }
    }
}

impl fmt::Display for CheckDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}
