use std::borrow::Cow;

#[derive(Clone, Debug)]
pub enum DiagnosticMessage {
    String(Cow<'static, str>),
}

impl DiagnosticMessage {
    pub fn with_sub(&self, sub: SubDiagnosticMessage) -> Self {
        let attr = match sub {
            SubDiagnosticMessage::String(s) => DiagnosticMessage::String(s)
        };

        match self {
            DiagnosticMessage::String(s) => DiagnosticMessage::String(s.clone())
        }
    }
}

impl From<String> for DiagnosticMessage {
    fn from(s: String) -> Self {
        Self::String(Cow::Owned(s))
    }
}

pub enum SubDiagnosticMessage {
    String(Cow<'static, str>),
}

impl From<String> for SubDiagnosticMessage {
    fn from(s: String) -> Self {
        Self::String(Cow::Owned(s))
    }
}

impl From<&'static str> for SubDiagnosticMessage {
    fn from(s: &'static str) -> Self {
        Self::String(Cow::Borrowed(s))
    }
}