use std::borrow::Cow;

pub enum DiagnosticMessage {
    String(Cow<'static, str>),
}