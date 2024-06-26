use std::vec;
use std::ops::{Deref, DerefMut};
use std::fmt;

use xc_span::Span;

use crate::{diag_ctx::DiagnosticContext, level::Level, msg::{DiagnosticMessage, SubDiagnosticMessage}, ErrorGuaranteed, MultiSpan};

pub struct Diagnostic<'a> {
    pub ctx: &'a DiagnosticContext,

    diag: Option<Box<DiagnosticInner>>
}

impl Deref for Diagnostic<'_> {
    type Target = DiagnosticInner;

    fn deref(&self) -> &Self::Target {
        self.diag.as_ref().unwrap()
    }
}

impl DerefMut for Diagnostic<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.diag.as_mut().unwrap()
    }
}

impl fmt::Debug for Diagnostic<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.diag.fmt(f)
    }
}

macro_rules! with {
    {
        $(#[$attrs:meta])*
        pub fn ($f:ident, $with_f:ident)(&mut $self:ident, $($name:ident: $ty:ty),* $(,)?) -> &mut Self {
            $($body:tt)*
        }
    } => {
        // The original function.
        $(#[$attrs])*
        #[doc = concat!("See [`Diag::", stringify!($f), "()`].")]
        pub fn $f(&mut $self, $($name: $ty),*) -> &mut Self {
            $($body)*
        }

        // The `with_*` variant.
        $(#[$attrs])*
        #[doc = concat!("See [`Diag::", stringify!($f), "()`].")]
        pub fn $with_f(mut $self, $($name: $ty),*) -> Self {
            $self.$f($($name),*);
            $self
        }
    };
}

impl<'a> Diagnostic<'a> {
    pub fn new(ctx: &'a DiagnosticContext, level: Level, msg: impl Into<DiagnosticMessage>) -> Self {
        Self {
            ctx,
            diag: Some(Box::new(DiagnosticInner::new(level, msg)))
        }
    }

    with! {
        pub fn (span, with_span)(&mut self, span: impl Into<MultiSpan>) -> &mut Self {
            self.span = span.into();

            if let Some(span) = self.span.get_primary() {
                self.sort_span = span;
            }

            self
        }
    }

    with! {
        pub fn (span_label, with_span_label)(&mut self, span: Span, label: impl Into<SubDiagnosticMessage>) -> &mut Self {
            let msg = self.sub_msg_to_main(label);
            self.span.push_span_label(span, msg);
            self
        }
    }

    pub(crate) fn sub_msg_to_main(&self, sub: impl Into<SubDiagnosticMessage>) -> DiagnosticMessage {
        self.deref().sub_msg_to_main(sub)
    }

    pub fn emit(self) -> ErrorGuaranteed {
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticInner {
    pub(crate) level: Level,

    pub messages: Vec<DiagnosticMessage>,
    pub span: MultiSpan,

    pub sort_span: Span,
}

impl DiagnosticInner {
    pub fn new<M: Into<DiagnosticMessage>>(level: Level, msg: M) -> Self {
        Self {
            level,
            messages: vec![msg.into()],
            span: MultiSpan::new(),
            sort_span: Span::DUMMY
        }
    }

    fn sub_msg_to_main(&self, sub: impl Into<SubDiagnosticMessage>) -> DiagnosticMessage {
        let msg = self.messages.iter().map(|m| m).next().expect("diagnostic with no message");

        msg.with_sub(sub.into())
    }
}