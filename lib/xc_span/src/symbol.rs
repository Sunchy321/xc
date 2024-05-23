use crate::with_session_globals;
use std::fmt;

pub mod kw {
    pub use crate::define::kw_generated::*;
}

pub mod op {
    pub use crate::define::op_generated::*;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub(crate) u32);

impl Symbol {
    pub(crate) const fn new(idx: u32) -> Self {
        Self(idx)
    }

    pub fn has_intern(string: &str) -> bool {
        with_session_globals(|g| g.symbol_interner.has(string))
    }

    pub fn intern(string: &str) -> Self {
        with_session_globals(|g| Symbol::new(g.symbol_interner.intern(string)))
    }

    pub fn is_keyword(self) -> bool {
        self <= kw::While
    }

    pub fn is_bool_lit(self) -> bool {
        self == kw::True || self == kw::False
    }

    pub fn as_str(&self) -> &str {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&str, &str>(session_globals.symbol_interner.get(*self))
        })
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}