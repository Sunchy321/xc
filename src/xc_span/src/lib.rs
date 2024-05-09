pub mod interner;
pub mod symbol;
pub mod define;

pub use crate::symbol::Symbol;

use std::ops::{Add, Sub, AddAssign};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn from_usize(n: usize) -> Self {
        Self(n as u32)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Add for BytePos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub for BytePos {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }

}

impl AddAssign for BytePos {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

#[derive(Clone, Copy)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(mut lo: BytePos, mut hi: BytePos) -> Self {
        if hi < lo {
            std::mem::swap(&mut lo, &mut hi);
        }

        Self { lo: lo.0, hi: hi.0 }
    }
}

pub struct SessionGlobals {
    symbol_interner: interner::Interner
}

scoped_tls::scoped_thread_local!(static SESSION_GLOBALS: SessionGlobals);

pub fn with_session_globals<R, F>(f: F) -> R
where
    F: FnOnce(&SessionGlobals) -> R,
{
    SESSION_GLOBALS.with(f)
}

impl SessionGlobals {
    pub(crate) fn new() -> Self {
        Self {
            symbol_interner: interner::Interner::new(),
        }
    }
}