pub mod define;
pub mod fatal_error;
pub mod interner;
pub mod source_file;
pub mod source_map;
pub mod symbol;

pub use crate::symbol::Symbol;

use std::cmp;
use std::ops::{Add, Sub};

pub trait Pos {
    fn from_usize(n: usize) -> Self;
    fn to_usize(self) -> usize;
    fn from_u32(n: u32) -> Self;
    fn to_u32(self) -> u32;
}

macro_rules! impl_pos {
    (
        $(
            $(#[$attr:meta])*
            $vis:vis struct $ident:ident($inner_vis:vis $inner_ty:ty);
        )*
    ) => {
        $(
            $(#[$attr])*
            $vis struct $ident($inner_vis $inner_ty);

            impl Pos for $ident {
                #[inline(always)]
                fn from_usize(n: usize) -> Self {
                    Self(n as $inner_ty)
                }

                #[inline(always)]
                fn to_usize(self) -> usize {
                    self.0 as usize
                }

                #[inline(always)]
                fn from_u32(n: u32) -> Self {
                    Self(n as $inner_ty)
                }

                #[inline(always)]
                fn to_u32(self) -> u32 {
                    self.0 as u32
                }
            }

            impl Add for $ident {
                type Output = Self;

                #[inline(always)]
                fn add(self, rhs: Self) -> Self::Output {
                    Self(self.0 + rhs.0)
                }
            }

            impl Sub for $ident {
                type Output = Self;

                #[inline(always)]
                fn sub(self, rhs: Self) -> Self::Output {
                    Self(self.0 - rhs.0)
                }
            }
        )*
    }
}

impl_pos! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct BytePos(pub u32);

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct RelativeBytePos(pub u32);

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct CharPos(pub u32);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

impl Span {
    pub fn new(mut lo: BytePos, mut hi: BytePos) -> Self {
        if hi < lo {
            std::mem::swap(&mut lo, &mut hi);
        }

        Self { lo, hi }
    }

    pub const DUMMY: Self = Self {
        lo: BytePos(0),
        hi: BytePos(0),
    };

    pub fn is_dummy(self) -> bool {
        self.lo == self.hi
    }

    pub fn to(self, end: Span) -> Span {
        Span::new(cmp::min(self.lo, end.lo), cmp::max(self.lo, end.lo))
    }

    pub fn with_hi(&self, hi: BytePos) -> Self {
        Self::new(self.lo, hi)
    }

    pub fn shrink_to_hi(self) -> Self {
        self.with_hi(self.hi)
    }
}

pub struct SessionGlobals {
    symbol_interner: interner::Interner,
}

scoped_tls::scoped_thread_local!(static SESSION_GLOBALS: SessionGlobals);

pub fn create_session_globals_then<R>(f: impl FnOnce() -> R) -> R {
    assert!(
        !SESSION_GLOBALS.is_set(),
        "SESSION_GLOBALS should never be overwritten! \
         Use another thread if you need another SessionGlobals"
    );
    let session_globals = SessionGlobals::new();
    SESSION_GLOBALS.set(&session_globals, f)
}

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

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: Symbol,
    pub span: Span,
}

impl Identifier {
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}
