use std::cell::RefCell;

use crate::Symbol;

pub struct Interner(RefCell<InternerImpl>);

impl Interner {
    pub fn new() -> Self {
        Interner(RefCell::new(InternerImpl {
            alloc: bumpalo::Bump::new(),
            strings: indexmap::IndexSet::new(),
        }))
    }

}

struct InternerImpl {
    alloc: bumpalo::Bump,
    strings: indexmap::IndexSet<&'static str>,
}

impl Interner {
    pub fn intern(&self, string: &str) -> u32 {
        let mut interner = self.0.borrow_mut();

        if let Some(idx) = interner.strings.get_index_of(string) {
            return idx as u32;
        }

        let string: &str = interner.alloc.alloc(string);

        let string: &'static str = unsafe { &*(string as *const str) };

        let (idx, is_new) = interner.strings.insert_full(string);
        debug_assert!(is_new);

        idx as u32
    }

    pub fn has(&self, string: &str) -> bool {
        self.0.borrow_mut().strings.contains(string)
    }

    pub(crate) fn get(&self, symbol: Symbol) -> &str {
        self.0.borrow_mut().strings.get_index(symbol.0 as usize).unwrap()
    }

    pub fn get_all(&self) -> Vec<u32> {
        let interner = self.0.borrow();

        interner.strings.iter().enumerate().map(|f| f.0 as u32).collect()
    }
}