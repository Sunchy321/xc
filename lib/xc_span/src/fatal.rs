use std::panic::resume_unwind;

#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct FatalError;

impl FatalError {
    pub fn raise() -> ! {
        resume_unwind(Box::new(FatalError));
    }
}

pub struct FatalErrorMarker;