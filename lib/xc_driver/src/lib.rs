use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use std::process;
use std::time::Instant;

use xc_error::fatal::{FatalError, FatalErrorMarker};
use xc_error::ErrorGuaranteed;

pub fn main() -> ! {
    let start_time = Instant::now();

    let exit_code = catch_with_exit_code(|| {
        Ok(())
    });

    process::exit(exit_code);
}

pub fn catch_fatal_error<R>(f: impl FnOnce() -> R) -> Result<R, FatalError> {
    catch_unwind(AssertUnwindSafe(f)).map_err(|value| {
        if value.is::<FatalErrorMarker>() {
            FatalError
        } else {
            resume_unwind(value)
        }
    })
}

pub const EXIT_SUCCESS: i32 = 0;
pub const EXIT_FAILURE: i32 = 1;

pub fn catch_with_exit_code(f: impl FnOnce() -> Result<(), ErrorGuaranteed>) -> i32 {
    match catch_fatal_error(f) {
        Ok(Ok(())) => EXIT_SUCCESS,
        _ => EXIT_FAILURE,
    }
}

pub struct RunCompiler {
}

impl RunCompiler {
    pub fn new() -> Self {
        Self { }
    }
}

