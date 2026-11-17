pub mod heap;
pub mod value;

pub use heap::{Heap, Obj, ObjKind, ObjRef, TaskState};
pub use value::{lit_scalar, write_value, Value};
