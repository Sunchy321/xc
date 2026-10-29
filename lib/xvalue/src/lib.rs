pub mod heap;
pub mod value;

pub use heap::{Heap, Obj, ObjKind, ObjRef};
pub use value::{lit_scalar, write_value, Value};
