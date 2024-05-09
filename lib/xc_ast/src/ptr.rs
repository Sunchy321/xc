#[derive(Clone)]
pub struct P<T> {
    ptr: Box<T>,
}