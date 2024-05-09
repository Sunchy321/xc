use crate::ptr::P;
use crate::stmt::Block;

#[derive(Clone)]
pub struct Lambda {
    pub block: P<Block>,
}
