use thin_vec::ThinVec;

use crate::decl::Qual;
use crate::func::{FuncBody, FuncParam, FuncTail};
use crate::ptr::P;

#[derive(Clone, Debug)]
pub struct Lambda {
    pub immediate: bool,
    pub quals: ThinVec<Qual>,
    pub params: ThinVec<FuncParam>,
    pub tail: FuncTail,
    pub body: FuncBody,
}
