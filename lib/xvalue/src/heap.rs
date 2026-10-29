//! 运行时堆：每个任务域一个 arena，M0 阶段无 GC，对象随 `Heap` 整体释放。
//!
//! 不变量（unsafe 依据，全部封闭在本模块）：
//! - 对象存于 `Vec<Box<Obj>>`，一经分配**永不移动**，`Heap` 存活期间 `ObjRef` 恒有效；
//! - 值不跨任务域（spawn 深拷贝转移，见 vm-bytecode-design-2026-10.md §九），
//!   因此无需线程同步。

use std::fmt;
use std::ptr::NonNull;

use crate::value::Value;

pub struct Obj {
    kind: ObjKind,
}

pub enum ObjKind {
    Str(Box<str>),
    Arr(Vec<Value>),
    /// place 单元：被取引用的局部/捕获的稳定存储。恒等 = 地址（引用恒等语义）。
    Cell(Value),
    /// 投影引用 `&arr[i]`：(数组对象, 捕获下标)。访问时按普通下标语义报错。
    ElemRef { arr: ObjRef, idx: i64 },
    /// 闭包：函数表索引 + 捕获环境（M1：env 槽恒为 cell 对象）。
    Closure { func: u32, env: Box<[Value]> },
}

/// 堆对象句柄：非移动堆上的稳定地址，place 引用（cell/投影）恒等性的载体。
#[derive(Clone, Copy)]
pub struct ObjRef(NonNull<Obj>);

impl PartialEq for ObjRef {
    /// 引用恒等：按 place 地址比较（忽略读写权限——权限在类型层）。
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl ObjRef {
    pub fn kind(self) -> &'static ObjKind {
        // SAFETY: 见模块注释——Heap 存活期间对象地址恒定。
        unsafe { &self.0.as_ref().kind }
    }

    pub fn as_str(self) -> Option<&'static str> {
        match self.kind() {
            ObjKind::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_arr(self) -> Option<&'static Vec<Value>> {
        match self.kind() {
            ObjKind::Arr(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_cell(self) -> Option<Value> {
        match self.kind() {
            ObjKind::Cell(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_elem_ref(self) -> Option<(ObjRef, i64)> {
        match self.kind() {
            ObjKind::ElemRef { arr, idx } => Some((*arr, *idx)),
            _ => None,
        }
    }

    /// 可变访问。安全性依据：值语义下每个对象有唯一主人（别名只在显式 `&`
    /// 之下存在，且由前端借用检查保证），运行时不经此路径产生悬垂别名。
    pub fn kind_mut(mut self) -> &'static mut ObjKind {
        // SAFETY: 见模块注释与唯一所有权不变量。
        unsafe { &mut self.0.as_mut().kind }
    }

    pub fn as_arr_mut(self) -> Option<&'static mut Vec<Value>> {
        match self.kind_mut() {
            ObjKind::Arr(a) => Some(a),
            _ => None,
        }
    }
}

impl fmt::Debug for ObjRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<obj {:p}>", self.0.as_ptr())
    }
}

pub struct Heap {
    objs: Vec<Box<Obj>>,
}

impl Heap {
    pub fn new() -> Self {
        Heap { objs: Vec::new() }
    }

    pub fn alloc(&mut self, kind: ObjKind) -> ObjRef {
        let obj = Box::new(Obj { kind });
        let r = ObjRef(NonNull::from(&*obj));
        self.objs.push(obj);
        r
    }

    pub fn len(&self) -> usize {
        self.objs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.objs.is_empty()
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_keeps_stable_addresses() {
        let mut heap = Heap::new();
        let a = heap.alloc(ObjKind::Str("a".into()));
        for i in 0..100 {
            heap.alloc(ObjKind::Arr(vec![Value::Int(i)]));
        }
        assert_eq!(a.as_str(), Some("a"));
    }
}
