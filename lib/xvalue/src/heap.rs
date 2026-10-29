//! 运行时堆：每个任务域一个 arena，M0 阶段无 GC，对象随 `Heap` 整体释放。
//!
//! 不变量（unsafe 依据，全部封闭在本模块）：
//! - 对象存于 `Vec<Box<Obj>>`，一经分配**永不移动**，`Heap` 存活期间 `ObjRef` 恒有效；
//! - 值不跨任务域（spawn 深拷贝转移，见 vm-bytecode-design-2026-10.md §九），
//!   因此无需线程同步。

use std::cell::Cell;
use std::fmt;
use std::ptr::NonNull;

use crate::value::Value;

pub struct Obj {
    kind: ObjKind,
    /// GC 标记位（域内单线程，内部可变性即可）。
    mark: Cell<bool>,
}

/// GC 触发阈值的下限；每次回收后按存活数翻倍（简易自适应）。
pub const GC_MIN_THRESHOLD: usize = 64;

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

    pub fn is_marked(self) -> bool {
        // SAFETY: Heap 存活期间对象地址恒定；Cell 提供内部可变性。
        unsafe { self.0.as_ref() }.mark.get()
    }

    pub fn set_marked(self) {
        // SAFETY: 同上。
        unsafe { self.0.as_ref() }.mark.set(true);
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
    /// 存活对象数达到该值时，下一个分配 safepoint 触发回收。
    next_gc: usize,
    /// 压力模式（XVM_GC_EVERY_ALLOC）：每个分配 safepoint 都回收。
    gc_stress: bool,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objs: Vec::new(),
            next_gc: GC_MIN_THRESHOLD,
            gc_stress: false,
        }
    }

    pub fn alloc(&mut self, kind: ObjKind) -> ObjRef {
        let obj = Box::new(Obj {
            kind,
            mark: Cell::new(false),
        });
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

    /// 压力模式开关（解释器从 XVM_GC_EVERY_ALLOC 读入一次）。
    pub fn set_gc_stress(&mut self, on: bool) {
        self.gc_stress = on;
    }

    /// 当前是否处于分配 safepoint 且需要回收。
    pub fn should_collect(&self) -> bool {
        self.gc_stress || self.objs.len() >= self.next_gc
    }

    /// 精确 mark-sweep：标记 `root_slices` 中每个 Value 可达的对象，
    /// 清扫其余。非移动堆——`retain` 只搬动 `Box` 指针值，`Obj` 地址恒定，
    /// 存活对象的 `ObjRef` 全部继续有效。返回 (回收数, 存活数)。
    pub fn collect(&mut self, root_slices: &[&[Value]]) -> (usize, usize) {
        // 标记：显式工作表（深链/循环不炸栈，循环由标记位天然终止）。
        let mut work: Vec<ObjRef> = Vec::new();
        for slice in root_slices {
            for v in slice.iter() {
                if let Value::Obj(o) = v {
                    work.push(*o);
                }
            }
        }
        while let Some(o) = work.pop() {
            if o.is_marked() {
                continue;
            }
            o.set_marked();
            match o.kind() {
                ObjKind::Str(_) => {}
                ObjKind::Arr(items) => {
                    for v in items.iter() {
                        if let Value::Obj(child) = v {
                            work.push(*child);
                        }
                    }
                }
                ObjKind::Cell(v) => {
                    if let Value::Obj(child) = v {
                        work.push(*child);
                    }
                }
                ObjKind::ElemRef { arr, .. } => work.push(*arr),
                ObjKind::Closure { env, .. } => {
                    for v in env.iter() {
                        if let Value::Obj(child) = v {
                            work.push(*child);
                        }
                    }
                }
            }
        }
        // 清扫：retain 同步清掉存活对象的标记位。
        let before = self.objs.len();
        self.objs.retain(|o| o.mark.replace(false));
        let freed = before - self.objs.len();
        self.next_gc = (self.objs.len() * 2).max(GC_MIN_THRESHOLD);
        (freed, self.objs.len())
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

    // ---- M2：mark-sweep ----

    fn roots_of(vecs: &[Vec<Value>]) -> Vec<&[Value]> {
        vecs.iter().map(|v| v.as_slice()).collect()
    }

    #[test]
    fn collect_drops_unreachable_keeps_reachable_chain() {
        let mut heap = Heap::new();
        let keep_arr = heap.alloc(ObjKind::Arr(vec![Value::Int(1)]));
        let _garbage = heap.alloc(ObjKind::Arr(vec![Value::Int(2)]));
        let _garbage2 = heap.alloc(ObjKind::Str("dead".into()));
        // 存活链：cell → keep_arr（cell 可达则链上全部可达）。
        let cell = heap.alloc(ObjKind::Cell(Value::Obj(keep_arr)));
        heap.collect(&roots_of(&[vec![Value::Obj(cell)]]));
        assert_eq!(heap.len(), 2, "cell + keep_arr survive; garbage swept");
        // 存活对象地址稳定，数据完好。
        assert_eq!(cell.as_cell(), Some(Value::Obj(keep_arr)));
        assert_eq!(keep_arr.as_arr().map(|a| a.as_slice()), Some(&[Value::Int(1)][..]));
    }

    #[test]
    fn collect_reclaims_cycles() {
        let mut heap = Heap::new();
        // 无外部根的环：cell → 数组 → 同一 cell。
        let cell = heap.alloc(ObjKind::Cell(Value::Nil));
        let arr = heap.alloc(ObjKind::Arr(vec![Value::Obj(cell)]));
        if let Some(a) = arr.as_arr_mut() {
            a[0] = Value::Obj(cell);
        }
        if let ObjKind::Cell(slot) = cell.kind_mut() {
            *slot = Value::Obj(arr);
        }
        heap.collect(&roots_of(&[vec![]]));
        assert_eq!(heap.len(), 0, "unrooted cycle fully reclaimed");
    }

    #[test]
    fn collect_keeps_closure_env_and_elemref_targets() {
        let mut heap = Heap::new();
        let arr = heap.alloc(ObjKind::Arr(vec![Value::Int(7)]));
        let eref = heap.alloc(ObjKind::ElemRef { arr, idx: 0 });
        let cell = heap.alloc(ObjKind::Cell(Value::Int(9)));
        let closure = heap.alloc(ObjKind::Closure {
            func: 0,
            env: Box::new([Value::Obj(cell)]),
        });
        heap.collect(&roots_of(&[vec![Value::Obj(eref), Value::Obj(closure)]]));
        assert_eq!(heap.len(), 4, "elemref keeps its array; closure keeps env");
        assert_eq!(eref.as_elem_ref().unwrap().0, arr);
        assert_eq!(cell.as_cell(), Some(Value::Int(9)));
    }

    #[test]
    fn collect_scales_threshold() {
        let mut heap = Heap::new();
        assert!(heap.next_gc >= GC_MIN_THRESHOLD);
        heap.next_gc = 8;
        for i in 0..20 {
            heap.alloc(ObjKind::Arr(vec![Value::Int(i)]));
        }
        // 无根回收后阈值按存活数重设，不低于下限。
        heap.collect(&roots_of(&[vec![]]));
        assert_eq!(heap.len(), 0);
        assert!(heap.next_gc >= GC_MIN_THRESHOLD);
    }
}
