//! 解释器：单一值栈 + 帧窗口（M0 单 fiber；fiber/事件循环为 M4）。
//! 错误消息与旧栈式原型逐字兼容（`xc_driver/tests/golden.rs` 为基线）。
//!
//! 无类型检查器阶段的实现暂态：算术恒发 I64 族——`+` 仅整数（字符串拼接是
//! `~` → CONCAT，`+` 对 string 无定义）；`EQ/NE` 为单条动态指令。类型检查器
//! 落地后按静态类型分家。

use xvalue::{write_value, Heap, ObjKind, Value};

use crate::isa::Opcode;
use crate::lower::Module;

#[derive(Clone, Copy, PartialEq)]
enum Walk {
    /// 正常执行。
    None,
    /// cleanup 完成后恢复到该 ip（ENDSCOPE 的落点）。
    Resume(u32),
    ///RET 走栈：完成后按约定传播 ret_val + er。
    Ret,
    /// RAISE 走栈：完成后仅传播 er。
    Raise,
    /// PANIC 走栈：完成后继续向调用帧展开。
    Panic,
}

struct Frame {
    func: u32,
    ip: u32,
    base: u32,
    /// 本帧执行期间约定的栈长（base + regs）；被调帧返回时恢复到此。
    restore: usize,
    /// 闭包捕获环境（槽 = cell 对象）；非闭包帧为空。
    env: Vec<Value>,
    /// 错误寄存器 er（Nil = 成功）；被调返回时写回调用帧（约定 A）。
    err: Value,
    /// defer 清理体的入口 ip，内层在后。
    defers: Vec<u32>,
    /// defer 作用域标记（defers 的长度快照，ENDSCOPE 用）。
    marks: Vec<usize>,
    walk: Walk,
    walk_remaining: usize,
    /// RET 走栈期间暂存的返回值（cleanup 不得破坏）及其落点槽位。
    ret_val: Value,
    ret_slot: usize,
}

impl Frame {
    fn new(func: u32, ip: u32, base: u32, restore: usize, env: Vec<Value>) -> Self {
        Frame {
            func,
            ip,
            base,
            restore,
            env,
            err: Value::Nil,
            defers: Vec::new(),
            marks: vec![0],
            walk: Walk::None,
            walk_remaining: 0,
            ret_val: Value::Nil,
            ret_slot: 0,
        }
    }
}

pub fn run(module: &Module, source: &str) -> Result<String, String> {
    let mut heap = Heap::new();
    heap.set_gc_stress(std::env::var("XVM_GC_EVERY_ALLOC").is_ok());
    let gc_stress = std::env::var("XVM_GC_EVERY_ALLOC").is_ok();
    let mut slots = vec![Value::Void; module.num_slots];
    // cell 化槽位：加载期建 cell，此后 GETSLOT/SETSLOT 透明穿透。
    for (idx, marked) in module.cell_slots.iter().enumerate() {
        if *marked {
            let o = heap.alloc(ObjKind::Cell(Value::Void));
            slots[idx] = Value::Obj(o);
        }
    }
    // 帧窗口的底座：调用约定为「实参占被调帧前导寄存器」。
    let top_regs = module.funcs[0].regs as usize;
    let mut stack: Vec<Value> = vec![Value::Nil; top_regs];
    let mut frames: Vec<Frame> = vec![Frame::new(0, 0, 0, top_regs, Vec::new())];
    let mut out = String::new();
    let mut panic_payload: Option<Value> = None;

    // 分配 safepoint：栈、槽表、各帧捕获环境都是精确根集。在分配型指令的
    // 操作数尚在寄存器（栈槽）时调用——GC 只回收不可达对象，不影响执行。
    fn maybe_gc(
        heap: &mut Heap,
        stack: &[Value],
        slots: &[Value],
        frames: &[Frame],
        extra: &[Value],
    ) {
        if !heap.should_collect() {
            return;
        }
        let mut roots: Vec<&[Value]> = vec![stack, slots];
        for f in frames {
            roots.push(&f.env);
        }
        roots.push(extra);
        let (freed, kept) = heap.collect(&roots);
        if std::env::var("XVM_TRACE_GC").is_ok() {
            eprintln!("gc: freed {freed}, kept {kept}");
        }
    }

    loop {
        let fi = frames.last().unwrap().func as usize;
        let base = frames.last().unwrap().base as usize;
        let func = &module.funcs[fi];
        let ip = frames.last().unwrap().ip as usize;
        let instr = func.code[ip];
        frames.last_mut().unwrap().ip = ip as u32 + 1;
        let pos = func.lines[ip];

        macro_rules! rt {
            ($msg:expr) => {
                return Err(rt_err(&func.name, source, pos, $msg))
            };
        }

        match instr.op() {
            Opcode::Move => stack[base + instr.a() as usize] = stack[base + instr.b() as usize],
            Opcode::LoadI => stack[base + instr.a() as usize] = Value::Int(instr.sbx() as i64),
            Opcode::LoadK => {
                stack[base + instr.a() as usize] = module.consts[instr.bx() as usize];
            }
            Opcode::LoadNil => stack[base + instr.a() as usize] = Value::Nil,
            Opcode::LoadB => {
                stack[base + instr.a() as usize] = Value::Bool(instr.b() != 0);
            }
            Opcode::GetSlot => {
                let v = slots[instr.bx() as usize];
                stack[base + instr.a() as usize] = if module.cell_slots[instr.bx() as usize] {
                    match v {
                        Value::Obj(o) => o.as_cell().unwrap_or(Value::Nil),
                        _ => rt!("internal: cell slot holds a non-cell"),
                    }
                } else {
                    v
                };
            }
            Opcode::SetSlot => {
                let v = stack[base + instr.a() as usize];
                let idx = instr.bx() as usize;
                if module.cell_slots[idx] {
                    match slots[idx] {
                        Value::Obj(o) => match o.kind_mut() {
                            ObjKind::Cell(slot) => *slot = v,
                            _ => rt!("internal: cell slot holds a non-cell"),
                        },
                        _ => rt!("internal: cell slot holds a non-cell"),
                    }
                } else {
                    slots[idx] = v;
                }
            }
            Opcode::GetSlotCell => {
                let v = slots[instr.bx() as usize];
                match v {
                    Value::Obj(o) if matches!(o.kind(), ObjKind::Cell(_)) => {
                        stack[base + instr.a() as usize] = v;
                    }
                    _ => rt!("internal: GETSLOTCELL on a non-cell slot"),
                }
            }
            Opcode::AddI64 | Opcode::SubI64 | Opcode::MulI64 | Opcode::DivI64
            | Opcode::ModI64 => {
                let (dst, l, r) = (
                    base + instr.a() as usize,
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                let arith = |a: i64, b: i64| -> Result<i64, String> {
                    match instr.op() {
                        Opcode::AddI64 => Ok(a + b),
                        Opcode::SubI64 => Ok(a - b),
                        Opcode::MulI64 => Ok(a * b),
                        Opcode::DivI64 => {
                            if b == 0 {
                                Err("division by zero".to_string())
                            } else {
                                Ok(a / b)
                            }
                        }
                        _ => {
                            if b == 0 {
                                Err("modulo by zero".to_string())
                            } else {
                                Ok(a % b)
                            }
                        }
                    }
                };
                stack[dst] = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        Value::Int(arith(a, b).map_err(|m| rt_err(&func.name, source, pos, m))?)
                    }
                    // `+` 仅整数；字符串拼接是 `~`（CONCAT），`+` 对 string 无定义。
                    _ => rt!("arithmetic operands must be int"),
                };
            }
            Opcode::AddI => {
                let dst = base + instr.a() as usize;
                match stack[dst] {
                    Value::Int(a) => stack[dst] = Value::Int(a + instr.sbx() as i64),
                    _ => rt!("arithmetic operands must be int"),
                }
            }
            Opcode::NegI64 => {
                let dst = base + instr.a() as usize;
                match stack[base + instr.b() as usize] {
                    Value::Int(a) => stack[dst] = Value::Int(-a),
                    _ => rt!("negation operand must be int"),
                }
            }
            Opcode::Concat => {
                let gc_extra: Vec<Value> = {
                    let mut v: Vec<Value> = frames.iter().flat_map(|f| [f.err, f.ret_val]).collect();
                    if let Some(p) = &panic_payload {
                        v.push(*p);
                    }
                    v
                };
                maybe_gc(&mut heap, &stack, &slots, &frames, &gc_extra);
                let (dst, l, r) = (
                    base + instr.a() as usize,
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                match (l, r) {
                    (Value::Obj(a), Value::Obj(b)) => match (a.as_str(), b.as_str()) {
                        (Some(x), Some(y)) => {
                            let o = heap.alloc(ObjKind::Str(format!("{x}{y}").into()));
                            stack[dst] = Value::Obj(o);
                        }
                        _ => rt!("concat operands must be string"),
                    },
                    _ => rt!("concat operands must be string"),
                }
            }
            Opcode::Eq | Opcode::Ne => {
                let dst = base + instr.a() as usize;
                let eq = stack[base + instr.b() as usize] == stack[base + instr.c() as usize];
                stack[dst] = Value::Bool(if instr.op() == Opcode::Eq { eq } else { !eq });
            }
            Opcode::Lt | Opcode::Le => {
                let (dst, l, r) = (
                    base + instr.a() as usize,
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                stack[dst] = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        Value::Bool(if instr.op() == Opcode::Lt { a < b } else { a <= b })
                    }
                    _ => rt!("ordering operands must be int"),
                };
            }
            Opcode::Jmp | Opcode::JmpF | Opcode::JmpT => {
                let taken = match instr.op() {
                    Opcode::Jmp => true,
                    _ => match stack[base + instr.a() as usize] {
                        Value::Bool(b) => {
                            if instr.op() == Opcode::JmpF {
                                !b
                            } else {
                                b
                            }
                        }
                        _ => rt!("condition must be bool"),
                    },
                };
                if taken {
                    let target = match instr.op() {
                        Opcode::Jmp => ip as i64 + 1 + instr.sj() as i64,
                        _ => ip as i64 + 1 + instr.sbx() as i64,
                    } as u32;
                    frames.last_mut().unwrap().ip = target;
                }
            }
            Opcode::NewArr => {
                let gc_extra: Vec<Value> = {
                    let mut v: Vec<Value> = frames.iter().flat_map(|f| [f.err, f.ret_val]).collect();
                    if let Some(p) = &panic_payload {
                        v.push(*p);
                    }
                    v
                };
                maybe_gc(&mut heap, &stack, &slots, &frames, &gc_extra);
                let (dst, base_i, count) = (instr.a() as usize, instr.b() as usize, instr.c() as usize);
                let elems = stack[base + base_i..base + base_i + count].to_vec();
                let o = heap.alloc(ObjKind::Arr(elems));
                stack[base + dst] = Value::Obj(o);
            }
            Opcode::Len => {
                let dst = base + instr.a() as usize;
                match stack[base + instr.b() as usize] {
                    Value::Obj(o) => match o.as_arr() {
                        Some(a) => stack[dst] = Value::Int(a.len() as i64),
                        None => rt!("length of a non-array"),
                    },
                    _ => rt!("length of a non-array"),
                }
            }
            Opcode::GetElem => {
                let (dst, container, idx) = (
                    base + instr.a() as usize,
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                let i = match idx {
                    Value::Int(i) => i,
                    _ => rt!("array index on a non-array"),
                };
                match container {
                    Value::Obj(o) => match o.as_arr() {
                        Some(a) => {
                            if i < 0 || i as usize >= a.len() {
                                rt!(format!(
                                    "array index {i} out of bounds (len {})",
                                    a.len()
                                ));
                            }
                            stack[dst] = a[i as usize];
                        }
                        None => rt!("array index on a non-array"),
                    },
                    _ => rt!("array index on a non-array"),
                }
            }
            Opcode::SetElem => {
                let (v, container, idx) = (
                    stack[base + instr.a() as usize],
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                let i = match idx {
                    Value::Int(i) => i,
                    _ => rt!("array index on a non-array"),
                };
                match container {
                    Value::Obj(o) => match o.as_arr_mut() {
                        Some(a) => {
                            if i < 0 || i as usize >= a.len() {
                                rt!(format!(
                                    "array index {i} out of bounds (len {})",
                                    a.len()
                                ));
                            }
                            a[i as usize] = v;
                        }
                        None => rt!("array index on a non-array"),
                    },
                    _ => rt!("array index on a non-array"),
                }
            }
            Opcode::CallK => {
                // 调用约定：被调帧基址 = 调用帧的 A 寄存器（实参 A..A+C-1 原地成为
                // 被调者 r0..）；返回值固定经被调者 r0 传回，落在调用者的 A 上。
                let callee = &module.funcs[instr.b() as usize];
                let base_new = base + instr.a() as usize;
                let need = base_new + callee.regs as usize;
                if stack.len() < need {
                    stack.resize(need, Value::Nil);
                }
                frames.push(Frame::new(instr.b(), 0, base_new as u32, need, Vec::new()));
            }
            Opcode::CallNat => {
                let n = instr.c() as usize;
                let start = base + instr.a() as usize;
                let vals: Vec<Value> = stack[start..start + n].to_vec();
                match module.natives[instr.b() as usize] {
                    "print" => {
                        for (i, v) in vals.iter().enumerate() {
                            if i > 0 {
                                out.push(' ');
                            }
                            write_value(v, &mut out).map_err(|_| "print failed".to_string())?;
                        }
                        out.push('\n');
                    }
                    other => rt!(format!("unknown native: {other}")),
                }
                stack[start] = Value::Void;
            }
            Opcode::Ret => {
                let ret = stack[base + instr.a() as usize];
                let f = frames.last_mut().unwrap();
                if !f.defers.is_empty() {
                    // 先走 defer 清理链：返回值暂存，走完再传播（约定 D）。
                    f.ret_val = ret;
                    f.ret_slot = base + instr.a() as usize;
                    f.walk = Walk::Ret;
                    f.walk_remaining = f.defers.len();
                    f.ip = *f.defers.last().unwrap();
                } else {
                    // 返回值在被调者 r0，槽位即调用者的 A 寄存器——原位无需移动。
                    let f_err = f.err;
                    frames.pop();
                    if frames.is_empty() {
                        if f_err != Value::Nil {
                            return Err(uncaught_message(&f_err));
                        }
                        return Ok(out);
                    }
                    let restore = frames.last().unwrap().restore;
                    if stack.len() > restore {
                        stack.truncate(restore);
                    }
                    frames.last_mut().unwrap().err = f_err;
                }
            }
            Opcode::Raise => {
                // throw：er ← 值；defer 走栈后仅传播 er（无返回值）。
                let f = frames.last_mut().unwrap();
                f.err = stack[base + instr.a() as usize];
                if !f.defers.is_empty() {
                    f.walk = Walk::Raise;
                    f.walk_remaining = f.defers.len();
                    f.ip = *f.defers.last().unwrap();
                } else {
                    let f_err = f.err;
                    frames.pop();
                    if frames.is_empty() {
                        return Err(uncaught_message(&f_err));
                    }
                    let restore = frames.last().unwrap().restore;
                    if stack.len() > restore {
                        stack.truncate(restore);
                    }
                    frames.last_mut().unwrap().err = f_err;
                }
            }
            Opcode::RetErr => {
                let f = frames.last_mut().unwrap();
                if f.err != Value::Nil && !f.defers.is_empty() {
                    f.walk = Walk::Raise;
                    f.walk_remaining = f.defers.len();
                    f.ip = *f.defers.last().unwrap();
                } else if f.err != Value::Nil {
                    let f_err = f.err;
                    frames.pop();
                    if frames.is_empty() {
                        return Err(uncaught_message(&f_err));
                    }
                    let restore = frames.last().unwrap().restore;
                    if stack.len() > restore {
                        stack.truncate(restore);
                    }
                    frames.last_mut().unwrap().err = f_err;
                }
            }
            Opcode::JmpErr => {
                let f = frames.last_mut().unwrap();
                if f.err != Value::Nil {
                    stack[base + instr.a() as usize] = f.err;
                    f.err = Value::Nil;
                    f.ip = (ip as i64 + 1 + instr.sbx() as i64) as u32;
                }
            }
            Opcode::Panic => {
                let payload = stack[base + instr.a() as usize];
                if panic_payload.is_some() {
                    return Err("panic during panic".to_string());
                }
                panic_payload = Some(payload);
                let f = frames.last_mut().unwrap();
                if !f.defers.is_empty() {
                    f.walk = Walk::Panic;
                    f.walk_remaining = f.defers.len();
                    f.ip = *f.defers.last().unwrap();
                } else if run_continue_panic(&mut frames, &panic_payload, &mut out) {
                    return Err(panic_message(&panic_payload));
                }
            }
            Opcode::Defer => {
                frames
                    .last_mut()
                    .unwrap()
                    .defers
                    .push(instr.bx() as u32);
            }
            Opcode::EndDefer => {
                // 清理体完成：弹出其记录（未弹出的留在栈上供链式进入）。
                frames.last_mut().unwrap().defers.pop();
                let (walk, remaining, err, ret, ret_slot) = {
                    let f = frames.last().unwrap();
                    (f.walk, f.walk_remaining, f.err, f.ret_val, f.ret_slot)
                };
                match walk {
                    Walk::Resume(target) => {
                        let f = frames.last_mut().unwrap();
                        f.ip = target;
                        f.walk = Walk::None;
                    }
                    Walk::Ret | Walk::Raise => {
                        let remaining = remaining - 1;
                        if remaining > 0 {
                            let f = frames.last_mut().unwrap();
                            let next = *f.defers.last().unwrap();
                            f.ip = next;
                        } else {
                            let is_ret = walk == Walk::Ret;
                            frames.pop();
                            if frames.is_empty() {
                                if err != Value::Nil {
                                    return Err(uncaught_message(&err));
                                }
                                return Ok(out);
                            }
                            let restore = frames.last().unwrap().restore;
                            if stack.len() > restore {
                                stack.truncate(restore);
                            }
                            frames.last_mut().unwrap().err = err;
                            if is_ret {
                                if ret_slot < stack.len() {
                                    stack[ret_slot] = ret;
                                } else {
                                    stack.push(ret);
                                }
                            }
                        }
                    }
                    Walk::Panic => {
                        let remaining = remaining - 1;
                        if remaining > 0 {
                            let f = frames.last_mut().unwrap();
                            let next = *f.defers.last().unwrap();
                            f.ip = next;
                        } else if run_continue_panic(&mut frames, &panic_payload, &mut out) {
                            return Err(panic_message(&panic_payload));
                        }
                    }
                    Walk::None => rt!("internal: ENDDEFER outside a defer walk"),
                }
            }
            Opcode::Mark => {
                let n = frames.last().unwrap().defers.len();
                frames.last_mut().unwrap().marks.push(n);
            }
            Opcode::EndScope => {
                let f = frames.last_mut().unwrap();
                let marker = f.marks.pop().unwrap_or(0);
                let remaining = f.defers.len().saturating_sub(marker);
                if remaining > 0 {
                    f.walk = Walk::Resume(ip as u32 + 1);
                    f.walk_remaining = remaining;
                    f.ip = *f.defers.last().unwrap();
                }
            }
            Opcode::AllocCell => {
                let gc_extra: Vec<Value> = {
                    let mut v: Vec<Value> = frames.iter().flat_map(|f| [f.err, f.ret_val]).collect();
                    if let Some(p) = &panic_payload {
                        v.push(*p);
                    }
                    v
                };
                maybe_gc(&mut heap, &stack, &slots, &frames, &gc_extra);
                let v = stack[base + instr.b() as usize];
                let o = heap.alloc(ObjKind::Cell(v));
                stack[base + instr.a() as usize] = Value::Obj(o);
            }
            Opcode::GetCell => {
                let cell = stack[base + instr.b() as usize];
                match cell {
                    Value::Obj(o) => match o.as_cell() {
                        Some(v) => stack[base + instr.a() as usize] = v,
                        None => rt!("getcell on a non-cell"),
                    },
                    _ => rt!("getcell on a non-cell"),
                }
            }
            Opcode::SetCell => {
                let v = stack[base + instr.a() as usize];
                let cell = stack[base + instr.b() as usize];
                match cell {
                    Value::Obj(o) => match o.kind_mut() {
                        ObjKind::Cell(slot) => *slot = v,
                        _ => rt!("setcell on a non-cell"),
                    },
                    _ => rt!("setcell on a non-cell"),
                }
            }
            Opcode::RefCell => {
                // 引用值 = cell 对象本身（权限在类型层）；此即指针拷贝。
                stack[base + instr.a() as usize] = stack[base + instr.b() as usize];
            }
            Opcode::RefElem => {
                let gc_extra: Vec<Value> = {
                    let mut v: Vec<Value> = frames.iter().flat_map(|f| [f.err, f.ret_val]).collect();
                    if let Some(p) = &panic_payload {
                        v.push(*p);
                    }
                    v
                };
                maybe_gc(&mut heap, &stack, &slots, &frames, &gc_extra);
                let (arr, idx) = (
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                let (o, i) = match (arr, idx) {
                    (Value::Obj(o), Value::Int(i)) => (o, i),
                    _ => rt!("&arr[i] requires array and int index"),
                };
                let len = match o.as_arr() {
                    Some(a) => a.len() as i64,
                    None => rt!("&arr[i] on a non-array"),
                };
                if i < 0 || i >= len {
                    rt!(format!("array index {i} out of bounds (len {len})"));
                }
                let r = heap.alloc(ObjKind::ElemRef { arr: o, idx: i });
                stack[base + instr.a() as usize] = Value::Obj(r);
            }
            Opcode::LoadRef => {
                let r = stack[base + instr.b() as usize];
                stack[base + instr.a() as usize] = match deref_ref(r) {
                    Ok(v) => v,
                    Err(msg) => rt!(msg),
                };
            }
            Opcode::StoreRef => {
                // 只消费 rA 这个 source，rB 所指 place 不失效。
                let v = stack[base + instr.a() as usize];
                let r = stack[base + instr.b() as usize];
                if let Err(msg) = store_ref(r, v) {
                    rt!(msg);
                }
            }
            Opcode::LoadFunc => {
                let o = heap.alloc(ObjKind::Closure {
                    func: instr.bx() as u32,
                    env: Box::new([]),
                });
                stack[base + instr.a() as usize] = Value::Obj(o);
            }
            Opcode::Closure => {
                let gc_extra: Vec<Value> = {
                    let mut v: Vec<Value> = frames.iter().flat_map(|f| [f.err, f.ret_val]).collect();
                    if let Some(p) = &panic_payload {
                        v.push(*p);
                    }
                    v
                };
                maybe_gc(&mut heap, &stack, &slots, &frames, &gc_extra);
                // 捕获描述伪指令紧随其后（CapRef/CapUp 序列），一并消费。
                let frame_env = frames.last().unwrap().env.clone();
                let mut caps = 0usize;
                while matches!(
                    func.code[ip + 1 + caps].op(),
                    Opcode::CapRef | Opcode::CapUp
                ) {
                    caps += 1;
                }
                let mut env = Vec::with_capacity(caps);
                for k in 0..caps {
                    let pseudo = func.code[ip + 1 + k];
                    match pseudo.op() {
                        Opcode::CapRef => {
                            let src = stack[base + pseudo.b() as usize];
                            match src {
                                Value::Obj(_) => env.push(src),
                                _ => rt!("internal: CAPREF source register is not a cell"),
                            }
                        }
                        Opcode::CapUp => {
                            let idx = pseudo.b() as usize;
                            match frame_env.get(idx) {
                                Some(v) => env.push(*v),
                                None => rt!("internal: CAPUP index out of range"),
                            }
                        }
                        _ => rt!("internal: capture descriptor expected"),
                    }
                }
                let o = heap.alloc(ObjKind::Closure {
                    func: instr.bx() as u32,
                    env: env.into_boxed_slice(),
                });
                stack[base + instr.a() as usize] = Value::Obj(o);
                frames.last_mut().unwrap().ip = (ip + 1 + caps) as u32;
            }
            Opcode::CapRef | Opcode::CapUp => {
                rt!("internal: capture descriptor outside CLOSURE")
            }
            Opcode::GetUpval => {
                let frame_env = frames.last().unwrap().env.clone();
                let idx = instr.b() as usize;
                let cell = match frame_env.get(idx) {
                    Some(v) => *v,
                    None => rt!("internal: upval index out of range"),
                };
                match cell {
                    Value::Obj(o) => match o.as_cell() {
                        Some(v) => stack[base + instr.a() as usize] = v,
                        None => rt!("internal: upval slot is not a cell"),
                    },
                    _ => rt!("internal: upval slot is not a cell"),
                }
            }
            Opcode::SetUpval => {
                let frame_env = frames.last().unwrap().env.clone();
                let idx = instr.b() as usize;
                let cell = match frame_env.get(idx) {
                    Some(v) => *v,
                    None => rt!("internal: upval index out of range"),
                };
                match cell {
                    Value::Obj(o) => match o.kind_mut() {
                        ObjKind::Cell(slot) => *slot = stack[base + instr.a() as usize],
                        _ => rt!("internal: upval slot is not a cell"),
                    },
                    _ => rt!("internal: upval slot is not a cell"),
                }
            }
            Opcode::Upval => {
                let frame_env = frames.last().unwrap().env.clone();
                let idx = instr.b() as usize;
                stack[base + instr.a() as usize] = match frame_env.get(idx) {
                    Some(v) => *v,
                    None => rt!("internal: upval index out of range"),
                };
            }
            Opcode::Call => {
                // 间接调用：被调值 rA（闭包），实参 rA+1..rA+C；被调帧基址 =
                // base+A+1，返回值经被调 r0 落回调用帧的 A+1 槽。
                let callee = stack[base + instr.a() as usize];
                let (fidx, env) = match callee {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Closure { func, env } => (*func, env.to_vec()),
                        _ => rt!("call of a non-function value"),
                    },
                    _ => rt!("call of a non-function value"),
                };
                let callee_proto = &module.funcs[fidx as usize];
                let base_new = base + instr.a() as usize + 1;
                let need = base_new + callee_proto.regs as usize;
                if stack.len() < need {
                    stack.resize(need, Value::Nil);
                }
                frames.push(Frame::new(fidx, 0, base_new as u32, need, env));
            }
        }
    }
}

/// 解引用读：cell / 元素投影。投影的越界在访问时报错（与普通下标同条件）。
fn deref_ref(r: Value) -> Result<Value, String> {
    match r {
        Value::Obj(o) => match o.kind() {
            ObjKind::Cell(v) => Ok(*v),
            ObjKind::ElemRef { arr, idx } => match arr.as_arr() {
                Some(a) => {
                    if *idx < 0 || *idx as usize >= a.len() {
                        Err(format!(
                            "array index {idx} out of bounds (len {})",
                            a.len()
                        ))
                    } else {
                        Ok(a[*idx as usize])
                    }
                }
                None => Err("dereference of a non-array element reference".to_string()),
            },
            _ => Err("dereference of a non-reference value".to_string()),
        },
        _ => Err("dereference of a non-reference value".to_string()),
    }
}

/// 经引用写：只更新目标 place，引用本身不失效。
fn store_ref(r: Value, v: Value) -> Result<(), String> {
    match r {
        Value::Obj(o) => match o.kind() {
            ObjKind::Cell(_) => {
                match o.kind_mut() {
                    ObjKind::Cell(slot) => *slot = v,
                    _ => unreachable!("kind checked above"),
                }
                Ok(())
            }
            ObjKind::ElemRef { arr, idx } => match arr.as_arr_mut() {
                Some(a) => {
                    if *idx < 0 || *idx as usize >= a.len() {
                        Err(format!(
                            "array index {idx} out of bounds (len {})",
                            a.len()
                        ))
                    } else {
                        a[*idx as usize] = v;
                        Ok(())
                    }
                }
                None => Err("store through a non-array element reference".to_string()),
            },
            _ => Err("store through a non-reference value".to_string()),
        },
        _ => Err("store through a non-reference value".to_string()),
    }
}

/// PANIC 展开的「弹到下一个带 defer 的帧」步进。返回 true = 已无帧（进程失败）。
fn run_continue_panic(frames: &mut Vec<Frame>, payload: &Option<Value>, out: &mut String) -> bool {
    loop {
        frames.pop();
        match frames.last_mut() {
            None => return true,
            Some(c) if !c.defers.is_empty() => {
                c.walk = Walk::Panic;
                c.walk_remaining = c.defers.len();
                c.ip = *c.defers.last().unwrap();
                let _ = out;
                return false;
            }
            Some(_) => {}
        }
    }
}

fn panic_message(payload: &Option<Value>) -> String {
    match payload {
        Some(p) => {
            let mut s = String::new();
            let _ = write_value(p, &mut s);
            format!("panic: {s}")
        }
        None => "panic".to_string(),
    }
}

fn uncaught_message(err: &Value) -> String {
    let mut s = String::new();
    let _ = write_value(err, &mut s);
    format!("uncaught throw: {s}")
}

fn rt_err(name: &str, source: &str, pos: u32, msg: impl std::fmt::Display) -> String {
    format!(
        "runtime error: {msg} at {name} (line {})",
        line_of(source, pos)
    )
}

fn line_of(source: &str, pos: u32) -> u32 {
    source
        .as_bytes()
        .iter()
        .take((pos as usize).min(source.len()))
        .filter(|&&b| b == b'\n')
        .count() as u32
        + 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::Instr;
    use crate::lower::{FuncProto, Module};
    use xvalue::Heap;

    #[test]
    fn vm_runs_hand_built_module() {
        // print(2 + 3)
        let top = FuncProto {
            name: "<top>".to_string(),
            arity: 0,
            regs: 2,
            upvals: 0,
            code: vec![
                Instr::asbx(Opcode::LoadI, 0, 2),
                Instr::asbx(Opcode::LoadI, 1, 3),
                Instr::abc(Opcode::AddI64, 0, 0, 1),
                Instr::abc(Opcode::CallNat, 0, 0, 1),
                Instr::abc(Opcode::Ret, 0, 0, 0),
            ],
            lines: vec![0; 5],
        };
        let m = Module {
            funcs: vec![top],
            consts: vec![],
            heap: Heap::new(),
            num_slots: 0,
            cell_slots: vec![],
            natives: vec!["print"],
        };
        assert_eq!(run(&m, "2 + 3").unwrap(), "5\n");
    }
}
