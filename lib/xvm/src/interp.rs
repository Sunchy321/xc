//! 解释器：域 × fiber 调度器（M4）。每任务域 = 独立堆 + 槽位快照 + fiber；
//! spawn 开新域（无共享内存，捕获/结果经深拷贝转移）；await 挂起当前 fiber，
//! 就绪队列 FIFO 确定性交错。错误消息与旧原型逐字兼容（golden 为基线）。
//!
//! 无类型检查器阶段的实现暂态：算术恒发 I64 族——`+` 仅整数（字符串拼接是
//! `~` → CONCAT，`+` 对 string 无定义）；`EQ/NE` 为单条动态指令。类型检查器
//! 落地后按静态类型分家。

use xvalue::{Heap, ObjKind, ObjRef, TaskState, Value, write_value};

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

#[derive(Clone, Copy, PartialEq, Debug)]
enum FiberState {
    Ready,
    Running,
    Waiting,
    Done,
}

struct Fiber {
    id: usize,
    domain: usize,
    stack: Vec<Value>,
    frames: Vec<Frame>,
    state: FiberState,
    /// panic 展开时的载荷（本域堆的值）。
    panic_payload: Option<Value>,
}

impl Fiber {
    fn new(id: usize, domain: usize, func: u32, regs: usize, env: Vec<Value>) -> Self {
        Fiber {
            id,
            domain,
            stack: vec![Value::Nil; regs],
            frames: vec![Frame::new(func, 0, 0, regs, env)],
            state: FiberState::Ready,
            panic_payload: None,
        }
    }
}

/// 任务域：独立堆 + 模块槽位快照 + 该域的全部 fiber。域间无共享内存。
struct Domain {
    heap: Heap,
    slots: Vec<Value>,
    /// 本域作为 spawn 子任务时：任务对象所在的 (owner 域, 对象)。
    spawned_as: Option<(usize, ObjRef)>,
}

impl Domain {
    fn new(gc_stress: bool, module: &Module) -> Self {
        let mut heap = Heap::new();
        heap.set_gc_stress(gc_stress);
        let mut slots = vec![Value::Void; module.num_slots];
        // cell 化槽位：加载期建 cell，此后 GETSLOT/SETSLOT 透明穿透。
        for (idx, marked) in module.cell_slots.iter().enumerate() {
            if *marked {
                let o = heap.alloc(ObjKind::Cell(Value::Void));
                slots[idx] = Value::Obj(o);
            }
        }
        Domain {
            heap,
            slots,
            spawned_as: None,
        }
    }
}

struct Waiting {
    fiber: usize,
    task_domain: usize,
    task: ObjRef,
    slot: usize,
}

enum FiberExit {
    /// 根帧返回（值在本域堆）。
    Finished(Value),
    /// AWAIT 挂起：等待的任务（所属域, 对象）与结果落点槽（本 fiber 栈的绝对下标）。
    Parked {
        task_domain: usize,
        task: ObjRef,
        slot: usize,
    },
    /// panic 传出到任务边界（payload 在本域堆）。
    Panicked(Value),
    /// spawn 请求：调度器建子域/任务对象，并把 spawner 置回队首继续。
    SpawnRequested {
        closure: Value,
        func: u32,
        env: Vec<Value>,
        reg: u32,
    },
    /// 运行期错误（进程级，确定性终止）。
    Error(String),
}

pub fn run(module: &Module, source: &str) -> Result<String, String> {
    run_value(module, source).map(|(out, _)| out)
}

/// 同 [`run`]，另返回根 fiber 的最终值（顶层最后一条语句的值；
/// REPL 值回显用）。`let` 语句的值为 Nil。
pub fn run_value(module: &Module, source: &str) -> Result<(String, Value), String> {
    let gc_stress = std::env::var("XVM_GC_EVERY_ALLOC").is_ok();
    let mut domains: Vec<Domain> = vec![Domain::new(gc_stress, module)];
    let top_regs = module.funcs[0].regs as usize;
    let mut fibers: Vec<Fiber> = vec![Fiber::new(0, 0, 0, top_regs, Vec::new())];
    let mut ready: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    ready.push_back(0);
    let mut waiting: Vec<Waiting> = Vec::new();
    let mut out = String::new();
    let mut root_value: Value = Value::Void;

    // 任务完成交付：结果深拷贝入 owner 堆、置状态、唤醒等待者。
    fn deliver(
        domains: &mut Vec<Domain>,
        fibers: &mut Vec<Fiber>,
        ready: &mut std::collections::VecDeque<usize>,
        waiting: &mut Vec<Waiting>,
        owner: usize,
        tref: ObjRef,
        root: Value,
        failed: bool,
    ) {
        let result = domains[owner].heap.copy_value(root);
        if let Some(ts) = tref.task_state_mut() {
            ts.done = true;
            ts.failed = failed;
            ts.result = result;
        }
        let mut i = 0;
        while i < waiting.len() {
            if waiting[i].task == tref {
                let w = waiting.remove(i);
                let wdomain = fibers[w.fiber].domain;
                let delivered = domains[wdomain].heap.copy_value(result);
                let wf = &mut fibers[w.fiber];
                if failed {
                    let tf = domains[wdomain]
                        .heap
                        .alloc(ObjKind::TaskFailure { payload: delivered });
                    wf.frames.last_mut().unwrap().err = Value::Obj(tf);
                } else {
                    wf.stack[w.slot] = delivered;
                }
                wf.state = FiberState::Ready;
                ready.push_back(w.fiber);
            } else {
                i += 1;
            }
        }
    }

    // 调度主循环：FIFO 就绪队列——同域确定性交错、跨域确定性排队。
    loop {
        let Some(fid) = ready.pop_front() else {
            if waiting.is_empty() {
                return Ok((out, root_value));
            }
            return Err("deadlock: all fibers are waiting".to_string());
        };
        let d = fibers[fid].domain;
        fibers[fid].state = FiberState::Running;
        // 同域兄弟 fiber 作为 GC 根（借用按 split 分离）。
        let (fiber, siblings) = {
            let (head, tail) = fibers.split_at_mut(fid);
            let (f, rest) = tail.split_first_mut().unwrap();
            let sib: Vec<&mut Fiber> = head
                .iter_mut()
                .chain(rest.iter_mut())
                .filter(|fr| fr.domain == d)
                .collect();
            (f, sib)
        };
        let domain = &mut domains[d];
        let sib_refs: Vec<&Fiber> = siblings.iter().map(|f| &**f).collect();
        let exit = exec_fiber(fiber, domain, &sib_refs, module, source, &mut out);

        match exit {
            FiberExit::Finished(root) => {
                fibers[fid].state = FiberState::Done;
                if fid == 0 && d == 0 {
                    root_value = root;
                }
                if let Some((owner, tref)) = domain.spawned_as {
                    deliver(
                        &mut domains,
                        &mut fibers,
                        &mut ready,
                        &mut waiting,
                        owner,
                        tref,
                        root,
                        false,
                    );
                }
            }
            FiberExit::Panicked(payload) => {
                fibers[fid].state = FiberState::Done;
                if d == 0 {
                    return Err(panic_message(&Some(payload)));
                }
                if let Some((owner, tref)) = domain.spawned_as {
                    deliver(
                        &mut domains,
                        &mut fibers,
                        &mut ready,
                        &mut waiting,
                        owner,
                        tref,
                        payload,
                        true,
                    );
                } else {
                    return Err(panic_message(&Some(payload)));
                }
            }
            FiberExit::Parked {
                task_domain,
                task,
                slot,
            } => {
                fibers[fid].state = FiberState::Waiting;
                waiting.push(Waiting {
                    fiber: fid,
                    task_domain,
                    task,
                    slot,
                });
            }
            FiberExit::SpawnRequested {
                closure,
                func,
                env,
                reg,
            } => {
                let parent_d = fibers[fid].domain;
                let child_d = domains.len();
                // 任务对象先在父域堆分配（此时 child_d 已知）。
                let task = domains[parent_d].heap.alloc(ObjKind::Task(TaskState {
                    domain: child_d,
                    done: false,
                    failed: false,
                    result: Value::Nil,
                }));
                // 子域：堆 + 槽位快照 + 闭包体深拷贝（env 的 cell → 子域私有）。
                let mut child = Domain::new(gc_stress, module);
                child.spawned_as = Some((parent_d, task));
                child.slots = domains[parent_d]
                    .slots
                    .iter()
                    .map(|v| child.heap.copy_value(*v))
                    .collect();
                let env2: Vec<Value> = env.iter().map(|v| child.heap.copy_value(*v)).collect();
                let regs = module.funcs[func as usize].regs as usize;
                domains.push(child);
                let child_fid = fibers.len();
                fibers.push(Fiber::new(child_fid, child_d, func, regs, env2));
                // spawner 立即继续（队首），子任务排队——确定性 FIFO。
                let slot = fibers[fid].frames.last().unwrap().base as usize + reg as usize;
                fibers[fid].stack[slot] = Value::Obj(task);
                fibers[fid].state = FiberState::Ready;
                ready.push_front(fid);
                ready.push_back(child_fid);
            }
            FiberExit::Error(msg) => return Err(msg),
        }

        // 唤醒已完成任务的等待者（交付 = 深拷贝入等待者域 + 置值/置 er）。
        let mut i = 0;
        while i < waiting.len() {
            let w = &waiting[i];
            let done_failed = w
                .task
                .as_task()
                .map(|ts| (ts.done, ts.failed, ts.result))
                .filter(|(done, ..)| *done);
            if let Some((_, failed, result)) = done_failed {
                let w = waiting.remove(i);
                let wdomain = fibers[w.fiber].domain;
                let delivered = domains[wdomain].heap.copy_value(result);
                let wf = &mut fibers[w.fiber];
                if failed {
                    // 失败在边界类型化：TaskFailure（waiter 域）进 er。
                    let tf = dw_alloc_taskfailure(&mut domains[wdomain].heap, delivered);
                    wf.frames.last_mut().unwrap().err = Value::Obj(tf);
                } else {
                    wf.stack[w.slot] = delivered;
                }
                wf.state = FiberState::Ready;
                ready.push_back(w.fiber);
            } else {
                i += 1;
            }
        }
    }
}

fn dw_alloc_taskfailure(heap: &mut Heap, payload: Value) -> ObjRef {
    heap.alloc(ObjKind::TaskFailure { payload })
}

// 调度器内取 fiber 所属域（可读性辅助）。
fn fid_domain(fibers: &[Fiber], fid: usize) -> usize {
    fibers[fid].domain
}

/// 执行一个 fiber 直至完成/挂起/panic 出界/出错。
/// `siblings` = 同域其他 fiber（GC 根，冻结中）。
fn exec_fiber(
    fiber: &mut Fiber,
    domain: &mut Domain,
    siblings: &[&Fiber],
    module: &Module,
    source: &str,
    out: &mut String,
) -> FiberExit {
    let Domain { heap, slots, .. } = domain;
    let stack = &mut fiber.stack;
    let frames = &mut fiber.frames;
    let fiber_panic = &mut fiber.panic_payload;

    // 分配 safepoint：栈、槽表、各帧捕获环境（含同域兄弟 fiber）都是精确根集。
    fn maybe_gc(
        heap: &mut Heap,
        stack: &[Value],
        frames: &[Frame],
        panic_payload: Option<Value>,
        slots: &[Value],
        siblings: &[&Fiber],
    ) {
        if !heap.should_collect() {
            return;
        }
        let mut root_vecs: Vec<Vec<Value>> = vec![stack.to_vec(), slots.to_vec()];
        for fr in frames {
            root_vecs.push(fr.env.clone());
            root_vecs.push(vec![fr.err, fr.ret_val]);
        }
        if let Some(p) = panic_payload {
            root_vecs.push(vec![p]);
        }
        for s in siblings {
            root_vecs.push(s.stack.clone());
            for fr in &s.frames {
                root_vecs.push(fr.env.clone());
                root_vecs.push(vec![fr.err, fr.ret_val]);
            }
        }
        let slices: Vec<&[Value]> = root_vecs.iter().map(|v| v.as_slice()).collect();
        let (freed, kept) = heap.collect(&slices);
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
                return FiberExit::Error(rt_err(&func.name, source, pos, $msg))
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
            Opcode::AddI64 | Opcode::SubI64 | Opcode::MulI64 | Opcode::DivI64 | Opcode::ModI64 => {
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
                    (Value::Int(a), Value::Int(b)) => Value::Int(match arith(a, b) {
                        Ok(v) => v,
                        Err(m) => rt!(m),
                    }),
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
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
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
                    (Value::Int(a), Value::Int(b)) => Value::Bool(if instr.op() == Opcode::Lt {
                        a < b
                    } else {
                        a <= b
                    }),
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
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
                let (dst, base_i, count) =
                    (instr.a() as usize, instr.b() as usize, instr.c() as usize);
                let elems = stack[base + base_i..base + base_i + count].to_vec();
                let o = heap.alloc(ObjKind::Arr(elems));
                stack[base + dst] = Value::Obj(o);
            }
            Opcode::NewDict => {
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
                let (dst, base_i, pairs) =
                    (instr.a() as usize, instr.b() as usize, instr.c() as usize);
                let mut d: Vec<(Value, Value)> = Vec::with_capacity(pairs);
                for k in 0..pairs {
                    let key = stack[base + base_i + k * 2];
                    let val = stack[base + base_i + k * 2 + 1];
                    // 字面量重复键后写覆盖（NEWDICT 统一语义；字面量常量键查重
                    // 是前端职责，随类型检查器批次）。
                    if let Some(slot) = d.iter_mut().find(|(k2, _)| *k2 == key) {
                        slot.1 = val;
                    } else {
                        d.push((key, val));
                    }
                }
                let o = heap.alloc(ObjKind::Dict(d));
                stack[base + dst] = Value::Obj(o);
            }
            Opcode::Len => {
                let dst = base + instr.a() as usize;
                match stack[base + instr.b() as usize] {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Arr(a) => stack[dst] = Value::Int(a.len() as i64),
                        ObjKind::Dict(d) => stack[dst] = Value::Int(d.len() as i64),
                        _ => rt!("length of a non-container"),
                    },
                    _ => rt!("length of a non-container"),
                }
            }
            Opcode::GetElem => {
                let (dst, container, idx) = (
                    base + instr.a() as usize,
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                match container {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Arr(a) => {
                            let i = match idx {
                                Value::Int(i) => i,
                                _ => rt!("array index must be int"),
                            };
                            if i < 0 || i as usize >= a.len() {
                                rt!(format!("array index {i} out of bounds (len {})", a.len()));
                            }
                            stack[dst] = a[i as usize];
                        }
                        ObjKind::Dict(d) => match d.iter().find(|(k, _)| *k == idx) {
                            Some((_, v)) => stack[dst] = *v,
                            None => {
                                let mut key = String::new();
                                let _ = write_value(&idx, &mut key);
                                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
                                let payload = Value::Obj(
                                    heap.alloc(ObjKind::Str(format!("missing key: {key}").into())),
                                );
                                if let Some(exit) = begin_panic(frames, stack, fiber_panic, payload)
                                {
                                    return exit;
                                }
                            }
                        },
                        _ => rt!("subscript on a non-container"),
                    },
                    _ => rt!("subscript on a non-container"),
                }
            }
            Opcode::SetElem => {
                let (v, container, idx) = (
                    stack[base + instr.a() as usize],
                    stack[base + instr.b() as usize],
                    stack[base + instr.c() as usize],
                );
                match container {
                    Value::Obj(o) => match o.kind_mut() {
                        ObjKind::Arr(a) => {
                            let i = match idx {
                                Value::Int(i) => i,
                                _ => rt!("array index must be int"),
                            };
                            if i < 0 || i as usize >= a.len() {
                                rt!(format!("array index {i} out of bounds (len {})", a.len()));
                            }
                            a[i as usize] = v;
                        }
                        ObjKind::Dict(d) => {
                            if let Some(slot) = d.iter_mut().find(|(k, _)| *k == idx) {
                                slot.1 = v;
                            } else {
                                d.push((idx, v));
                            }
                        }
                        _ => rt!("subscript on a non-container"),
                    },
                    _ => rt!("subscript on a non-container"),
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
                            let ok = write_value(v, out).is_ok();
                            if !ok {
                                rt!("print failed");
                            }
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
                            return FiberExit::Error(uncaught_message(&f_err));
                        }
                        return FiberExit::Finished(ret);
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
                        return FiberExit::Error(uncaught_message(&f_err));
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
                        return FiberExit::Error(uncaught_message(&f_err));
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
                if let Some(exit) = begin_panic(frames, stack, fiber_panic, payload) {
                    return exit;
                }
            }
            Opcode::Defer => {
                frames.last_mut().unwrap().defers.push(instr.bx() as u32);
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
                                    return FiberExit::Error(uncaught_message(&err));
                                }
                                return FiberExit::Finished(ret);
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
                        frames.last_mut().unwrap().walk_remaining = remaining;
                        if remaining > 0 {
                            let f = frames.last_mut().unwrap();
                            let next = *f.defers.last().unwrap();
                            f.ip = next;
                        } else if run_continue_panic(frames, stack) {
                            return FiberExit::Panicked((*fiber_panic).unwrap_or(Value::Nil));
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
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
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
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
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
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
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
            Opcode::Spawn => {
                maybe_gc(heap, stack, frames, *fiber_panic, slots, siblings);
                let closure = stack[base + instr.a() as usize];
                let func = match &closure {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Closure { func, .. } => *func,
                        _ => rt!("spawn of a non-closure value"),
                    },
                    _ => rt!("spawn of a non-closure value"),
                };
                let env = match &closure {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Closure { env, .. } => env.to_vec(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                return FiberExit::SpawnRequested {
                    closure,
                    func,
                    env,
                    reg: instr.a(),
                };
            }
            Opcode::Await => {
                let v = stack[base + instr.a() as usize];
                let (tdomain, tref, done, failed, result) = match v {
                    Value::Obj(o) => match o.kind() {
                        ObjKind::Task(ts) => (ts.domain, o, ts.done, ts.failed, ts.result),
                        _ => rt!("await on a non-task value"),
                    },
                    _ => rt!("await on a non-task value"),
                };
                if done {
                    // Task 对象在 spawner 域 = 本域：结果直取。失败经边界类型化
                    // 的 TaskFailure 进 er（后续 RET 传播 / try-catch 捕获）。
                    stack[base + instr.a() as usize] = result;
                    if failed {
                        frames.last_mut().unwrap().err = result;
                    }
                } else {
                    return FiberExit::Parked {
                        task_domain: tdomain,
                        task: tref,
                        slot: base + instr.a() as usize,
                    };
                }
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
                        Err(format!("array index {idx} out of bounds (len {})", a.len()))
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
                        Err(format!("array index {idx} out of bounds (len {})", a.len()))
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
fn begin_panic(
    frames: &mut Vec<Frame>,
    stack: &mut Vec<Value>,
    fiber_panic: &mut Option<Value>,
    payload: Value,
) -> Option<FiberExit> {
    if fiber_panic.is_some() {
        return Some(FiberExit::Error("panic during panic".to_string()));
    }

    *fiber_panic = Some(payload);
    if frames
        .last()
        .expect("panic requires an executing frame")
        .defers
        .is_empty()
    {
        if run_continue_panic(frames, stack) {
            Some(FiberExit::Panicked(payload))
        } else {
            None
        }
    } else {
        let frame = frames.last_mut().unwrap();
        frame.walk = Walk::Panic;
        frame.walk_remaining = frame.defers.len();
        frame.ip = *frame.defers.last().unwrap();
        None
    }
}

fn run_continue_panic(frames: &mut Vec<Frame>, stack: &mut Vec<Value>) -> bool {
    loop {
        frames.pop();
        match frames.last_mut() {
            None => return true,
            Some(c) if !c.defers.is_empty() => {
                if stack.len() > c.restore {
                    stack.truncate(c.restore);
                }
                c.walk = Walk::Panic;
                c.walk_remaining = c.defers.len();
                c.ip = *c.defers.last().unwrap();
                return false;
            }
            Some(c) => {
                if stack.len() > c.restore {
                    stack.truncate(c.restore);
                }
            }
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
