//! 从 `xc_check::Program`（名字解析后的 AST）降级为寄存器字节码。
//!
//! 不变量：字节码只从 check 通过的程序生成。M0/M1 临时态：xc_check 尚无类型
//! 信息，算术按 I64 族发射（`+` 仅整数；字符串拼接是 `~` → `CONCAT`，`+` 对
//! string 无定义）；类型检查器落地后收紧为纯静态发射。
//!
//! 寄存器分配：局部变量在声明处获得固定寄存器（高水位计入帧大小），表达式
//! 临时值在语句边界回收（`next_reg` 重置回 `temp_base`）。
//!
//! M1（cell 与闭包）：
//! - 被取引用（`&x` / `&mut x`）或被闭包捕获的局部在声明处升格为 cell
//!   （`ALLOCCELL`），此后读写走 `GETCELL` / `SETCELL`——place 稳定性与
//!   「赋值可见」由 cell 兑现。升格判据来自函数级预扫描（超近似：多升格无害）。
//! - 闭包（`do (params) { body }`）降级为独立 proto + `CLOSURE`；捕获**全部按
//!   引用**（env 槽恒为 cell），来源惰性解析：创建处寄存器 → `CAPREF`；外层
//!   lambda 的 upval → `CAPUP`。模块槽与函数名不捕获（直接寻址）。
//! - 已知局限（M1 记录）：lambda 体中 `let x = …x…` 重声明引用外部同名变量的
//!   场景、以及引用「声明晚于 lambda 创建」的变量，均按 undefined 处理或不保证
//!   cell 语义；移动捕获升级与借用检查属后续里程碑。

use std::collections::HashMap;
use std::collections::HashSet;

use xc_ast::expr::{Argument, Expr, ExprItem, ExprKind};
use xc_ast::func::{Func, FuncBody};
use xc_ast::pattern::PatternKind;
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_ast::Mutability;
use xc_check::Program;
use xc_span::symbol::op;
use xc_span::Symbol;
use xvalue::{lit_scalar, Heap, ObjKind, Value};

use crate::isa::Instr;
use crate::isa::Opcode;

pub struct FuncProto {
    pub name: String,
    pub arity: u32,
    pub regs: u32,
    pub upvals: u32,
    pub code: Vec<Instr>,
    /// 与 code 等长；元素为源码字节偏移，运行期换算行号。
    pub lines: Vec<u32>,
}

pub struct Module {
    pub funcs: Vec<FuncProto>,
    pub consts: Vec<Value>,
    /// 常量堆对象（字符串字面量）；与运行堆分离，随 Module 存活。
    pub heap: Heap,
    pub num_slots: usize,
    /// cell 化槽位：被 `&` 取址的模块级绑定。加载期建 cell，GETSLOT/SETSLOT
    /// 透明穿透，`&slot` 取 cell 本体（赋值可见语义对全局同样成立）。
    pub cell_slots: Vec<bool>,
    pub natives: Vec<&'static str>,
}

pub fn compile(program: &Program) -> Result<Module, String> {
    let mut lw = Lower {
        func_indices: &program.func_indices,
        slots: Vec::new(),
        seq: 0,
        funcs: Vec::new(),
        consts: Vec::new(),
        heap: Heap::new(),
        lambda_seq: 0,
        pending: Vec::new(),
        lambda_base: 1 + program.funcs.len(),
        amp_targets: HashSet::new(),
    };

    // 顶层先降级：槽表随之完整，之后函数体引用全局无需顺序检查。
    let promoted = scan_promoted_stmts(&program.top);
    let mut st = FnState::new(promoted, None);
    lw.compile_stmts(&mut st, &program.top, true)?;
    lw.emit(&mut st, Instr::abc(Opcode::Ret, 0, 0, 0), 0);
    lw.funcs.push(st.finish("<top>".to_string(), 0));

    for f in &program.funcs {
        lw.lower_func(f)?;
    }

    lw.funcs.extend(lw.pending.drain(..));
    let cell_names = lw.amp_targets.clone();
    let cell_slots = (0..lw.slots.len())
        .map(|i| cell_names.contains(&lw.slots[i].0))
        .collect();
    Ok(Module {
        num_slots: lw.slots.len(),
        funcs: lw.funcs,
        consts: lw.consts,
        heap: lw.heap,
        cell_slots,
        natives: vec!["print"],
    })
}

struct LoopInfo {
    breaks: Vec<usize>,
    continue_to: usize,
}

/// 捕获来源：创建处帧的寄存器（必为 cell），或外层 lambda 的 upval 槽。
#[derive(Clone, Copy, Debug)]
enum CapSrc {
    Reg(u32),
    ParentUpval(u32),
}

/// 创建子 lambda 时对外层解析状态的快照（拍平整个外层链）。
#[derive(Clone)]
struct CaptureCtx {
    locals: Vec<(Symbol, LocalInfo)>,
    upvals: Vec<(Symbol, UpvalInfo)>,
}

/// 名字解析结果。
enum Resolved {
    /// 本 lambda/函数的局部；cell = 已升格（寄存器持 cell 而非值）。
    Local(LocalInfo),
    Upval(u32),
    Slot(u32),
}

/// 局部绑定信息。`is_mut`：绑定可再赋值（`let mut`）；`ref_mut`：绑定持有
/// 可变引用（初始化为 `&mut …`，允许 `*r = v`）；`cell`：已升格（寄存器持
/// cell 而非值）。
#[derive(Clone, Copy)]
struct LocalInfo {
    reg: u32,
    is_mut: bool,
    ref_mut: bool,
    cell: bool,
}

/// upval 槽：捕获来源 + 沿捕获链继承的权限位。
#[derive(Clone, Copy, Debug)]
struct UpvalInfo {
    src: CapSrc,
    is_mut: bool,
    ref_mut: bool,
}

struct FnState {
    code: Vec<Instr>,
    lines: Vec<u32>,
    /// (名字, 绑定信息)；同名再声明后写覆盖（旧槽随之死亡，语义为遮蔽）。
    locals: Vec<(Symbol, LocalInfo)>,
    /// (名字, 捕获信息)，下标即 upval 槽号；惰性追加，CLOSURE 发射前定稿。
    upvals: Vec<(Symbol, UpvalInfo)>,
    parent: Option<CaptureCtx>,
    /// 函数级升格名单（预扫描产物；按名共享给嵌套 lambda，多升格无害）。
    promoted: HashSet<Symbol>,
    next_reg: u32,
    /// 已分配局部的最高水位；临时寄存器在语句边界重置回此处。
    temp_base: u32,
    /// 帧大小 = 寄存器号的历史高水位（next_reg 会被语句边界重置，不能用它）。
    regs_watermark: u32,
    loop_stack: Vec<LoopInfo>,
    /// 当前顶层语句序号；函数体为 u32::MAX（全局引用不做 use-before-let 检查）。
    cur_seq: u32,
}

impl FnState {
    fn new(promoted: HashSet<Symbol>, parent: Option<CaptureCtx>) -> Self {
        FnState {
            code: Vec::new(),
            lines: Vec::new(),
            locals: Vec::new(),
            upvals: Vec::new(),
            parent,
            promoted,
            next_reg: 0,
            temp_base: 0,
            regs_watermark: 0,
            loop_stack: Vec::new(),
            cur_seq: u32::MAX,
        }
    }

    fn finish(self, name: String, arity: u32) -> FuncProto {
        let upvals = self.upvals.len() as u32;
        FuncProto {
            name,
            arity,
            regs: self.regs_watermark.max(1),
            upvals,
            code: self.code,
            lines: self.lines,
        }
    }
}

struct Lower<'p> {
    func_indices: &'p HashMap<Symbol, u32>,
    /// 顶层槽表：(名字, 槽号, 声明序, is_mut, ref_mut)。
    slots: Vec<(Symbol, u32, u32, bool, bool)>,
    seq: u32,
    funcs: Vec<FuncProto>,
    consts: Vec<Value>,
    heap: Heap,
    lambda_seq: u32,
    /// lambda proto 延后追加：模块函数表布局 = [top, 声明函数…, lambda…]，
    /// 与 xc_check 的 func_indices 编号（声明序 1..n）保持一致。
    pending: Vec<FuncProto>,
    /// pending 区在最终函数表中的起始下标 = 1 + 声明函数数。
    lambda_base: usize,
    /// 全程序 `&` 目标名（含槽位名）；决定哪些槽位 cell 化。
    amp_targets: HashSet<Symbol>,
}

impl<'p> Lower<'p> {
    // ---- 基础设施 ----

    fn emit(&mut self, st: &mut FnState, instr: Instr, pos: u32) {
        st.code.push(instr);
        st.lines.push(pos);
    }

    fn temp(&mut self, st: &mut FnState) -> Result<u32, String> {
        if st.next_reg >= 255 {
            return Err("function uses too many registers".to_string());
        }
        let r = st.next_reg;
        st.next_reg += 1;
        st.regs_watermark = st.regs_watermark.max(st.next_reg);
        Ok(r)
    }

    /// 局部名落位：返回寄存器；升格名在此只分配寄存器，cell 化由调用方发射
    /// （参数与 let 的初值来源不同）。
    fn bind_local(
        &mut self,
        st: &mut FnState,
        name: Symbol,
        is_mut: bool,
        ref_mut: bool,
    ) -> Result<u32, String> {
        let r = self.temp(st)?;
        st.temp_base = st.temp_base.max(st.next_reg);
        let info = LocalInfo {
            reg: r,
            is_mut,
            ref_mut,
            cell: st.promoted.contains(&name),
        };
        if let Some(entry) = st.locals.iter_mut().rev().find(|(n, _)| *n == name) {
            entry.1 = info;
        } else {
            st.locals.push((name, info));
        }
        Ok(r)
    }

    /// 确保 r0..=r 已纳入帧高水位（调用窗口等「成组写入」的目标寄存器
    /// 不经 temp() 分配，需显式入账，否则 RET 截栈会截掉结果槽）。
    fn ensure_reg(&mut self, st: &mut FnState, r: u32) -> Result<(), String> {
        while st.next_reg <= r {
            self.temp(st)?;
        }
        Ok(())
    }

    fn konst(&mut self, v: Value) -> Result<u16, String> {
        if self.consts.len() >= 65536 {
            return Err("too many constants in module".to_string());
        }
        self.consts.push(v);
        Ok((self.consts.len() - 1) as u16)
    }

    fn slot_of(&self, name: Symbol) -> Option<(u32, u32, bool, bool)> {
        self.slots
            .iter()
            .find(|(n, ..)| *n == name)
            .map(|(_, s, seq, m, rm)| (*s, *seq, *m, *rm))
    }

    fn slot_declare(&mut self, name: Symbol, is_mut: bool, ref_mut: bool) -> u32 {
        if let Some(entry) = self.slots.iter_mut().find(|(n, ..)| *n == name) {
            entry.2 = self.seq;
            entry.3 = is_mut;
            entry.4 = ref_mut;
            return entry.1;
        }
        let idx = self.slots.len() as u32;
        self.slots.push((name, idx, self.seq, is_mut, ref_mut));
        idx
    }

    fn slot_ref_mut(&self, idx: u32) -> bool {
        self.slots
            .get(idx as usize)
            .map(|(_, _, _, _, rm)| *rm)
            .unwrap_or(false)
    }

    fn patch(&mut self, st: &mut FnState, at: usize) {
        let old = st.code[at];
        let off = st.code.len() as i64 - (at as i64 + 1);
        st.code[at] = jump_instr(old, off);
    }

    fn emit_jump(&mut self, st: &mut FnState, op: Opcode, a: u32, pos: u32) -> usize {
        let instr = match op {
            Opcode::Jmp => Instr::new_sj(op, 0),
            _ => Instr::asbx(op, a, 0),
        };
        self.emit(st, instr, pos);
        st.code.len() - 1
    }

    fn emit_back(&mut self, st: &mut FnState, op: Opcode, a: u32, target: usize, pos: u32) {
        let off = target as i64 - (st.code.len() as i64 + 1);
        let instr = match op {
            Opcode::Jmp => Instr::new_sj(op, off as i32),
            _ => Instr::asbx(op, a, off as i16),
        };
        self.emit(st, instr, pos);
    }

    // ---- 名字解析 ----

    fn resolve(&mut self, st: &mut FnState, name: Symbol) -> Option<Resolved> {
        if let Some((_, info)) = st.locals.iter().rev().find(|(n, _)| *n == name) {
            return Some(Resolved::Local(*info));
        }
        if let Some(i) = st.upvals.iter().position(|(n, _)| *n == name) {
            return Some(Resolved::Upval(i as u32));
        }
        if let Some(ctx) = &st.parent {
            if let Some((_, info)) = ctx.locals.iter().rev().find(|(n, _)| *n == name) {
                st.upvals.push((
                    name,
                    UpvalInfo {
                        src: CapSrc::Reg(info.reg),
                        is_mut: info.is_mut,
                        ref_mut: info.ref_mut,
                    },
                ));
                return Some(Resolved::Upval((st.upvals.len() - 1) as u32));
            }
            if let Some(i) = ctx.upvals.iter().position(|(n, _)| *n == name) {
                let (src, is_mut, ref_mut) = {
                    let (_, u) = &ctx.upvals[i];
                    (u.src, u.is_mut, u.ref_mut)
                };
                st.upvals.push((
                    name,
                    UpvalInfo {
                        src,
                        is_mut,
                        ref_mut,
                    },
                ));
                return Some(Resolved::Upval((st.upvals.len() - 1) as u32));
            }
        }
        self.slot_of(name).map(|(idx, ..)| Resolved::Slot(idx))
    }

    fn load_var(
        &mut self,
        st: &mut FnState,
        name: Symbol,
        is_top: bool,
        pos: u32,
    ) -> Result<u32, String> {
        match self.resolve(st, name) {
            Some(Resolved::Local(LocalInfo { reg, cell: false, .. })) => Ok(reg), // 零指令读取
            Some(Resolved::Local(LocalInfo { reg, cell: true, .. })) => {
                let rd = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::GetCell, rd, reg, 0), pos);
                Ok(rd)
            }
            Some(Resolved::Upval(i)) => {
                let rd = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::GetUpval, rd, i as u32, 0), pos);
                Ok(rd)
            }
            Some(Resolved::Slot(idx)) => {
                if is_top && st.parent.is_none() {
                    if let Some((_, decl_seq, ..)) = self.slot_of(name) {
                        if decl_seq > st.cur_seq {
                            return Err(format!("undefined variable: {}", name.as_str()));
                        }
                    }
                }
                let rd = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::GetSlot, rd, idx as u16), pos);
                Ok(rd)
            }
            None => Err(format!("undefined variable: {}", name.as_str())),
        }
    }

    /// 赋值目标的名字存储（`x = v` 的 x）。返回赋值表达式的值寄存器。
    fn store_var(
        &mut self,
        st: &mut FnState,
        name: Symbol,
        v: u32,
        is_top: bool,
        pos: u32,
    ) -> Result<u32, String> {
        match self.resolve(st, name) {
            Some(Resolved::Local(LocalInfo { reg, is_mut, cell: false, .. })) => {
                if !is_mut {
                    return Err(format!(
                        "cannot assign to immutable variable `{}`",
                        name.as_str()
                    ));
                }
                if reg != v {
                    self.emit(st, Instr::abc(Opcode::Move, reg, v, 0), pos);
                }
                Ok(reg)
            }
            Some(Resolved::Local(LocalInfo { reg, is_mut, cell: true, .. })) => {
                if !is_mut {
                    return Err(format!(
                        "cannot assign to immutable variable `{}`",
                        name.as_str()
                    ));
                }
                self.emit(st, Instr::abc(Opcode::SetCell, v, reg, 0), pos);
                Ok(v)
            }
            Some(Resolved::Upval(i)) => {
                if !st.upvals[i as usize].1.is_mut {
                    return Err(format!(
                        "cannot assign to immutable variable `{}`",
                        name.as_str()
                    ));
                }
                self.emit(st, Instr::abc(Opcode::SetUpval, v, i, 0), pos);
                Ok(v)
            }
            Some(Resolved::Slot(idx)) => {
                if !self.slot_mut(idx) {
                    return Err(format!(
                        "cannot assign to immutable variable `{}`",
                        name.as_str()
                    ));
                }
                self.emit(st, Instr::abx(Opcode::SetSlot, v, idx as u16), pos);
                Ok(v)
            }
            None => {
                // 顶层对未声明名字赋值 = 现声明建槽（对齐原型 StoreGlobal）；
                // 函数/lambda 内仍视为未定义。
                if st.parent.is_none() && is_top {
                    let idx = self.slot_declare(name, true, false);
                    self.emit(st, Instr::abx(Opcode::SetSlot, v, idx as u16), pos);
                    Ok(v)
                } else {
                    Err(format!("undefined variable: {}", name.as_str()))
                }
            }
        }
    }

    fn slot_mut(&self, idx: u32) -> bool {
        self.slots
            .get(idx as usize)
            .map(|(_, _, _, m, _)| *m)
            .unwrap_or(false)
    }

    // ---- 语句 ----

    fn compile_stmts(
        &mut self,
        st: &mut FnState,
        stmts: &[Stmt],
        is_top: bool,
    ) -> Result<u32, String> {
        if stmts.is_empty() {
            let r = self.temp(st)?;
            self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), 0);
            return Ok(r);
        }
        let n = stmts.len();
        let mut value = None;
        for (i, stmt) in stmts.iter().enumerate() {
            let last = i + 1 == n;
            if is_top {
                self.seq += 1;
                st.cur_seq = self.seq;
            }
            st.next_reg = st.temp_base; // 语句边界回收临时寄存器
            value = self.compile_stmt(st, stmt, is_top, last)?;
        }
        match value {
            Some(r) => Ok(r),
            // 块以 Return/Break/Continue 收尾：后续不可达，仍补一个合法结果寄存器。
            None => {
                let r = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), 0);
                Ok(r)
            }
        }
    }

    /// 返回 `None` 表示控制已离开（Return/Break/Continue），无块值。
    fn compile_stmt(
        &mut self,
        st: &mut FnState,
        s: &Stmt,
        is_top: bool,
        _last: bool,
    ) -> Result<Option<u32>, String> {
        let pos = s.span.lo.0;
        match &s.kind {
            StmtKind::Empty => {
                let r = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos);
                Ok(Some(r))
            }
            StmtKind::Expr(e) | StmtKind::ExprSemi(e) => {
                let r = self.compile_expr(st, e, is_top)?;
                Ok(Some(r))
            }
            StmtKind::Let(pat, init) => {
                let (name, is_mut) = match &pat.kind {
                    PatternKind::Bind(ident, m) => (ident.name, matches!(m, Mutability::Mut)),
                    _ => return Err("unsupported pattern in let".to_string()),
                };
                let ref_mut = init.as_ref().is_some_and(|e| {
                    matches!(&e.kind, ExprKind::Prefix(sym, _) if sym.as_str() == "&mut")
                });
                let v = match init {
                    Some(init) => self.compile_expr(st, init, is_top)?,
                    None => {
                        let r = self.temp(st)?;
                        self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos);
                        r
                    }
                };
                if is_top {
                    // 先取槽号再比较：`let x = x + 1` 中 RHS 的 x 解析到声明前状态。
                    let idx = self.slot_declare(name, is_mut, ref_mut);
                    self.emit(st, Instr::abx(Opcode::SetSlot, v, idx as u16), pos);
                } else {
                    let dst = self.bind_local(st, name, is_mut, ref_mut)?;
                    if st.promoted.contains(&name) {
                        // 升格：新绑定 = 新 cell（let 重声明不共享旧 cell，符合语义）。
                        self.emit(st, Instr::abc(Opcode::AllocCell, dst, v, 0), pos);
                    } else if dst != v {
                        self.emit(st, Instr::abc(Opcode::Move, dst, v, 0), pos);
                    }
                }
                let r = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos); // let 语句值为 void
                Ok(Some(r))
            }
        }
    }

    // ---- 表达式 ----

    fn compile_block(
        &mut self,
        st: &mut FnState,
        block: &Block,
        is_top: bool,
    ) -> Result<u32, String> {
        self.compile_stmts(st, &block.stmts, is_top)
    }

    /// 把表达式的值收进 `dst`（简单读取已是目标寄存器时零指令）。
    fn compile_into(
        &mut self,
        st: &mut FnState,
        dst: u32,
        e: &Expr,
        is_top: bool,
    ) -> Result<u32, String> {
        let r = self.compile_expr(st, e, is_top)?;
        if r != dst {
            self.emit(st, Instr::abc(Opcode::Move, dst, r, 0), e.span.lo.0);
        }
        Ok(dst)
    }

    fn compile_expr(
        &mut self,
        st: &mut FnState,
        e: &Expr,
        is_top: bool,
    ) -> Result<u32, String> {
        let pos = e.span.lo.0;
        match &e.kind {
            ExprKind::Literal(lit) => {
                if let Some(s) = Value::from_string_lit(lit) {
                    let o = self.heap.alloc(ObjKind::Str(s.into()));
                    let k = self.konst(Value::Obj(o))?;
                    let r = self.temp(st)?;
                    self.emit(st, Instr::abx(Opcode::LoadK, r, k), pos);
                    return Ok(r);
                }
                match lit_scalar(lit)? {
                    Some(Value::Nil) => {
                        let r = self.temp(st)?;
                        self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos);
                        Ok(r)
                    }
                    Some(Value::Bool(b)) => {
                        let r = self.temp(st)?;
                        self.emit(st, Instr::abc(Opcode::LoadB, r, b as u32, 0), pos);
                        Ok(r)
                    }
                    Some(Value::Int(v)) => {
                        let r = self.temp(st)?;
                        if let Some(s) = i16::try_from(v).ok() {
                            self.emit(st, Instr::asbx(Opcode::LoadI, r, s), pos);
                        } else {
                            let k = self.konst(Value::Int(v))?;
                            self.emit(st, Instr::abx(Opcode::LoadK, r, k), pos);
                        }
                        Ok(r)
                    }
                    Some(v) => {
                        let k = self.konst(v)?;
                        let r = self.temp(st)?;
                        self.emit(st, Instr::abx(Opcode::LoadK, r, k), pos);
                        Ok(r)
                    }
                    None => Err(format!("unsupported literal in this build: {lit}")),
                }
            }
            ExprKind::Nil => {
                let r = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos);
                Ok(r)
            }
            ExprKind::Paren(inner) => self.compile_expr(st, inner, is_top),
            ExprKind::Path(p) if p.segments.len() == 1 => {
                self.load_var(st, p.segments[0].ident.name, is_top, pos)
            }
            ExprKind::Path(_) => Err("qualified paths not supported".to_string()),
            ExprKind::Block(b) => self.compile_block(st, b, is_top),
            ExprKind::Array(items) => {
                let mut count = 0u32;
                let base = self.temp(st)?;
                for item in items {
                    match item {
                        ExprItem::Expr(e) => {
                            count += 1;
                            self.ensure_reg(st, base + count - 1)?;
                            self.compile_into(st, base + count - 1, e, is_top)?;
                        }
                        _ => return Err("expansion in array literal not supported".to_string()),
                    }
                }
                self.emit(st, Instr::abc(Opcode::NewArr, base, base, count), pos);
                Ok(base)
            }
            ExprKind::Subscript(base_e, args) => {
                let rb = self.compile_expr(st, base_e, is_top)?;
                if args.0.len() != 1 {
                    return Err("subscript requires exactly one index".to_string());
                }
                let idx = match &args.0[0] {
                    Argument::Ordinal(e) => e,
                    _ => return Err("named / expansion index not supported".to_string()),
                };
                let ri = self.temp(st)?;
                self.compile_into(st, ri, idx, is_top)?;
                let rd = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::GetElem, rd, rb, ri), pos);
                Ok(rd)
            }
            ExprKind::Lambda(l) => self.compile_lambda(st, l, pos),
            ExprKind::If(cond, then, else_) => {
                let rc = self.compile_expr(st, cond, is_top)?;
                let j_else = self.emit_jump(st, Opcode::JmpF, rc, pos);
                let rd = self.temp(st)?;
                self.compile_into(st, rd, then, is_top)?;
                let j_end = self.emit_jump(st, Opcode::Jmp, 0, pos);
                self.patch(st, j_else);
                match else_ {
                    Some(e) => {
                        self.compile_into(st, rd, e, is_top)?;
                    }
                    None => self.emit(st, Instr::abx(Opcode::LoadNil, rd, 0), pos),
                }
                self.patch(st, j_end);
                Ok(rd)
            }
            ExprKind::While(cond, body, else_, _label) => {
                let rd = self.temp(st)?;
                let loop_start = st.code.len();
                let rc = self.compile_expr(st, cond, is_top)?;
                let j_exit = self.emit_jump(st, Opcode::JmpF, rc, pos);
                st.loop_stack.push(LoopInfo {
                    breaks: Vec::new(),
                    continue_to: loop_start,
                });
                self.compile_expr(st, body, is_top)?; // 体值丢弃
                self.emit_back(st, Opcode::Jmp, 0, loop_start, pos);
                let info = st.loop_stack.pop().unwrap();
                self.patch(st, j_exit);
                match else_ {
                    Some(e) => {
                        self.compile_into(st, rd, e, is_top)?;
                    }
                    None => self.emit(st, Instr::abx(Opcode::LoadNil, rd, 0), pos),
                }
                for bp in info.breaks {
                    self.patch(st, bp);
                }
                Ok(rd)
            }
            ExprKind::For(pat, iter, body, else_block, _kind, _label) => {
                if else_block.is_some() {
                    return Err("for-else is not supported yet".to_string());
                }
                let var = match &pat.kind {
                    PatternKind::Bind(ident, _) => self.bind_local(st, ident.name, false, false)?,
                    _ => return Err("unsupported pattern in for loop".to_string()),
                };
                // #arr/#i 是循环的内部簿记（内部寄存器写，非用户赋值），不入 mut 检查。
                let arr = self.bind_local(st, Symbol::intern("#arr"), false, false)?;
                let i = self.bind_local(st, Symbol::intern("#i"), false, false)?;
                self.compile_into(st, arr, iter, is_top)?;
                self.emit(st, Instr::asbx(Opcode::LoadI, i, 0), pos);
                let loop_start = st.code.len();
                let len = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::Len, len, arr, 0), pos);
                let rc = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::Lt, rc, i, len), pos);
                let j_exit = self.emit_jump(st, Opcode::JmpF, rc, pos);
                let var_v = self.temp(st)?;
                self.emit(st, Instr::abc(Opcode::GetElem, var_v, arr, i), pos);
                self.emit(st, Instr::abc(Opcode::Move, var, var_v, 0), pos);
                st.loop_stack.push(LoopInfo {
                    breaks: Vec::new(),
                    continue_to: 0,
                });
                self.compile_block(st, body, is_top)?; // 体值丢弃
                let info_continue = st.loop_stack.last_mut().unwrap();
                let continue_to = st.code.len();
                info_continue.continue_to = continue_to;
                self.emit(st, Instr::asbx(Opcode::AddI, i, 1), pos);
                self.emit_back(st, Opcode::Jmp, 0, loop_start, pos);
                let info = st.loop_stack.pop().unwrap();
                self.patch(st, j_exit);
                let rd = self.temp(st)?;
                self.emit(st, Instr::abx(Opcode::LoadNil, rd, 0), pos);
                for bp in info.breaks {
                    self.patch(st, bp);
                }
                Ok(rd)
            }
            ExprKind::Return(val) => {
                let r = match val {
                    Some(v) => self.compile_expr(st, v, is_top)?,
                    None => {
                        let r = self.temp(st)?;
                        self.emit(st, Instr::abx(Opcode::LoadNil, r, 0), pos);
                        r
                    }
                };
                // 约定：返回值固定经 r0 传回（调用帧的 A 即被调帧 r0）。
                if r != 0 {
                    self.emit(st, Instr::abc(Opcode::Move, 0, r, 0), pos);
                }
                self.emit(st, Instr::abc(Opcode::Ret, 0, 0, 0), pos);
                Ok(r)
            }
            ExprKind::Break(val, _) => {
                if val.is_some() {
                    return Err("break with a value is not supported yet".to_string());
                }
                if st.loop_stack.is_empty() {
                    return Err("break outside loop".to_string());
                }
                let r = self.temp(st)?;
                let bp = self.emit_jump(st, Opcode::Jmp, 0, pos);
                st.loop_stack.last_mut().unwrap().breaks.push(bp);
                Ok(r)
            }
            ExprKind::Continue(_) => {
                let info = st
                    .loop_stack
                    .last()
                    .ok_or_else(|| "continue outside loop".to_string())?;
                let continue_to = info.continue_to;
                self.emit_back(st, Opcode::Jmp, 0, continue_to, pos);
                let r = self.temp(st)?;
                Ok(r)
            }
            ExprKind::Infix(sym, exprs) => {
                if *sym == op::Assign {
                    if exprs.len() != 2 {
                        return Err("assignment requires two operands".to_string());
                    }
                    return self.compile_assign(st, &exprs[0], &exprs[1], is_top, pos);
                }
                if *sym == op::AndAnd || *sym == op::OrOr {
                    if exprs.len() != 2 {
                        return Err("&& / || require two operands".to_string());
                    }
                    let and = *sym == op::AndAnd;
                    let rd = self.temp(st)?;
                    let r1 = self.compile_expr(st, &exprs[0], is_top)?;
                    // 左值未短路时两条路径都取右侧；短路目标只差常量（&& 假、|| 真）。
                    let short = self.emit_jump(
                        st,
                        if and { Opcode::JmpF } else { Opcode::JmpT },
                        r1,
                        pos,
                    );
                    self.compile_into(st, rd, &exprs[1], is_top)?;
                    let end = self.emit_jump(st, Opcode::Jmp, 0, pos);
                    self.patch(st, short);
                    self.emit(
                        st,
                        Instr::abc(Opcode::LoadB, rd, if and { 0 } else { 1 }, 0),
                        pos,
                    );
                    self.patch(st, end);
                    return Ok(rd);
                }
                let opcode = opcode_for(*sym)
                    .ok_or_else(|| format!("unsupported operator: {}", sym.as_str()))?;
                let mut iter = exprs.iter();
                let first = iter.next().expect("infix with no operands");
                let mut r = self.compile_expr(st, first, is_top)?;
                for e in iter {
                    let rt = self.temp(st)?;
                    self.compile_into(st, rt, e, is_top)?;
                    // dst 用新临时寄存器：左操作数可能是局部/参数，就地写会毁掉它。
                    let rd = self.temp(st)?;
                    self.emit(st, Instr::abc(opcode, rd, r, rt), pos);
                    r = rd;
                }
                Ok(r)
            }
            ExprKind::Comparison(syms, exprs) => {
                if exprs.len() != 2 {
                    return Err("chained comparisons are not supported yet".to_string());
                }
                let (opcode, swapped) = match syms[0] {
                    op::Less => (Opcode::Lt, false),
                    op::LessEqual => (Opcode::Le, false),
                    op::Greater => (Opcode::Lt, true),
                    op::GreaterEqual => (Opcode::Le, true),
                    s => return Err(format!("unsupported comparison: {}", s.as_str())),
                };
                let (a, b) = if swapped {
                    (&exprs[1], &exprs[0])
                } else {
                    (&exprs[0], &exprs[1])
                };
                let r1 = self.compile_expr(st, a, is_top)?;
                let r2 = self.temp(st)?;
                self.compile_into(st, r2, b, is_top)?;
                let rd = self.temp(st)?;
                self.emit(st, Instr::abc(opcode, rd, r1, r2), pos);
                Ok(rd)
            }
            ExprKind::Prefix(sym, inner) => self.compile_prefix(st, *sym, inner, is_top, pos),
            ExprKind::FuncCall(callee, args, _) => {
                if !matches!(&callee.kind, ExprKind::Path(p) if p.segments.len() == 1) {
                    // 链式/任意表达式被调：值降级后按间接调用约定布局。
                    let mut count = 0u32;
                    for arg in &args.0 {
                        match arg {
                            Argument::Ordinal(_) => count += 1,
                            _ => return Err(
                                "named / expansion arguments not supported yet".to_string()
                            ),
                        }
                    }
                    let v = self.compile_expr(st, callee, is_top)?;
                    let rc = self.temp(st)?;
                    if rc != v {
                        self.emit(st, Instr::abc(Opcode::Move, rc, v, 0), pos);
                    }
                    self.emit_call_args(st, rc, &args.0, count, is_top, pos)?;
                    return Ok(rc + 1);
                }
                let name = match &callee.kind {
                    ExprKind::Path(p) if p.segments.len() == 1 => p.segments[0].ident.name,
                    _ => unreachable!("non-path callee handled above"),
                };
                let mut count = 0u32;
                for arg in &args.0 {
                    match arg {
                        Argument::Ordinal(_) => count += 1,
                        _ => {
                            return Err(
                                "named / expansion arguments not supported yet".to_string()
                            )
                        }
                    }
                }
                if name.as_str() == "print" {
                    let ra = self.temp(st)?;
                    let mut k = 0u32;
                    for arg in &args.0 {
                        if let Argument::Ordinal(e) = arg {
                            self.compile_into(st, ra + k, e, is_top)?;
                            k += 1;
                        }
                    }
                    self.emit(st, Instr::abc(Opcode::CallNat, ra, 0, count), pos);
                    return Ok(ra);
                }
                // 被调者优先级：局部 / upval / 槽（间接 CALL）→ 模块函数（CALLK）。
                match self.resolve(st, name) {
                    Some(Resolved::Local(info)) => {
                        let rc = self.temp(st)?;
                        if info.cell {
                            self.emit(st, Instr::abc(Opcode::GetCell, rc, info.reg, 0), pos);
                        } else {
                            self.emit(st, Instr::abc(Opcode::Move, rc, info.reg, 0), pos);
                        }
                        self.emit_call_args(st, rc, &args.0, count, is_top, pos)?;
                        Ok(rc + 1)
                    }
                    Some(Resolved::Upval(i)) => {
                        let rc = self.temp(st)?;
                        self.emit(st, Instr::abc(Opcode::GetUpval, rc, i as u32, 0), pos);
                        self.emit_call_args(st, rc, &args.0, count, is_top, pos)?;
                        Ok(rc + 1)
                    }
                    Some(Resolved::Slot(idx)) => {
                        let rc = self.temp(st)?;
                        self.emit(st, Instr::abx(Opcode::GetSlot, rc, idx as u16), pos);
                        self.emit_call_args(st, rc, &args.0, count, is_top, pos)?;
                        Ok(rc + 1)
                    }
                    None => {
                        let idx = *self
                            .func_indices
                            .get(&name)
                            .ok_or_else(|| format!("undefined function: {}", name.as_str()))?;
                        let ra = self.temp(st)?;
                        let mut k = 0u32;
                        for arg in &args.0 {
                            if let Argument::Ordinal(e) = arg {
                                self.ensure_reg(st, ra + k)?;
                                self.compile_into(st, ra + k, e, is_top)?;
                                k += 1;
                            }
                        }
                        self.emit(st, Instr::abc(Opcode::CallK, ra, idx, count), pos);
                        Ok(ra)
                    }
                }
            }
            other => Err(format!("unsupported expression in this build: {other:?}")),
        }
    }

    /// 间接调用的实参布局：被调值在 rc，实参占 rc+1..rc+1+C，结果落在 rc+1。
    fn emit_call_args(
        &mut self,
        st: &mut FnState,
        rc: u32,
        args: &[Argument],
        count: u32,
        is_top: bool,
        pos: u32,
    ) -> Result<(), String> {
        let mut k = 1u32;
        for arg in args {
            if let Argument::Ordinal(e) = arg {
                self.ensure_reg(st, rc + k)?;
                self.compile_into(st, rc + k, e, is_top)?;
                k += 1;
            }
        }
        // 结果落在 rc+1（被调 r0 = 本帧的 rc+1 槽），必须在本帧窗口内。
        self.ensure_reg(st, rc + 1)?;
        self.emit(st, Instr::abc(Opcode::Call, rc, 0, count), pos);
        Ok(())
    }

    fn compile_assign(
        &mut self,
        st: &mut FnState,
        target: &Expr,
        value: &Expr,
        is_top: bool,
        pos: u32,
    ) -> Result<u32, String> {
        match &target.kind {
            ExprKind::Path(p) if p.segments.len() == 1 => {
                let v = self.compile_expr(st, value, is_top)?;
                self.store_var(st, p.segments[0].ident.name, v, is_top, pos)
            }
            ExprKind::Prefix(sym, inner) if *sym == op::Multiply => {
                // *r = v：只消费 v 这个 source，r 不失效。r 绑定须持可变引用。
                if let ExprKind::Path(p) = &inner.kind {
                    if p.segments.len() == 1 {
                        let name = p.segments[0].ident.name;
                        self.check_ref_mut(st, name)?;
                    }
                }
                let rref = self.compile_expr(st, inner, is_top)?;
                let v = self.compile_expr(st, value, is_top)?;
                self.emit(st, Instr::abc(Opcode::StoreRef, v, rref, 0), pos);
                Ok(v)
            }
            ExprKind::Subscript(base_e, args) => {
                if args.0.len() != 1 {
                    return Err("subscript assignment requires exactly one index".to_string());
                }
                // 容器为具名绑定时要求 mut（复杂表达式的检查随类型检查器）。
                if let ExprKind::Path(p) = &base_e.kind {
                    if p.segments.len() == 1 {
                        let name = p.segments[0].ident.name;
                        match self.resolve(st, name) {
                            Some(Resolved::Local(info)) if !info.is_mut => {
                                return Err(format!(
                                    "cannot mutate immutable variable `{}`",
                                    name.as_str()
                                ))
                            }
                            Some(Resolved::Upval(i)) if !st.upvals[i as usize].1.is_mut => {
                                return Err(format!(
                                    "cannot mutate immutable variable `{}`",
                                    name.as_str()
                                ))
                            }
                            Some(Resolved::Slot(idx)) if !self.slot_mut(idx) => {
                                return Err(format!(
                                    "cannot mutate immutable variable `{}`",
                                    name.as_str()
                                ))
                            }
                            _ => {}
                        }
                    }
                }
                let idx = match &args.0[0] {
                    Argument::Ordinal(e) => e,
                    _ => return Err("named / expansion index not supported".to_string()),
                };
                let rb = self.compile_expr(st, base_e, is_top)?;
                let ri = self.temp(st)?;
                self.compile_into(st, ri, idx, is_top)?;
                let v = self.compile_expr(st, value, is_top)?;
                self.emit(st, Instr::abc(Opcode::SetElem, v, rb, ri), pos);
                Ok(v)
            }
            _ => Err("assignment target must be a variable, dereference or subscript".to_string()),
        }
    }

    fn compile_prefix(
        &mut self,
        st: &mut FnState,
        sym: Symbol,
        inner: &Expr,
        is_top: bool,
        pos: u32,
    ) -> Result<u32, String> {
        if sym == op::Amp || sym.as_str() == "&mut" {
            // `&` / `&mut`：M1 不区分权限（权限在类型层）。
            return match &inner.kind {
                ExprKind::Path(p) if p.segments.len() == 1 => {
                    let name = p.segments[0].ident.name;
                    let mutable = sym.as_str() == "&mut";
                    match self.resolve(st, name) {
                        Some(Resolved::Local(info)) => {
                            if mutable && !info.is_mut {
                                return Err(format!(
                                    "cannot take a mutable reference to immutable variable `{}`",
                                    name.as_str()
                                ));
                            }
                            if !info.cell {
                                return Err(format!(
                                    "internal: reference target {} was not promoted",
                                    name.as_str()
                                ));
                            }
                            let rd = self.temp(st)?;
                            self.emit(st, Instr::abc(Opcode::RefCell, rd, info.reg, 0), pos);
                            Ok(rd)
                        }
                        Some(Resolved::Upval(i)) => {
                            // upval 槽本身持 cell：加载 cell 即得引用值。
                            if mutable && !st.upvals[i as usize].1.is_mut {
                                return Err(format!(
                                    "cannot take a mutable reference to immutable variable `{}`",
                                    name.as_str()
                                ));
                            }
                            let rd = self.temp(st)?;
                            self.emit(st, Instr::abc(Opcode::Upval, rd, i, 0), pos);
                            Ok(rd)
                        }
                        Some(Resolved::Slot(idx)) => {
                            if mutable && !self.slot_mut(idx) {
                                return Err(format!(
                                    "cannot take a mutable reference to immutable variable `{}`",
                                    name.as_str()
                                ));
                            }
                            self.amp_targets.insert(name);
                            let rd = self.temp(st)?;
                            self.emit(st, Instr::abx(Opcode::GetSlotCell, rd, idx as u16), pos);
                            Ok(rd)
                        }
                        None => Err(format!("undefined variable: {}", name.as_str())),
                    }
                }
                ExprKind::Subscript(base_e, args) => {
                    // &a[i]：投影操作数在此求值一次并捕获；越界即错误值。
                    if args.0.len() != 1 {
                        return Err("subscript requires exactly one index".to_string());
                    }
                    let idx = match &args.0[0] {
                        Argument::Ordinal(e) => e,
                        _ => return Err("named / expansion index not supported".to_string()),
                    };
                    let rb = self.compile_expr(st, base_e, is_top)?;
                    let ri = self.temp(st)?;
                    self.compile_into(st, ri, idx, is_top)?;
                    let rd = self.temp(st)?;
                    self.emit(st, Instr::abc(Opcode::RefElem, rd, rb, ri), pos);
                    Ok(rd)
                }
                _ => Err("unsupported reference target".to_string()),
            };
        }
        if sym == op::Multiply {
            // *r 解引用读；不是 move source（检查器职责）。
            let r = self.compile_expr(st, inner, is_top)?;
            let rd = self.temp(st)?;
            self.emit(st, Instr::abc(Opcode::LoadRef, rd, r, 0), pos);
            return Ok(rd);
        }
        let r = self.compile_expr(st, inner, is_top)?;
        if sym == op::Minus {
            let rd = self.temp(st)?;
            self.emit(st, Instr::abc(Opcode::NegI64, rd, r, 0), pos);
            Ok(rd)
        } else if sym == op::Plus {
            Ok(r)
        } else {
            Err(format!("unsupported prefix operator: {}", sym.as_str()))
        }
    }

    /// `*r = v` 的 r 绑定须持可变引用（复杂表达式目标随类型检查器）。
    fn check_ref_mut(&mut self, st: &mut FnState, name: Symbol) -> Result<(), String> {
        match self.resolve(st, name) {
            Some(Resolved::Local(info)) if !info.ref_mut => Err(format!(
                "cannot assign through an immutable reference `{}`",
                name.as_str()
            )),
            Some(Resolved::Upval(i)) if !st.upvals[i as usize].1.ref_mut => Err(format!(
                "cannot assign through an immutable reference `{}`",
                name.as_str()
            )),
            Some(Resolved::Slot(idx)) if !self.slot_ref_mut(idx) => Err(format!(
                "cannot assign through an immutable reference `{}`",
                name.as_str()
            )),
            _ => Ok(()),
        }
    }

    /// lambda 降级：独立 proto + CLOSURE + 捕获描述伪指令。捕获全部按引用。
    ///
    /// 捕获解析在创建时**沿活父链急切完成**：对本 lambda 的自由名逐个经父级
    /// `resolve`——命中父级局部 → `CAPREF`（父级寄存器必为 cell，升格预扫描
    /// 保证）；命中父级 upval → `CAPUP`；命中槽位 → 不捕获（GETSLOT 直达）。
    /// 父级自身经由更外层的捕获在此过程中顺势建立（中间层不为直接引用也会
    /// 补上转发 upval）。函数体内体降级期间的惰性解析仅作兜底。
    fn compile_lambda(
        &mut self,
        st: &mut FnState,
        l: &xc_ast::lambda::Lambda,
        pos: u32,
    ) -> Result<u32, String> {
        let mut cst = FnState::new(
            st.promoted.clone(),
            Some(CaptureCtx {
                locals: st.locals.clone(),
                upvals: st.upvals.clone(),
            }),
        );
        // 急切解析自由名（相对本 lambda 的作用域：参数为界）。
        let mut bound: Vec<Symbol> = l.params.iter().map(|p| p.ident.name).collect();
        let free = match &l.body {
            FuncBody::Block(b) => scan_stmts(&b.stmts, &mut bound, &mut st.promoted.clone()),
            FuncBody::Arrow(e) => scan_expr(e, &mut bound, &mut st.promoted.clone()),
            FuncBody::Semicolon => return Err("lambda without body".to_string()),
        };
        for name in free {
            if cst.upvals.iter().any(|(n, _)| *n == name) {
                continue;
            }
            match self.resolve(st, name) {
                Some(Resolved::Local(info)) => {
                    cst.upvals.push((
                        name,
                        UpvalInfo {
                            src: CapSrc::Reg(info.reg),
                            is_mut: info.is_mut,
                            ref_mut: info.ref_mut,
                        },
                    ));
                }
                Some(Resolved::Upval(i)) => {
                    // 经外层 lambda 转发：改写为 ParentUpval（外层 upval 槽），
                    // 原样继承会把外层创建帧的寄存器号错当成本层创建帧的。
                    let (is_mut, ref_mut) = {
                        let u = &st.upvals[i as usize].1;
                        (u.is_mut, u.ref_mut)
                    };
                    cst.upvals.push((
                        name,
                        UpvalInfo {
                            src: CapSrc::ParentUpval(i),
                            is_mut,
                            ref_mut,
                        },
                    ));
                }
                // 槽位/函数名不捕获：子体内按槽/函数直接寻址。
                Some(Resolved::Slot(_)) | None => {}
            }
        }
        // 参数落位（升格参数就地 cell 化）。参数默认不可变（M1：无 mut 参数语法）。
        for p in &l.params {
            let preg = self.bind_local(&mut cst, p.ident.name, false, false)?;
            if cst.promoted.contains(&p.ident.name) {
                // 升格参数：cell 入新寄存器，名字必须重绑到 cell 寄存器（约定 D）。
                let creg = self.temp(&mut cst)?;
                self.emit(&mut cst, Instr::abc(Opcode::AllocCell, creg, preg, 0), pos);
                if let Some(entry) = cst.locals.iter_mut().find(|(n, _)| *n == p.ident.name) {
                    entry.1.reg = creg;
                    entry.1.cell = true;
                }
            }
        }
        match &l.body {
            FuncBody::Block(b) => {
                let vr = self.compile_stmts(&mut cst, &b.stmts, false)?;
                if vr != 0 {
                    self.emit(&mut cst, Instr::abc(Opcode::Move, 0, vr, 0), pos);
                }
            }
            FuncBody::Arrow(e) => {
                let vr = self.compile_expr(&mut cst, e, false)?;
                if vr != 0 {
                    self.emit(&mut cst, Instr::abc(Opcode::Move, 0, vr, 0), pos);
                }
            }
            FuncBody::Semicolon => unreachable!("checked above"),
        }
        self.emit(&mut cst, Instr::abc(Opcode::Ret, 0, 0, 0), pos);
        let idx = (self.lambda_base + self.pending.len()) as u16;
        self.lambda_seq += 1;
        let upvals = cst.upvals.clone();
        self.pending
            .push(cst.finish(format!("<lambda{}>", self.lambda_seq), l.params.len() as u32));
        let rd = self.temp(st)?;
        self.emit(st, Instr::abx(Opcode::Closure, rd, idx), pos);
        for (_, info) in &upvals {
            let pseudo = match info.src {
                CapSrc::Reg(r) => Instr::abc(Opcode::CapRef, 0, r, 0),
                CapSrc::ParentUpval(i) => Instr::abc(Opcode::CapUp, 0, i, 0),
            };
            self.emit(st, pseudo, pos);
        }
        Ok(rd)
    }

    fn lower_func(&mut self, f: &P<Func>) -> Result<(), String> {
        let promoted = scan_promoted_func(f);
        let mut st = FnState::new(promoted, None);
        let name = f.ident.as_str().to_string();
        let arity = f.sig.params.len() as u32;
        for p in &f.sig.params {
            let preg = self.bind_local(&mut st, p.ident.name, false, false)?;
            if st.promoted.contains(&p.ident.name) {
                // 升格参数：cell 入新寄存器，名字必须重绑到 cell 寄存器——
                // 否则后续读写/捕获拿到的是未包装的参数值（fib 级真 bug）。
                let creg = self.temp(&mut st)?;
                self.emit(&mut st, Instr::abc(Opcode::AllocCell, creg, preg, 0), 0);
                if let Some(entry) = st.locals.iter_mut().find(|(n, _)| *n == p.ident.name) {
                    entry.1.reg = creg;
                    entry.1.cell = true;
                }
            }
        }
        match &f.body {
            FuncBody::Block(block) => {
                let vr = self.compile_block(&mut st, block, false)?;
                if vr != 0 {
                    self.emit(&mut st, Instr::abc(Opcode::Move, 0, vr, 0), 0);
                }
            }
            FuncBody::Arrow(e) => {
                let vr = self.compile_expr(&mut st, e, false)?;
                if vr != 0 {
                    self.emit(&mut st, Instr::abc(Opcode::Move, 0, vr, 0), 0);
                }
            }
            FuncBody::Semicolon => {
                return Err(format!("function without body: {}", f.ident.as_str()))
            }
        }
        self.emit(&mut st, Instr::abc(Opcode::Ret, 0, 0, 0), 0);
        self.funcs.push(st.finish(name, arity));
        Ok(())
    }
}

fn opcode_for(sym: Symbol) -> Option<Opcode> {
    use op::*;
    Some(match sym {
        Plus => Opcode::AddI64,
        Minus => Opcode::SubI64,
        Multiply => Opcode::MulI64,
        Divide => Opcode::DivI64,
        Modulo => Opcode::ModI64,
        Concat => Opcode::Concat,
        Equal => Opcode::Eq,
        NotEqual => Opcode::Ne,
        _ => return None,
    })
}

/// 按原指令的格式重写跳转偏移（Jmp 走 24 位，条件跳转走 iAsBx 的 16 位并保留 A）。
fn jump_instr(old: Instr, off: i64) -> Instr {
    match old.op() {
        Opcode::Jmp => Instr::new_sj(Opcode::Jmp, off as i32),
        op => Instr::asbx(op, old.a(), off as i16),
    }
}

// ---- 升格预扫描 ----
//
// 判据（超近似，多升格无害）：
// (a) `&x` / `&mut x` 的目标（含槽位名，供 cell_slots 使用）；
// (b) lambda 的自由名——lambda 体以**自身参数 + 自身 let** 为界（外层名字都是
//     捕获），自由名标记进升格名单，并向上层传播（滤去本层绑定后成为本层的
//     自由名）：内层 lambda 捕获外层 lambda 的参数时，该参数必须 cell 化。
//
// 返回值 = 本作用域的自由名（向上传播用）。

fn scan_promoted_stmts(stmts: &[Stmt]) -> HashSet<Symbol> {
    let mut set = HashSet::new();
    scan_stmts(stmts, &mut Vec::new(), &mut set);
    set
}

fn scan_promoted_func(f: &P<Func>) -> HashSet<Symbol> {
    let mut set = HashSet::new();
    match &f.body {
        FuncBody::Block(b) => {
            scan_stmts(&b.stmts, &mut Vec::new(), &mut set);
        }
        FuncBody::Arrow(e) => {
            scan_expr(e, &mut Vec::new(), &mut set);
        }
        FuncBody::Semicolon => {}
    }
    set
}

fn scan_stmts(stmts: &[Stmt], bound: &mut Vec<Symbol>, set: &mut HashSet<Symbol>) -> Vec<Symbol> {
    let mut free = Vec::new();
    for s in stmts {
        match &s.kind {
            StmtKind::Empty => {}
            StmtKind::Expr(e) | StmtKind::ExprSemi(e) => {
                free.extend(scan_expr(e, bound, set));
            }
            StmtKind::Let(pat, init) => {
                if let Some(init) = init {
                    free.extend(scan_expr(init, bound, set));
                }
                if let PatternKind::Bind(ident, _) = &pat.kind {
                    bound.push(ident.name);
                }
            }
        }
    }
    free
}

fn scan_expr(e: &Expr, bound: &mut Vec<Symbol>, set: &mut HashSet<Symbol>) -> Vec<Symbol> {
    match &e.kind {
        ExprKind::Path(p) if p.segments.len() == 1 => {
            let name = p.segments[0].ident.name;
            if bound.contains(&name) {
                Vec::new()
            } else {
                vec![name]
            }
        }
        ExprKind::Prefix(sym, inner) => {
            if (*sym == op::Amp || sym.as_str() == "&mut")
                && matches!(inner.kind, ExprKind::Path(ref p) if p.segments.len() == 1)
            {
                if let ExprKind::Path(p) = &inner.kind {
                    let name = p.segments[0].ident.name;
                    set.insert(name);
                    // 引用可逃逸 = 捕获：未在本层绑定的名字向上传播。
                    return if bound.contains(&name) {
                        Vec::new()
                    } else {
                        vec![name]
                    };
                }
            }
            scan_expr(inner, bound, set)
        }
        ExprKind::Paren(inner) => scan_expr(inner, bound, set),
        ExprKind::Array(items) => {
            let mut free = Vec::new();
            for item in items {
                if let ExprItem::Expr(e) = item {
                    free.extend(scan_expr(e, bound, set));
                }
            }
            free
        }
        ExprKind::Subscript(base, args) => {
            let mut free = scan_expr(base, bound, set);
            for arg in &args.0 {
                if let Argument::Ordinal(e) = arg {
                    free.extend(scan_expr(e, bound, set));
                }
            }
            free
        }
        ExprKind::Lambda(l) => {
            let mut b2 = Vec::new();
            for p in &l.params {
                b2.push(p.ident.name);
            }
            let f = match &l.body {
                FuncBody::Block(b) => scan_stmts(&b.stmts, &mut b2, set),
                FuncBody::Arrow(e2) => scan_expr(e2, &mut b2, set),
                FuncBody::Semicolon => Vec::new(),
            };
            // lambda 的自由名都经创建处帧捕获：绑定层全部升格。
            set.extend(&f);
            // 未在本层绑定的名字继续向上传播。
            f.into_iter().filter(|n| !bound.contains(n)).collect()
        }
        ExprKind::If(c, t, el) => {
            let mut free = scan_expr(c, bound, set);
            free.extend(scan_expr(t, bound, set));
            if let Some(el) = el {
                free.extend(scan_expr(el, bound, set));
            }
            free
        }
        ExprKind::While(c, body, el, _) => {
            let mut free = scan_expr(c, bound, set);
            free.extend(scan_expr(body, bound, set));
            if let Some(el) = el {
                free.extend(scan_expr(el, bound, set));
            }
            free
        }
        ExprKind::For(pat, iter, body, else_, _kind, _label) => {
            let mut free = scan_expr(iter, bound, set);
            if let PatternKind::Bind(ident, _) = &pat.kind {
                bound.push(ident.name);
            }
            free.extend(scan_stmts(&body.stmts, bound, set));
            if let Some(el) = else_ {
                free.extend(scan_stmts(&el.stmts, bound, set));
            }
            free
        }
        ExprKind::Return(v) | ExprKind::Throw(v) | ExprKind::Break(v, _) => {
            v.as_ref().map_or(Vec::new(), |v| scan_expr(v, bound, set))
        }
        ExprKind::Continue(_) => Vec::new(),
        ExprKind::Infix(_, exprs) | ExprKind::Comparison(_, exprs) => {
            let mut free = Vec::new();
            for e in exprs {
                free.extend(scan_expr(e, bound, set));
            }
            free
        }
        ExprKind::FuncCall(callee, args, _) => {
            let mut free = scan_expr(callee, bound, set);
            for arg in &args.0 {
                if let Argument::Ordinal(e) = arg {
                    free.extend(scan_expr(e, bound, set));
                }
            }
            free
        }
        ExprKind::Block(b) => scan_stmts(&b.stmts, bound, set),
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use thin_vec::thin_vec;
    use xc_ast::expr::ExprFlags;
    use xc_ast::literal::{Literal, LiteralKind};
    use xc_ast::stmt::Stmt;
    use xc_span::create_session_globals_then;
    use xc_span::source_map::Filename;
    use xc_span::Span;

    fn lit_int(n: i64) -> P<Expr> {
        P(Expr {
            kind: ExprKind::Literal(Literal::new(
                LiteralKind::Integer,
                Symbol::intern(&n.to_string()),
                None,
            )),
            span: Span::DUMMY,
            flags: ExprFlags::empty(),
        })
    }

    fn bin(op_sym: Symbol, l: P<Expr>, r: P<Expr>) -> P<Expr> {
        P(Expr {
            kind: ExprKind::Infix(op_sym, thin_vec![l, r]),
            span: Span::DUMMY,
            flags: ExprFlags::empty(),
        })
    }

    #[test]
    fn infix_emits_typed_ops() {
        create_session_globals_then(|| {
            let program = Program {
                funcs: vec![],
                func_indices: HashMap::new(),
                var_names: HashSet::new(),
                top: vec![Stmt {
                    kind: StmtKind::ExprSemi(bin(op::Plus, lit_int(1), lit_int(2))),
                    span: Span::DUMMY,
                }],
            };
            let m = compile(&program).unwrap();
            let ops: Vec<Opcode> = m.funcs[0].code.iter().map(|i| i.op()).collect();
            // 运算 dst 用新临时寄存器（ISA 规则：dst 永不覆盖局部/参数）；
            // compile_into 暂走「临时寄存器 + MOVE」慢路径（M6 做字面量直写）。
            assert_eq!(
                ops,
                vec![
                    Opcode::LoadI,
                    Opcode::LoadI,
                    Opcode::Move,
                    Opcode::AddI64,
                    Opcode::Ret
                ]
            );
        });
    }

    /// golden 快照：锁定递归调用的降级形态（寄存器分配器改动会在此显形）。
    #[test]
    fn golden_fib_dump() {
        create_session_globals_then(|| {
            let src = "func fib(n: int) { if n < 2 { return n; } return fib(n - 1) + fib(n - 2); } print(fib(3));";
            let parsed = xc_check::parse_program(Filename::Normal("t".into()), src).unwrap();
            let program = xc_check::check(parsed).unwrap();
            let m = compile(&program).unwrap();
            let dump = crate::isa::dump_module(&m);
            let expected = "\
func <top> (arity 0, regs 3):
  0000 LOADI r2, 3
  0001 MOVE r1, r2
  0002 CALLK r1, fib, 1
  0003 MOVE r0, r1
  0004 CALLNAT r0, 0, 1
  0005 RET r0

func fib (arity 1, regs 11):
  0000 LOADI r2, 2
  0001 MOVE r1, r2
  0002 LT r3, r0, r1
  0003 JMPF r3, -> 7
  0004 RET r0
  0005 MOVE r4, r0
  0006 JMP -> 8
  0007 LOADNIL r4
  0008 LOADI r3, 1
  0009 MOVE r2, r3
  0010 SUBI64 r4, r0, r2
  0011 MOVE r1, r4
  0012 CALLK r1, fib, 1
  0013 LOADI r8, 2
  0014 MOVE r7, r8
  0015 SUBI64 r9, r0, r7
  0016 MOVE r6, r9
  0017 CALLK r6, fib, 1
  0018 MOVE r5, r6
  0019 ADDI64 r10, r1, r5
  0020 MOVE r0, r10
  0021 RET r0
  0022 MOVE r0, r10
  0023 RET r0
";
            assert_eq!(dump, expected);
            let out = crate::interp::run(&m, src).unwrap();
            assert_eq!(out, "2\n");
        });
    }

    // ---- M1 验收：备忘录附录 A 范例一（闭包与可变捕获）----

    #[test]
    fn example1_closure_counter() {
        create_session_globals_then(|| {
            let src = "\
func makeCounter() { let mut n = 0; return do () { n = n + 1; n }; }
let c = makeCounter();
print(c());
print(c());
";
            let parsed = xc_check::parse_program(Filename::Normal("t".into()), src).unwrap();
            let program = xc_check::check(parsed).unwrap();
            let m = compile(&program).unwrap();
            let out = crate::interp::run(&m, src).unwrap();
            assert_eq!(out, "1\n2\n");
        });
    }

    // ---- M1 验收：备忘录附录 A 范例二（place 引用）----

    #[test]
    fn example2_place_references() {
        create_session_globals_then(|| {
            let src = "\
let mut x = 1;
let r = &mut x;
*r = 2;
let a = [1, 2, 3];
let p = &a[1];
x = 3;
print(*p);
print(*r);
";
            let parsed = xc_check::parse_program(Filename::Normal("t".into()), src).unwrap();
            let program = xc_check::check(parsed).unwrap();
            let m = compile(&program).unwrap();
            let out = crate::interp::run(&m, src).unwrap();
            // x = 3 后 *r 观察到 3（赋值可见）；*p 读 a[1] = 2（数组未被改）。
            assert_eq!(out, "2\n3\n");
        });
    }

    #[test]
    fn subscript_read_write() {
        create_session_globals_then(|| {
            for (src, want) in [
                ("let a = [1, 2]; print(a[0]);", "1\n"),
                ("let mut a = [1]; a[0] = 5; print(a[0]);", "5\n"),
            ] {
                let parsed = xc_check::parse_program(Filename::Normal("t".into()), src).unwrap();
                let program = xc_check::check(parsed).unwrap();
                let m = compile(&program).unwrap();
                assert_eq!(crate::interp::run(&m, src).unwrap(), want, "{src}");
            }
        });
    }

    #[test]
    fn nested_lambda_captures() {
        create_session_globals_then(|| {
            // 三层捕获链：inner 捕获 outer 的 upval（CAPUP），outer 捕获 f 的参数（CAPREF）。
            let src = "\
func adder(n: int) { return do (x: int) { return do (y: int) { x + y + n }; }; }
let g = adder(10);
print(g(1)(2));
";
            let parsed = xc_check::parse_program(Filename::Normal("t".into()), src).unwrap();
            let program = xc_check::check(parsed).unwrap();
            let m = compile(&program).unwrap();
            let out = crate::interp::run(&m, src).unwrap();
            assert_eq!(out, "13\n");
        });
    }
}