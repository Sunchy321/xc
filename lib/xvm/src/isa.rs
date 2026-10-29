//! 指令编码：定长 32 位，四格式（vm-bytecode-design-2026-10.md §二）。
//!
//! ```text
//! iABC : | op:8 | A:8  | B:8  | C:8  |
//! iABx : | op:8 | A:8  | Bx:16        |
//! iAsBx: | op:8 | A:8  | sBx:16 (有符号) |
//! isJ  : | op:8 | sJ:24 (有符号，相对下一条指令)  |
//! ```
//!
//! M0 指令子集（ISA v0.2 的前缀）：er 通道、panic、闭包、async、字典随后续里程碑加入。

use std::fmt;
use std::fmt::Write;

use crate::lower::{FuncProto, Module};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    Move = 0,
    LoadI = 1,
    LoadK = 2,
    LoadNil = 3,
    LoadB = 4,
    GetSlot = 5,
    SetSlot = 6,
    AddI64 = 7,
    SubI64 = 8,
    MulI64 = 9,
    DivI64 = 10,
    ModI64 = 11,
    AddI = 12,
    NegI64 = 13,
    Concat = 14,
    Eq = 15,
    Lt = 16,
    Le = 17,
    Jmp = 18,
    JmpF = 19,
    JmpT = 20,
    NewArr = 21,
    Len = 22,
    GetElem = 23,
    SetElem = 24,
    CallK = 25,
    CallNat = 26,
    Ret = 27,
    Ne = 28,
    AllocCell = 29,
    GetCell = 30,
    SetCell = 31,
    RefCell = 32,
    RefElem = 33,
    LoadRef = 34,
    StoreRef = 35,
    LoadFunc = 36,
    Closure = 37,
    CapRef = 38,
    GetUpval = 39,
    SetUpval = 40,
    Upval = 41,
    Call = 42,
    CapUp = 43,
    GetSlotCell = 44,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    Abc,
    ABx,
    AsBx,
    SJ,
}

pub fn format_of(op: Opcode) -> Format {
    match op {
        // 条件跳转需要（条件寄存器， 偏移）两个字段：走 iAsBx（A + sJ:16）。
        Opcode::LoadI | Opcode::AddI | Opcode::JmpF | Opcode::JmpT => Format::AsBx,
        Opcode::LoadK | Opcode::GetSlot | Opcode::SetSlot => Format::ABx,
        Opcode::Jmp => Format::SJ,
        _ => Format::Abc,
    }
}

/// 一条定长指令。按 `format_of(op)` 解释位段。
#[derive(Clone, Copy, PartialEq)]
pub struct Instr(u32);

impl Instr {
    pub fn new(op: Opcode) -> Self {
        Instr((op as u32) << 24)
    }

    pub fn abc(op: Opcode, a: u32, b: u32, c: u32) -> Self {
        debug_assert!(a <= 0xff && b <= 0xff && c <= 0xff);
        Instr((op as u32) << 24 | a << 16 | b << 8 | c)
    }

    pub fn abx(op: Opcode, a: u32, bx: u16) -> Self {
        debug_assert!(a <= 0xff);
        Instr((op as u32) << 24 | a << 16 | bx as u32)
    }

    pub fn asbx(op: Opcode, a: u32, sbx: i16) -> Self {
        debug_assert!(a <= 0xff);
        Instr((op as u32) << 24 | a << 16 | sbx as u16 as u32)
    }

    pub fn new_sj(op: Opcode, sj: i32) -> Self {
        debug_assert!(-(1 << 23) <= sj && sj < (1 << 23));
        Instr((op as u32) << 24 | (sj as u32 & 0x00ff_ffff))
    }

    pub fn op(self) -> Opcode {
        let v = (self.0 >> 24) as u8;
        if v == Opcode::Move as u8 {
            return Opcode::Move;
        }
        if v == Opcode::LoadI as u8 {
            return Opcode::LoadI;
        }
        if v == Opcode::LoadK as u8 {
            return Opcode::LoadK;
        }
        if v == Opcode::LoadNil as u8 {
            return Opcode::LoadNil;
        }
        if v == Opcode::LoadB as u8 {
            return Opcode::LoadB;
        }
        if v == Opcode::GetSlot as u8 {
            return Opcode::GetSlot;
        }
        if v == Opcode::SetSlot as u8 {
            return Opcode::SetSlot;
        }
        if v == Opcode::AddI64 as u8 {
            return Opcode::AddI64;
        }
        if v == Opcode::SubI64 as u8 {
            return Opcode::SubI64;
        }
        if v == Opcode::MulI64 as u8 {
            return Opcode::MulI64;
        }
        if v == Opcode::DivI64 as u8 {
            return Opcode::DivI64;
        }
        if v == Opcode::ModI64 as u8 {
            return Opcode::ModI64;
        }
        if v == Opcode::AddI as u8 {
            return Opcode::AddI;
        }
        if v == Opcode::NegI64 as u8 {
            return Opcode::NegI64;
        }
        if v == Opcode::Concat as u8 {
            return Opcode::Concat;
        }
        if v == Opcode::Eq as u8 {
            return Opcode::Eq;
        }
        if v == Opcode::Lt as u8 {
            return Opcode::Lt;
        }
        if v == Opcode::Le as u8 {
            return Opcode::Le;
        }
        if v == Opcode::Jmp as u8 {
            return Opcode::Jmp;
        }
        if v == Opcode::JmpF as u8 {
            return Opcode::JmpF;
        }
        if v == Opcode::JmpT as u8 {
            return Opcode::JmpT;
        }
        if v == Opcode::NewArr as u8 {
            return Opcode::NewArr;
        }
        if v == Opcode::Len as u8 {
            return Opcode::Len;
        }
        if v == Opcode::GetElem as u8 {
            return Opcode::GetElem;
        }
        if v == Opcode::SetElem as u8 {
            return Opcode::SetElem;
        }
        if v == Opcode::CallK as u8 {
            return Opcode::CallK;
        }
        if v == Opcode::CallNat as u8 {
            return Opcode::CallNat;
        }
        if v == Opcode::Ret as u8 {
            return Opcode::Ret;
        }
        if v == Opcode::Ne as u8 {
            return Opcode::Ne;
        }
        if v == Opcode::AllocCell as u8 {
            return Opcode::AllocCell;
        }
        if v == Opcode::GetCell as u8 {
            return Opcode::GetCell;
        }
        if v == Opcode::SetCell as u8 {
            return Opcode::SetCell;
        }
        if v == Opcode::RefCell as u8 {
            return Opcode::RefCell;
        }
        if v == Opcode::RefElem as u8 {
            return Opcode::RefElem;
        }
        if v == Opcode::LoadRef as u8 {
            return Opcode::LoadRef;
        }
        if v == Opcode::StoreRef as u8 {
            return Opcode::StoreRef;
        }
        if v == Opcode::LoadFunc as u8 {
            return Opcode::LoadFunc;
        }
        if v == Opcode::Closure as u8 {
            return Opcode::Closure;
        }
        if v == Opcode::CapRef as u8 {
            return Opcode::CapRef;
        }
        if v == Opcode::GetUpval as u8 {
            return Opcode::GetUpval;
        }
        if v == Opcode::SetUpval as u8 {
            return Opcode::SetUpval;
        }
        if v == Opcode::Upval as u8 {
            return Opcode::Upval;
        }
        if v == Opcode::Call as u8 {
            return Opcode::Call;
        }
        if v == Opcode::CapUp as u8 {
            return Opcode::CapUp;
        }
        if v == Opcode::GetSlotCell as u8 {
            return Opcode::GetSlotCell;
        }
        unreachable!("invalid opcode byte {v} in {:#010x}", self.0)
    }

    pub fn a(self) -> u32 {
        (self.0 >> 16) & 0xff
    }

    pub fn b(self) -> u32 {
        (self.0 >> 8) & 0xff
    }

    pub fn c(self) -> u32 {
        self.0 & 0xff
    }

    pub fn bx(self) -> u16 {
        (self.0 & 0xffff) as u16
    }

    pub fn sbx(self) -> i16 {
        (self.0 & 0xffff) as u16 as i16
    }

    pub fn sj(self) -> i32 {
        ((self.0 & 0x00ff_ffff) << 8) as i32 >> 8
    }
}

/// 反汇编为文本（golden 快照锁定 ISA 与降级形态）。
pub fn dump_module(m: &Module) -> String {
    let mut w = String::new();
    if !m.consts.is_empty() {
        w.push_str("consts:\n");
        for (i, k) in m.consts.iter().enumerate() {
            writeln!(w, "  k{i} = {}", dump_const(k)).unwrap();
        }
    }
    for (i, f) in m.funcs.iter().enumerate() {
        if i > 0 || !m.consts.is_empty() {
            w.push('\n');
        }
        dump_func(f, m, &mut w);
    }
    w
}

fn dump_const(k: &xvalue::Value) -> String {
    match k {
        xvalue::Value::Obj(o) => match o.as_str() {
            Some(s) => format!("\"{s}\""),
            None => "<arr>".to_string(),
        },
        v => {
            let mut s = String::new();
            xvalue::write_value(v, &mut s).unwrap();
            s
        }
    }
}

pub fn dump_func(f: &FuncProto, m: &Module, w: &mut impl fmt::Write) {
    writeln!(w, "func {} (arity {}, regs {}):", f.name, f.arity, f.regs).unwrap();
    for (ip, instr) in f.code.iter().enumerate() {
        write!(w, "  {ip:04} ").unwrap();
        dump_instr(*instr, ip, m, w);
        writeln!(w).unwrap();
    }
}

fn dump_instr(instr: Instr, ip: usize, m: &Module, w: &mut impl fmt::Write) {
    let r = |n: u32| format!("r{n}");
    match instr.op() {
        Opcode::Move => write!(w, "MOVE {}, {}", r(instr.a()), r(instr.b())),
        Opcode::LoadI => write!(w, "LOADI {}, {}", r(instr.a()), instr.sbx()),
        Opcode::LoadK => write!(w, "LOADK {}, k{}", r(instr.a()), instr.bx()),
        Opcode::LoadNil => write!(w, "LOADNIL {}", r(instr.a())),
        Opcode::LoadB => write!(w, "LOADB {}, {}", r(instr.a()), instr.b()),
        Opcode::GetSlot => write!(w, "GETSLOT {}, s{}", r(instr.a()), instr.bx()),
        Opcode::SetSlot => write!(w, "SETSLOT {}, s{}", r(instr.a()), instr.bx()),
        Opcode::AddI64 => {
            write!(w, "ADDI64 {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::SubI64 => {
            write!(w, "SUBI64 {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::MulI64 => {
            write!(w, "MULI64 {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::DivI64 => {
            write!(w, "DIVI64 {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::ModI64 => {
            write!(w, "MODI64 {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::AddI => write!(w, "ADDI {}, {}", r(instr.a()), instr.sbx()),
        Opcode::NegI64 => write!(w, "NEGI64 {}, {}", r(instr.a()), r(instr.b())),
        Opcode::Concat => {
            write!(w, "CONCAT {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::Eq => write!(w, "EQ {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c())),
        Opcode::Ne => write!(w, "NE {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c())),
        Opcode::Lt => write!(w, "LT {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c())),
        Opcode::Le => write!(w, "LE {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c())),
        Opcode::Jmp => write!(w, "JMP -> {}", target(instr, ip)),
        Opcode::JmpF => write!(w, "JMPF {}, -> {}", r(instr.a()), target16(instr, ip)),
        Opcode::JmpT => write!(w, "JMPT {}, -> {}", r(instr.a()), target16(instr, ip)),
        Opcode::NewArr => {
            write!(w, "NEWARR {}, {}, {}", r(instr.a()), r(instr.b()), instr.c())
        }
        Opcode::Len => write!(w, "LEN {}, {}", r(instr.a()), r(instr.b())),
        Opcode::GetElem => {
            write!(w, "GETELEM {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::SetElem => {
            write!(w, "SETELEM {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::CallK => {
            let name = m
                .funcs
                .get(instr.b() as usize)
                .map(|f| f.name.as_str())
                .unwrap_or("?");
            write!(w, "CALLK {}, {}, {}", r(instr.a()), name, instr.c())
        }
        Opcode::CallNat => {
            write!(w, "CALLNAT {}, {}, {}", r(instr.a()), instr.b(), instr.c())
        }
        Opcode::Ret => write!(w, "RET {}", r(instr.a())),
        Opcode::AllocCell => {
            write!(w, "ALLOCCELL {}, {}", r(instr.a()), r(instr.b()))
        }
        Opcode::GetCell => write!(w, "GETCELL {}, {}", r(instr.a()), r(instr.b())),
        Opcode::SetCell => write!(w, "SETCELL {}, {}", r(instr.a()), r(instr.b())),
        Opcode::RefCell => write!(w, "REFCELL {}, {}", r(instr.a()), r(instr.b())),
        Opcode::RefElem => {
            write!(w, "REFELEM {}, {}, {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        Opcode::LoadRef => write!(w, "LOADREF {}, {}", r(instr.a()), r(instr.b())),
        Opcode::StoreRef => write!(w, "STOREREF {}, {}", r(instr.a()), r(instr.b())),
        Opcode::LoadFunc => write!(w, "LOADFUNC {}, {}", r(instr.a()), instr.bx()),
        Opcode::Closure => {
            write!(w, "CLOSURE {}, func{}", r(instr.a()), instr.bx())
        }
        Opcode::CapRef => write!(w, "CAPREF {}", r(instr.b())),
        Opcode::GetUpval => write!(w, "GETUPV {}, u{}", r(instr.a()), instr.b()),
        Opcode::SetUpval => write!(w, "SETUPV {}, u{}", r(instr.a()), instr.b()),
        Opcode::Upval => write!(w, "UPVAL {}, u{}", r(instr.a()), instr.b()),
        Opcode::Call => write!(w, "CALL {}, argc={}", r(instr.a()), instr.c()),
        Opcode::CapUp => write!(w, "CAPUP u{}", instr.b()),
        Opcode::GetSlotCell => write!(w, "GETSLOTCELL {}, s{}", r(instr.a()), instr.bx()),
    }
    .unwrap();
}

fn target(instr: Instr, ip: usize) -> usize {
    (ip as i64 + 1 + instr.sj() as i64) as usize
}

fn target16(instr: Instr, ip: usize) -> usize {
    (ip as i64 + 1 + instr.sbx() as i64) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abc_roundtrip() {
        let i = Instr::abc(Opcode::AddI64, 3, 200, 255);
        assert_eq!(i.op(), Opcode::AddI64);
        assert_eq!(i.a(), 3);
        assert_eq!(i.b(), 200);
        assert_eq!(i.c(), 255);
    }

    #[test]
    fn abx_asbx_roundtrip() {
        let i = Instr::abx(Opcode::LoadK, 7, 0xbeef);
        assert_eq!(i.op(), Opcode::LoadK);
        assert_eq!(i.a(), 7);
        assert_eq!(i.bx(), 0xbeef);

        let i = Instr::asbx(Opcode::LoadI, 7, -32768);
        assert_eq!(i.sbx(), -32768);
    }

    #[test]
    fn sj_sign_extension() {
        for off in [-8388608, -1, 0, 1, 8388607] {
            let i = Instr::new_sj(Opcode::Jmp, off);
            assert_eq!(i.sj(), off);
        }
    }
}
