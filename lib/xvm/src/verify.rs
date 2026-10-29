//! 结构校验：寄存器号越界、跳转必须落在函数内、索引有效、捕获描述只在
//! CLOSURE 窗口内出现。模块加载时先过此处，畸形字节码在进入执行前拒绝。

use crate::isa::Opcode;
use crate::lower::Module;

pub fn verify(m: &Module) -> Result<(), String> {
    for f in &m.funcs {
        if f.code.len() != f.lines.len() {
            return Err(format!(
                "verify: func {} line table out of sync ({} vs {})",
                f.name,
                f.code.len(),
                f.lines.len()
            ));
        }
        let mut ip = 0usize;
        while ip < f.code.len() {
            let instr = f.code[ip];
            // CLOSURE 的捕获描述伪指令随 CLOSURE 一并校验并跳过。
            if instr.op() == Opcode::Closure {
                if instr.a() as usize >= f.regs as usize {
                    return Err(format!("verify: func {} ip {ip}: register out of range", f.name));
                }
                if instr.bx() as usize >= m.funcs.len() {
                    return Err(format!(
                        "verify: func {} ip {ip}: function index out of range",
                        f.name
                    ));
                }
                let mut caps = 0usize;
                while ip + 1 + caps < f.code.len()
                    && matches!(
                        f.code[ip + 1 + caps].op(),
                        Opcode::CapRef | Opcode::CapUp
                    )
                {
                    let pseudo = f.code[ip + 1 + caps];
                    match pseudo.op() {
                        Opcode::CapRef => {
                            if pseudo.b() as usize >= f.regs as usize {
                                return Err(format!(
                                    "verify: func {} ip {}: CAPREF register out of range",
                                    f.name,
                                    ip + 1 + caps
                                ));
                            }
                        }
                        _ => {
                            if pseudo.b() as usize >= f.upvals as usize {
                                return Err(format!(
                                    "verify: func {} ip {}: CAPUP index out of range",
                                    f.name,
                                    ip + 1 + caps
                                ));
                            }
                        }
                    }
                    caps += 1;
                }
                ip += 1 + caps;
                continue;
            }
            let bad = |msg: String| {
                Err(format!(
                    "verify: func {} ip {ip} ({:?}): {}",
                    f.name,
                    instr.op(),
                    msg
                ))
            };
            let reg = |r: u32| (r as usize) < f.regs as usize;
            let need = |regs: &[u32]| regs.iter().all(|&r| reg(r));
            match instr.op() {
                Opcode::Move | Opcode::NegI64 => {
                    if !need(&[instr.a(), instr.b()]) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::LoadI | Opcode::LoadK | Opcode::GetSlot | Opcode::GetSlotCell => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    if matches!(instr.op(), Opcode::GetSlot | Opcode::GetSlotCell)
                        && instr.bx() as usize >= m.num_slots
                    {
                        return bad("slot index out of range".to_string());
                    }
                }
                Opcode::LoadNil | Opcode::Ret => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::LoadB => {
                    if !reg(instr.a()) || instr.b() > 1 {
                        return bad("LOADB operands invalid".to_string());
                    }
                }
                Opcode::SetSlot => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    if instr.bx() as usize >= m.num_slots {
                        return bad("slot index out of range".to_string());
                    }
                }
                Opcode::AddI64 | Opcode::SubI64 | Opcode::MulI64 | Opcode::DivI64
                | Opcode::ModI64 | Opcode::Concat | Opcode::Eq | Opcode::Ne | Opcode::Lt
                | Opcode::Le | Opcode::GetElem | Opcode::SetElem => {
                    if !need(&[instr.a(), instr.b(), instr.c()]) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::AddI => {
                    if !need(&[instr.a(), instr.b()]) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::NewArr => {
                    let (base, count) = (instr.b() as usize, instr.c() as usize);
                    if !reg(instr.a()) || base + count > f.regs as usize {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::Len => {
                    if !need(&[instr.a(), instr.b()]) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::CallK => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    let callee = match m.funcs.get(instr.b() as usize) {
                        Some(c) => c,
                        None => return bad("function index out of range".to_string()),
                    };
                    if instr.c() != callee.arity {
                        return bad(format!(
                            "arity mismatch: {} args for {} (arity {})",
                            instr.c(),
                            callee.name,
                            callee.arity
                        ));
                    }
                }
                Opcode::CallNat => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    if instr.b() as usize >= m.natives.len() {
                        return bad("native index out of range".to_string());
                    }
                }
                Opcode::Jmp => {
                    let target = ip as i64 + 1 + instr.sj() as i64;
                    if target < 0 || target as usize > f.code.len() {
                        return bad("jump target out of range".to_string());
                    }
                }
                Opcode::JmpF | Opcode::JmpT => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    let target = ip as i64 + 1 + instr.sbx() as i64;
                    if target < 0 || target as usize > f.code.len() {
                        return bad("jump target out of range".to_string());
                    }
                }
                Opcode::AllocCell | Opcode::GetCell | Opcode::SetCell | Opcode::RefCell
                | Opcode::LoadRef | Opcode::StoreRef | Opcode::GetUpval | Opcode::SetUpval
                | Opcode::Upval => {
                    if !need(&[instr.a(), instr.b()]) {
                        return bad("register out of range".to_string());
                    }
                    if matches!(
                        instr.op(),
                        Opcode::GetUpval | Opcode::SetUpval | Opcode::Upval
                    ) && instr.b() >= f.upvals
                    {
                        return bad("upval index out of range".to_string());
                    }
                }
                Opcode::RefElem => {
                    if !need(&[instr.a(), instr.b(), instr.c()]) {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::LoadFunc => {
                    if !reg(instr.a()) {
                        return bad("register out of range".to_string());
                    }
                    if instr.bx() as usize >= m.funcs.len() {
                        return bad("function index out of range".to_string());
                    }
                }
                Opcode::Call => {
                    let a = instr.a() as usize;
                    if !reg(instr.a()) || a + 1 + instr.c() as usize > f.regs as usize {
                        return bad("register out of range".to_string());
                    }
                }
                Opcode::Closure | Opcode::CapRef | Opcode::CapUp => {
                    unreachable!("handled above")
                }
            }
            ip += 1;
        }
    }
    Ok(())
}
