//! xvm：X 的寄存器字节码解释器（ISA v0.2，设计见 vm-bytecode-design-2026-10.md）。
//!
//! 模块结构：`isa`（编码/反汇编）、`lower`（AST → 字节码）、`verify`（结构校验）、
//! `interp`（执行循环）。堆与值表示在 `xvalue`。

mod interp;
pub mod isa;
mod lower;
mod verify;

use xc_check::Program;

pub use interp::run;
pub use isa::{dump_module, Instr, Opcode};
pub use lower::{FuncProto, Module};

/// 编译 → 结构校验。校验器同时充当降级器的差分裁判：降级产出畸形字节码
/// 在此即刻暴露，而不是在解释器深处。
pub fn compile(program: &Program) -> Result<Module, String> {
    let m = lower::compile(program)?;
    verify::verify(&m)?;
    Ok(m)
}
