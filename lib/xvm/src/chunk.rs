use xvalue::Value;

use crate::op::Opcode;

#[derive(Clone, Copy, Debug)]
pub struct Instr {
    pub op: Opcode,
    pub operand: i32,
    pub pos: u32,
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub name: String,
    pub code: Vec<Instr>,
    pub constants: Vec<Value>,
    pub arity: u32,
    pub num_locals: u32,
}

impl Chunk {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            code: Vec::new(),
            constants: vec![Value::Void],
            arity: 0,
            num_locals: 0,
        }
    }

    pub fn emit(&mut self, op: Opcode, operand: i32, pos: u32) {
        self.code.push(Instr { op, operand, pos });
    }

    pub fn add_const(&mut self, v: Value) -> i32 {
        self.constants.push(v);
        (self.constants.len() - 1) as i32
    }

    pub fn emit_const(&mut self, v: Value, pos: u32) {
        let idx = self.add_const(v);
        self.emit(Opcode::Const, idx, pos);
    }

    pub fn emit_jump(&mut self, op: Opcode, pos: u32) -> usize {
        self.code.push(Instr { op, operand: 0, pos });
        self.code.len() - 1
    }

    pub fn patch_jump(&mut self, jump: usize) {
        let offset = (self.code.len() as i32) - (jump as i32 + 1);
        self.code[jump].operand = offset;
    }
}
