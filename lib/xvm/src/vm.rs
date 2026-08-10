use std::collections::HashMap;

use xvalue::{write_value, Value};

use crate::chunk::Chunk;
use crate::op::Opcode;

struct Frame {
    chunk: usize,
    ip: usize,
    base: usize,
}

pub fn run(chunks: &[Chunk], source: &str) -> Result<String, String> {
    // Reserve the top-level chunk's local slots (top-level for-loops use them).
    let top_num_locals = chunks[0].num_locals as usize;
    let mut stack: Vec<Value> = std::iter::repeat(Value::Void)
        .take(top_num_locals)
        .collect();
    let mut globals: HashMap<String, Value> = HashMap::new();
    let mut frames: Vec<Frame> = vec![Frame {
        chunk: 0,
        ip: 0,
        base: 0,
    }];
    let mut output = String::new();

    macro_rules! rt {
        ($instr:expr, $msg:expr) => {
            return Err(format!(
                "runtime error: {} at {} (line {})",
                $msg,
                chunks[frames[frames.len() - 1].chunk].name,
                line_of(source, $instr.pos)
            ))
        };
    }

    loop {
        let frame_idx = frames.len() - 1;
        let chunk = &chunks[frames[frame_idx].chunk];
        let instr = chunk.code[frames[frame_idx].ip];
        frames[frame_idx].ip += 1;

        match instr.op {
            Opcode::Const => {
                let v = chunk.constants[instr.operand as usize].clone();
                stack.push(v);
            }
            Opcode::LoadLocal => {
                let slot = frames[frame_idx].base + instr.operand as usize;
                stack.push(stack[slot].clone());
            }
            Opcode::StoreLocal => {
                let slot = frames[frame_idx].base + instr.operand as usize;
                let v = stack.pop().unwrap();
                stack[slot] = v;
            }
            Opcode::LoadGlobal => {
                let name = match &chunk.constants[instr.operand as usize] {
                    Value::String(s) => s.clone(),
                    _ => rt!(instr,"internal: global name constant is not a string"),
                };
                match globals.get(&name) {
                    Some(v) => stack.push(v.clone()),
                    None => rt!(instr, format!("undefined variable: {name}")),
                }
            }
            Opcode::StoreGlobal => {
                let name = match &chunk.constants[instr.operand as usize] {
                    Value::String(s) => s.clone(),
                    _ => rt!(instr,"internal: global name constant is not a string"),
                };
                let v = stack.pop().unwrap();
                globals.insert(name, v);
            }
            Opcode::Pop => {
                stack.pop();
            }
            Opcode::Dup => {
                let v = stack.last().unwrap().clone();
                stack.push(v);
            }
            Opcode::Arr => {
                let n = instr.operand as usize;
                let items = stack.split_off(stack.len() - n);
                stack.push(Value::Array(items));
            }
            Opcode::ArrLen => {
                let v = stack.pop().unwrap();
                match v {
                    Value::Array(items) => stack.push(Value::Int(items.len() as i128)),
                    _ => rt!(instr,"length of a non-array"),
                }
            }
            Opcode::ArrIndex => {
                let idx = stack.pop().unwrap();
                let arr = stack.pop().unwrap();
                match (arr, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        if i < 0 || i as usize >= items.len() {
                            rt!(instr, format!(
                                "array index {i} out of bounds (len {})",
                                items.len()
                            ));
                        }
                        stack.push(items[i as usize].clone());
                    }
                    _ => rt!(instr,"array index on a non-array"),
                }
            }
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        let v = match instr.op {
                            Opcode::Div => {
                                if b == 0 {
                                    rt!(instr, "division by zero");
                                }
                                a / b
                            }
                            Opcode::Mod => {
                                if b == 0 {
                                    rt!(instr, "modulo by zero");
                                }
                                a % b
                            }
                            _ => match instr.op {
                                Opcode::Add => a + b,
                                Opcode::Sub => a - b,
                                Opcode::Mul => a * b,
                                _ => unreachable!(),
                            },
                        };
                        stack.push(Value::Int(v));
                    }
                    (Value::String(a), Value::String(b)) if instr.op == Opcode::Add => {
                        stack.push(Value::String(format!("{a}{b}")));
                    }
                    _ => rt!(instr,"arithmetic operands must be int (string only for +)"),
                }
            }
            Opcode::Neg => {
                let v = stack.pop().unwrap();
                match v {
                    Value::Int(a) => stack.push(Value::Int(-a)),
                    _ => rt!(instr,"negation operand must be int"),
                }
            }
            Opcode::Eq | Opcode::Ne | Opcode::Lt | Opcode::Gt | Opcode::Le | Opcode::Ge => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                let b = match (instr.op, &l, &r) {
                    (Opcode::Eq, a, b) => a == b,
                    (Opcode::Ne, a, b) => a != b,
                    (Opcode::Lt, Value::Int(a), Value::Int(b)) => a < b,
                    (Opcode::Gt, Value::Int(a), Value::Int(b)) => a > b,
                    (Opcode::Le, Value::Int(a), Value::Int(b)) => a <= b,
                    (Opcode::Ge, Value::Int(a), Value::Int(b)) => a >= b,
                    (Opcode::Lt | Opcode::Gt | Opcode::Le | Opcode::Ge, _, _) => {
                        rt!(instr, "ordering operands must be int")
                    }
                    _ => rt!(instr,"comparison operands mismatch"),
                };
                stack.push(Value::Bool(b));
            }
            Opcode::Jmp => {
                frames[frame_idx].ip = (frames[frame_idx].ip as i32 + instr.operand) as usize;
            }
            Opcode::JmpIfFalse => {
                let cond = stack.pop().unwrap();
                match cond {
                    Value::Bool(false) => {
                        frames[frame_idx].ip =
                            (frames[frame_idx].ip as i32 + instr.operand) as usize;
                    }
                    Value::Bool(true) => {}
                    _ => rt!(instr,"condition must be bool"),
                }
            }
            Opcode::Call => {
                let f = instr.operand as usize;
                let callee = &chunks[f];
                let arity = callee.arity as usize;
                if stack.len() < arity {
                    rt!(instr, format!("internal: call to {} with too few args", callee.name));
                }
                let base = stack.len() - arity;
                while stack.len() < base + callee.num_locals as usize {
                    stack.push(Value::Void);
                }
                frames.push(Frame { chunk: f, ip: 0, base });
            }
            Opcode::Ret => {
                let ret = stack.pop().unwrap();
                let frame = frames.pop().unwrap();
                stack.truncate(frame.base);
                if frames.is_empty() {
                    return Ok(output);
                }
                stack.push(ret);
            }
            Opcode::Print => {
                let n = instr.operand as usize;
                let mut vals: Vec<Value> = Vec::with_capacity(n);
                for _ in 0..n {
                    vals.push(stack.pop().unwrap());
                }
                vals.reverse();
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 {
                        output.push(' ');
                    }
                    write_value(v, &mut output).map_err(|_| "print failed".to_string())?;
                }
                output.push('\n');
                // print evaluates to void, so a value remains for the enclosing statement.
                stack.push(Value::Void);
            }
        }
    }
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
    use crate::chunk::Chunk;
    use crate::op::Opcode;
    use xvalue::Value;

    #[test]
    fn vm_runs_hand_built_chunk() {
        // program: print(2 + 3)
        let mut c = Chunk::new("<test>");
        let two = c.add_const(Value::Int(2));
        let three = c.add_const(Value::Int(3));
        c.emit(Opcode::Const, two, 0);
        c.emit(Opcode::Const, three, 0);
        c.emit(Opcode::Add, 0, 0);
        c.emit(Opcode::Print, 1, 0);
        c.emit(Opcode::Ret, 0, 0);
        let out = run(&[c], "2 + 3").unwrap();
        assert_eq!(out, "5\n");
    }
}
