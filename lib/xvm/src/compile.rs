use std::collections::HashMap;

use xc_ast::expr::{Argument, Expr, ExprItem, ExprKind};
use xc_ast::func::{Func, FuncBody};
use xc_ast::pattern::PatternKind;
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_check::Program;
use xc_span::symbol::op;
use xc_span::Symbol;
use xvalue::Value;

use crate::chunk::Chunk;
use crate::op::Opcode;

pub fn compile(program: &Program) -> Result<Vec<Chunk>, String> {
    let mut compiler = Compiler {
        func_indices: &program.func_indices,
        loop_stack: Vec::new(),
        chunks: Vec::new(),
    };
    compiler.compile_top(program)?;
    for f in &program.funcs {
        compiler.compile_func(f)?;
    }
    Ok(compiler.chunks)
}

struct LoopInfo {
    break_positions: Vec<usize>,
    continue_to: usize,
}

struct Ctx {
    locals: HashMap<Symbol, u32>,
    next_local: u32,
}

impl Ctx {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
        }
    }
    fn resolve(&self, name: Symbol) -> Option<u32> {
        self.locals.get(&name).copied()
    }
    fn declare(&mut self, name: Symbol) -> u32 {
        let slot = self.next_local;
        self.locals.insert(name, slot);
        self.next_local += 1;
        slot
    }
    fn alloc_temp(&mut self) -> u32 {
        let slot = self.next_local;
        self.next_local += 1;
        slot
    }
}

struct Compiler<'p> {
    func_indices: &'p HashMap<Symbol, u32>,
    loop_stack: Vec<LoopInfo>,
    chunks: Vec<Chunk>,
}

impl<'p> Compiler<'p> {
    fn compile_top(&mut self, program: &Program) -> Result<(), String> {
        let mut chunk = Chunk::new("<top>");
        let mut ctx = Ctx::new();
        self.compile_stmts(&mut chunk, &mut ctx, &program.top, true)?;
        chunk.num_locals = ctx.next_local;
        let pos = program.top.last().map(|s| s.span.lo.0).unwrap_or(0);
        chunk.emit(Opcode::Ret, 0, pos);
        self.chunks.push(chunk);
        Ok(())
    }

    fn compile_func(&mut self, f: &P<Func>) -> Result<(), String> {
        let mut chunk = Chunk::new(f.ident.as_str());
        let mut ctx = Ctx::new();
        for param in &f.sig.params {
            ctx.declare(param.ident.name);
        }
        chunk.arity = f.sig.params.len() as u32;
        match &f.body {
            FuncBody::Block(block) => self.compile_block(&mut chunk, &mut ctx, block, false)?,
            FuncBody::Arrow(expr) => self.compile_expr(&mut chunk, &mut ctx, expr, false)?,
            FuncBody::Semicolon => {
                return Err(format!("function without body: {}", f.ident.as_str()))
            }
        }
        chunk.num_locals = ctx.next_local;
        chunk.emit(Opcode::Ret, 0, 0);
        self.chunks.push(chunk);
        Ok(())
    }

    fn compile_stmts(
        &mut self,
        chunk: &mut Chunk,
        ctx: &mut Ctx,
        stmts: &[Stmt],
        is_top: bool,
    ) -> Result<(), String> {
        if stmts.is_empty() {
            chunk.emit_const(Value::Void, 0);
            return Ok(());
        }
        let n = stmts.len();
        for (i, stmt) in stmts.iter().enumerate() {
            let last = i + 1 == n;
            let terminated = self.compile_stmt(chunk, ctx, stmt, is_top, last)?;
            if !last && !terminated {
                chunk.emit(Opcode::Pop, 0, stmt.span.lo.0);
            }
        }
        Ok(())
    }

    /// Compiles one statement. Returns `true` if the statement terminated control
    /// (a `return`), in which case no value is left on the stack.
    fn compile_stmt(
        &mut self,
        chunk: &mut Chunk,
        ctx: &mut Ctx,
        stmt: &Stmt,
        is_top: bool,
        _last: bool,
    ) -> Result<bool, String> {
        let pos = stmt.span.lo.0;
        match &stmt.kind {
            StmtKind::Empty => {
                chunk.emit_const(Value::Void, pos);
                Ok(false)
            }
            StmtKind::Expr(e) | StmtKind::ExprSemi(e) => {
                self.compile_expr(chunk, ctx, e, is_top)?;
                Ok(false)
            }
            StmtKind::Let(pat, init) => {
                let name = match &pat.kind {
                    PatternKind::Bind(ident, _) => ident.name,
                    _ => return Err("unsupported pattern in let".to_string()),
                };
                match init {
                    Some(init) => self.compile_expr(chunk, ctx, init, is_top)?,
                    None => chunk.emit_const(Value::Void, pos),
                }
                self.emit_store(chunk, ctx, name, is_top, pos)?;
                // A let statement evaluates to void.
                chunk.emit_const(Value::Void, pos);
                Ok(false)
            }
        }
    }

    fn compile_block(
        &mut self,
        chunk: &mut Chunk,
        ctx: &mut Ctx,
        block: &Block,
        is_top: bool,
    ) -> Result<(), String> {
        self.compile_stmts(chunk, ctx, &block.stmts, is_top)
    }

    fn compile_expr(
        &mut self,
        chunk: &mut Chunk,
        ctx: &mut Ctx,
        expr: &Expr,
        is_top: bool,
    ) -> Result<(), String> {
        use ExprKind::*;
        let pos = expr.span.lo.0;
        match &expr.kind {
            Literal(lit) => {
                let v = Value::from_lit(lit)?;
                chunk.emit_const(v, pos);
            }
            Nil => {
                chunk.emit_const(Value::Nil, pos);
            }
            Paren(inner) => self.compile_expr(chunk, ctx, inner, is_top)?,
            Path(p) if p.segments.len() == 1 => {
                let name = p.segments[0].ident.name;
                self.emit_load(chunk, ctx, name, is_top, pos)?;
            }
            Path(_) => return Err("qualified paths not supported".to_string()),
            Block(b) => self.compile_block(chunk, ctx, b, is_top)?,
            Array(items) => {
                let mut count = 0i32;
                for item in items {
                    match item {
                        ExprItem::Expr(e) => {
                            self.compile_expr(chunk, ctx, e, is_top)?;
                            count += 1;
                        }
                        _ => return Err("expansion in array literal not supported".to_string()),
                    }
                }
                chunk.emit(Opcode::Arr, count, pos);
            }
            If(cond, then, else_) => {
                self.compile_expr(chunk, ctx, cond, is_top)?;
                let jmp_false = chunk.emit_jump(Opcode::JmpIfFalse, pos);
                self.compile_expr(chunk, ctx, then, is_top)?;
                let jmp_end = chunk.emit_jump(Opcode::Jmp, pos);
                chunk.patch_jump(jmp_false);
                if let Some(e) = else_ {
                    self.compile_expr(chunk, ctx, e, is_top)?;
                } else {
                    chunk.emit_const(Value::Void, pos);
                }
                chunk.patch_jump(jmp_end);
            }
            While(cond, body, else_block, _label) => {
                let loop_start = chunk.code.len();
                self.compile_expr(chunk, ctx, cond, is_top)?;
                let jmp_false = chunk.emit_jump(Opcode::JmpIfFalse, pos);
                self.loop_stack.push(LoopInfo {
                    break_positions: Vec::new(),
                    continue_to: loop_start,
                });
                self.compile_expr(chunk, ctx, body, is_top)?;
                chunk.emit(Opcode::Pop, 0, pos);
                let back = chunk.emit_jump(Opcode::Jmp, pos);
                let offset = (loop_start as i32) - (back as i32 + 1);
                chunk.code[back].operand = offset;
                let info = self.loop_stack.pop().unwrap();
                chunk.patch_jump(jmp_false);
                if let Some(el) = else_block {
                    self.compile_expr(chunk, ctx, el, is_top)?;
                } else {
                    chunk.emit_const(Value::Void, pos);
                }
                let end = chunk.code.len();
                for bp in &info.break_positions {
                    chunk.code[*bp].operand = (end as i32) - (*bp as i32 + 1);
                }
            }
            For(pat, iter, body, else_block, _kind, _label) => {
                if else_block.is_some() {
                    return Err("for-else is not supported yet".to_string());
                }
                let var_slot = match &pat.kind {
                    PatternKind::Bind(ident, _) => ctx.declare(ident.name),
                    _ => return Err("unsupported pattern in for loop".to_string()),
                };
                let temp_arr = ctx.alloc_temp();
                let temp_i = ctx.alloc_temp();
                self.compile_expr(chunk, ctx, iter, is_top)?;
                chunk.emit(Opcode::StoreLocal, temp_arr as i32, pos);
                chunk.emit_const(Value::Int(0), pos);
                chunk.emit(Opcode::StoreLocal, temp_i as i32, pos);
                let loop_start = chunk.code.len();
                chunk.emit(Opcode::LoadLocal, temp_i as i32, pos);
                chunk.emit(Opcode::LoadLocal, temp_arr as i32, pos);
                chunk.emit(Opcode::ArrLen, 0, pos);
                chunk.emit(Opcode::Lt, 0, pos);
                let jmp_false = chunk.emit_jump(Opcode::JmpIfFalse, pos);
                chunk.emit(Opcode::LoadLocal, temp_arr as i32, pos);
                chunk.emit(Opcode::LoadLocal, temp_i as i32, pos);
                chunk.emit(Opcode::ArrIndex, 0, pos);
                chunk.emit(Opcode::StoreLocal, var_slot as i32, pos);
                self.loop_stack.push(LoopInfo {
                    break_positions: Vec::new(),
                    continue_to: 0,
                });
                self.compile_block(chunk, ctx, body, is_top)?;
                chunk.emit(Opcode::Pop, 0, pos);
                let increment_pos = chunk.code.len();
                self.loop_stack.last_mut().unwrap().continue_to = increment_pos;
                chunk.emit(Opcode::LoadLocal, temp_i as i32, pos);
                chunk.emit_const(Value::Int(1), pos);
                chunk.emit(Opcode::Add, 0, pos);
                chunk.emit(Opcode::StoreLocal, temp_i as i32, pos);
                let back = chunk.emit_jump(Opcode::Jmp, pos);
                chunk.code[back].operand = (loop_start as i32) - (back as i32 + 1);
                let info = self.loop_stack.pop().unwrap();
                chunk.patch_jump(jmp_false);
                chunk.emit_const(Value::Void, pos);
                let end = chunk.code.len();
                for bp in &info.break_positions {
                    chunk.code[*bp].operand = (end as i32) - (*bp as i32 + 1);
                }
            }
            Return(val) => {
                match val {
                    Some(v) => self.compile_expr(chunk, ctx, v, is_top)?,
                    None => chunk.emit_const(Value::Void, pos),
                }
                chunk.emit(Opcode::Ret, 0, pos);
            }
            Break(val, _) => {
                if val.is_some() {
                    return Err("break with a value is not supported yet".to_string());
                }
                let info = self
                    .loop_stack
                    .last_mut()
                    .ok_or_else(|| "break outside loop".to_string())?;
                let bp = chunk.emit_jump(Opcode::Jmp, pos);
                info.break_positions.push(bp);
            }
            Continue(_label) => {
                let continue_to = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| "continue outside loop".to_string())?
                    .continue_to;
                let jmp = chunk.emit_jump(Opcode::Jmp, pos);
                chunk.code[jmp].operand = (continue_to as i32) - (jmp as i32 + 1);
            }
            Infix(sym, exprs) => {
                if *sym == op::Assign {
                    if exprs.len() != 2 {
                        return Err("assignment requires two operands".to_string());
                    }
                    let target = &exprs[0];
                    let value = &exprs[1];
                    let name = match &target.kind {
                        ExprKind::Path(p) if p.segments.len() == 1 => p.segments[0].ident.name,
                        _ => return Err("assignment target must be a simple variable".to_string()),
                    };
                    self.compile_expr(chunk, ctx, value, is_top)?;
                    chunk.emit(Opcode::Dup, 0, pos);
                    self.emit_store(chunk, ctx, name, is_top, pos)?;
                    return Ok(());
                }
                if *sym == op::AndAnd || *sym == op::OrOr {
                    if exprs.len() != 2 {
                        return Err("&& / || require two operands".to_string());
                    }
                    // a && b  =>  a false ? false : b
                    // a || b  =>  a false ? b     : true
                    self.compile_expr(chunk, ctx, &exprs[0], is_top)?;
                    let short_jmp = chunk.emit_jump(Opcode::JmpIfFalse, pos);
                    if *sym == op::AndAnd {
                        self.compile_expr(chunk, ctx, &exprs[1], is_top)?;
                        let end_jmp = chunk.emit_jump(Opcode::Jmp, pos);
                        chunk.patch_jump(short_jmp);
                        chunk.emit_const(Value::Bool(false), pos);
                        chunk.patch_jump(end_jmp);
                    } else {
                        chunk.emit_const(Value::Bool(true), pos);
                        let end_jmp = chunk.emit_jump(Opcode::Jmp, pos);
                        chunk.patch_jump(short_jmp);
                        self.compile_expr(chunk, ctx, &exprs[1], is_top)?;
                        chunk.patch_jump(end_jmp);
                    }
                    return Ok(());
                }
                let opcode = opcode_for(*sym)
                    .ok_or_else(|| format!("unsupported operator: {}", sym.as_str()))?;
                let mut iter = exprs.iter();
                let first = iter.next().expect("infix with no operands");
                self.compile_expr(chunk, ctx, first, is_top)?;
                for e in iter {
                    self.compile_expr(chunk, ctx, e, is_top)?;
                    chunk.emit(opcode, 0, pos);
                }
            }
            Comparison(syms, exprs) => {
                if exprs.len() != 2 {
                    return Err("chained comparisons are not supported yet".to_string());
                }
                let opcode = opcode_for(syms[0])
                    .ok_or_else(|| format!("unsupported comparison: {}", syms[0].as_str()))?;
                self.compile_expr(chunk, ctx, &exprs[0], is_top)?;
                self.compile_expr(chunk, ctx, &exprs[1], is_top)?;
                chunk.emit(opcode, 0, pos);
            }
            Prefix(sym, inner) => {
                self.compile_expr(chunk, ctx, inner, is_top)?;
                if *sym == op::Minus {
                    chunk.emit(Opcode::Neg, 0, pos);
                } else if *sym != op::Plus {
                    return Err(format!("unsupported prefix operator: {}", sym.as_str()));
                }
            }
            FuncCall(callee, args, _) => {
                let name = match &callee.kind {
                    ExprKind::Path(p) if p.segments.len() == 1 => p.segments[0].ident.name,
                    _ => return Err("only simple function calls are supported".to_string()),
                };
                if name.as_str() == "print" {
                    let mut arity = 0i32;
                    for arg in &args.0 {
                        match arg {
                            Argument::Ordinal(e) => {
                                self.compile_expr(chunk, ctx, e, is_top)?;
                                arity += 1;
                            }
                            _ => {
                                return Err(
                                    "named / expansion arguments not supported yet".to_string()
                                )
                            }
                        }
                    }
                    chunk.emit(Opcode::Print, arity, pos);
                } else {
                    let idx = *self
                        .func_indices
                        .get(&name)
                        .ok_or_else(|| format!("undefined function: {}", name.as_str()))?;
                    for arg in &args.0 {
                        match arg {
                            Argument::Ordinal(e) => self.compile_expr(chunk, ctx, e, is_top)?,
                            _ => {
                                return Err(
                                    "named / expansion arguments not supported yet".to_string()
                                )
                            }
                        }
                    }
                    chunk.emit(Opcode::Call, idx as i32, pos);
                }
            }
            other => {
                return Err(format!("unsupported expression in this build: {other:?}"));
            }
        }
        Ok(())
    }

    fn emit_load(
        &mut self,
        chunk: &mut Chunk,
        ctx: &Ctx,
        name: Symbol,
        _is_top: bool,
        pos: u32,
    ) -> Result<(), String> {
        if let Some(slot) = ctx.resolve(name) {
            chunk.emit(Opcode::LoadLocal, slot as i32, pos);
            return Ok(());
        }
        let idx = chunk.add_const(Value::String(name.as_str().to_string()));
        chunk.emit(Opcode::LoadGlobal, idx, pos);
        Ok(())
    }

    fn emit_store(
        &mut self,
        chunk: &mut Chunk,
        ctx: &mut Ctx,
        name: Symbol,
        is_top: bool,
        pos: u32,
    ) -> Result<(), String> {
        if !is_top {
            let slot = match ctx.resolve(name) {
                Some(slot) => slot,
                None => ctx.declare(name),
            };
            chunk.emit(Opcode::StoreLocal, slot as i32, pos);
        } else {
            let idx = chunk.add_const(Value::String(name.as_str().to_string()));
            chunk.emit(Opcode::StoreGlobal, idx, pos);
        }
        Ok(())
    }
}

fn opcode_for(sym: Symbol) -> Option<Opcode> {
    use op::*;
    Some(match sym {
        Plus => Opcode::Add,
        Minus => Opcode::Sub,
        Multiply => Opcode::Mul,
        Divide => Opcode::Div,
        Modulo => Opcode::Mod,
        Equal => Opcode::Eq,
        NotEqual => Opcode::Ne,
        Less => Opcode::Lt,
        LessEqual => Opcode::Le,
        Greater => Opcode::Gt,
        GreaterEqual => Opcode::Ge,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use thin_vec::thin_vec;
    use xc_ast::expr::ExprFlags;
    use xc_ast::literal::{Literal, LiteralKind};
    use xc_ast::stmt::Stmt;
    use xc_span::create_session_globals_then;
    use xc_span::Span;

    fn lit_int(n: i128) -> P<Expr> {
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
    fn infix_emits_binary_ops() {
        create_session_globals_then(|| {
            let program = Program {
                funcs: vec![],
                func_indices: std::collections::HashMap::new(),
                top: vec![Stmt {
                    kind: StmtKind::ExprSemi(bin(op::Plus, lit_int(1), lit_int(2))),
                    span: Span::DUMMY,
                }],
            };
            let chunks = compile(&program).unwrap();
            let ops: Vec<Opcode> = chunks[0].code.iter().map(|i| i.op).collect();
            assert_eq!(
                ops,
                vec![Opcode::Const, Opcode::Const, Opcode::Add, Opcode::Ret]
            );
        });
    }
}
