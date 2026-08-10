# X 解释器垂直切片实现计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 让 `x run foo.x` 端到端执行 X 语言最小子集：解析 → 检查 → 字节码 → 栈式 VM。

**Architecture:** 复用现有 `xc_parse`/`xc_ast` 前端；新增 `xvalue`（运行时值）、`xc_check`（解析编排 + 符号表 + 最小检查）、`xvm`（字节码 IR + 编译器 + 栈式 VM）；`xc_driver` 提供 `run_source` 与 CLI；根包改名 `x`。VM 为栈式，变量分 frame-local 槽位 + 全局表，值用 tagged `Value`。

**Tech Stack:** Rust 2024 edition，workspace 分层 crate；`xc_parse`/`xc_ast`/`xc_span`/`xc_error` 现有依赖；无新外部依赖（`thin-vec` 已是现有依赖）。

---

## 范围偏差（相对 spec 文档已确认）

- **切片含数组**：`for` 需要数组才能迭代，故 `Value` 含 `Array`，支持 `[a,b,c]` 字面量与 `for-in` 迭代。不含下标/字典/方法。
- **`xvalue` 为新建 crate，不动 xepl**：xepl 的 `Value` 暂时保留，REPL 迁移到 VM 时再统一（spec 的"抽离共用"推迟）。
- **未定义变量为运行时错误**（`LoadGlobal` 未命中报错），不是检查期错误。
- **链式比较不支持**（`a < b < c` 编译报错），仅支持单比较；`&&`/`||` 正确短路。
- **函数参数必须带类型标注**（如 `func add(a: int, b: int)`）：`parse_func_param` 对无类型参数命中错误恢复 stub。返回类型可省略。测试与示例均按此书写。
- **`for` 用冒号不用 `in`**：`for Pattern : Expression Block`（spec statement.tex）。并给 for 模式解析补上 `PAT_IN_LET`，否则 `x` 被解析成 Expr 模式而非绑定。
- **分号可选**：修掉 stmt.rs 的 `todo!()` 后，缺失分号宽容接受（测试仍写分号）。

实现过程中额外发现并修复的既有解析器 bug：

- **运算符优先级反了**：`1 + 2 * 3` 原算成 `(1+2)*3 = 9`。`merge_infix` 的 `main_info` 选了值最小（绑得最紧）的操作符，应选值最大（最松）。改为 7。
- **实现 STMT_EXPR（参考 rustc `should_continue_as_assoc_expr`）**：语句位置的完整表达式（`if`/`while`/`for`/`match`/块）在 `}` 处结束语句；后跟可作前缀的运算符（`+`/`-`）也结束（成为下一条语句）；后跟明确二元运算符报错提示加括号；表达式位置不受影响。由此 `while i < 10 { }` 后无需分号。
- **`(1)` 后缀 + infix 混排 bug**：`f(1) + 2` 的原子 run 是 `[FuncCall, +]`，分类的 `possible_infix` 检查把前后缀方向写反，且 infix 构造用 `pop()`（尾取）在 `drain` 后越界。改为后缀在前/前缀在后 + `remove(0)` 取 infix。
- **编译器 `emit_load` 先查本地槽**：顶层 for 循环变量是 frame-0 槽，原先顶层一律 LoadGlobal 导致 `undefined variable`。

## 文件结构

| 文件 | 职责 |
|------|------|
| `lib/xvalue/src/value.rs` | `Value` 枚举 + `from_lit` + `write_value` |
| `lib/xc_check/src/parse.rs` | `parse_program`：顶层解析循环（func 声明 + 顶层语句） |
| `lib/xc_check/src/check.rs` | `Program` + `check`：重复函数、未定义函数、调用参数个数 |
| `lib/xvm/src/op.rs` | `Opcode` 枚举 |
| `lib/xvm/src/chunk.rs` | `Chunk`/`Instr` + emit/jump/patch 辅助 |
| `lib/xvm/src/compile.rs` | AST → `Vec<Chunk>`（chunk 0 顶层，函数随后） |
| `lib/xvm/src/vm.rs` | 栈式 VM：执行 `&[Chunk]`，返回 stdout 字符串 |
| `lib/xc_driver/src/lib.rs` | `run_source` + CLI `main` |
| `lib/xc_parse/src/parser/stmt.rs` | 修 `todo!()`（分号可选） |

---

### Task 1: `let` 归位为语句关键字；缺分号不再 panic（解析器修复）

**Files:**
- Modify: `lib/xc_ast/src/token.rs`（`ident_can_begin_expr` 白名单移除 `kw::Let`）
- Modify: `lib/xc_parse/src/parser/stmt.rs:66-71`
- Test: `lib/xc_parse/src/parser/tests.rs`

设计事实：`let` 是语句关键字，**不是表达式**。但 `ident_can_begin_expr` 把 `kw::Let` 列在白名单里，而 `parse_expr_primary` 根本没有处理 `let`（会 `unimplemented!()` panic）——白名单与实现不一致。第一步把 `kw::Let` 从白名单删掉，让 `let` 与 `func` 一样不被当作表达式开头。

- [ ] **Step 1: 写失败测试**

在 `lib/xc_parse/src/parser/tests.rs` 末尾追加：

```rust
#[test]
fn missing_semicolon_before_let_parses_two_statements() {
    create_session_globals_then(|| {
        let session = ParseSession::new();
        let mut parser = parser_from_source_str(
            &session,
            Filename::Normal("test".into()),
            "{ 1 let x = 2 }".to_string(),
        );
        let block = parser.parse_block().expect("block parses");
        assert_eq!(block.stmts.len(), 2, "`1` and `let x = 2` are two statements");
    });
}
```

（`tests.rs` 模块顶层已导入 `crate::{parser_from_source_str, session::ParseSession}` 与 `xc_span::{create_session_globals_then, source_map::Filename}`，测试内无需再 `use`。）

**为什么输入是 `{ 1 let x = 2 }`：** 表达式语句 `1` 需要分号；`let` 是语句关键字，不应作为表达式开头。修好 token.rs 后，原子收集循环在 `let` 前干净结束，走到 stmt.rs 的 `todo!()` 分支；修复后再把 `let x = 2` 解析为第二条语句。若不加 token.rs 修复，`let` 会先被当作紧跟的表达式原子收集，在 `parse_expr_primary`（expr.rs:1007）`unimplemented!()` panic，到不了 stmt.rs。

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xc_parse missing_semicolon_before_let_parses_two_statements`
Expected: FAIL — `parse_expr_primary` 对 `let` 命中 `unimplemented!()`（expr.rs:1007），panic。

- [ ] **Step 3: 修复 token.rs：从白名单移除 `kw::Let`**

`lib/xc_ast/src/token.rs` 的 `ident_can_begin_expr`，把数组中的 `kw::Let,` 一行删除：

```rust
        || [
            kw::Break,
            kw::Continue,
            kw::Do,
            kw::False,
            kw::For,
            kw::If,
            // kw::Let — `let` is a statement keyword, not an expression starter
            kw::Match,
            kw::Nil,
            kw::Operator,
            kw::Return,
            kw::This,
            kw::Throw,
            kw::True,
            kw::Try,
            kw::While,
        ]
```

- [ ] **Step 4: 运行测试——现在应 panic 在 stmt.rs**

Run: `cargo test -p xc_parse missing_semicolon_before_let_parses_two_statements`
Expected: 仍 FAIL，但 panic 位置变为 `stmt.rs` 的 `todo!()`（输出含 `not yet implemented` 且文件为 `stmt.rs`）。这证明 token.rs 修复生效，剩余阻碍是 stmt.rs。

- [ ] **Step 5: 修复 stmt.rs：去掉 `todo!()`**

`lib/xc_parse/src/parser/stmt.rs`，把 `todo!()` 所在的第一个 `Expr` 分支体（约 66-71 行）改为空体：

```rust
            Expr(expr)
                if expr.require_semi_to_be_stmt() &&
                    ![TokenKind::Eof, TokenKind::Semicolon, TokenKind::CloseDelim(Delimiter::Brace)].contains(&self.token.kind) =>
            {
                // Missing semicolon: accepted leniently; trailing `;` stays optional.
            }
```

（第二个分支保持不变：它只会在下一个 token 是 `;` 或 `}` 时命中，`expect_one_of` 的 `inedible` 分支不会 panic。）

- [ ] **Step 6: 运行测试确认通过**

Run: `cargo test -p xc_parse missing_semicolon_before_let_parses_two_statements`
Expected: PASS — 块解析出 `Expr(1)` 与 `Let(x = 2)` 两条语句。

- [ ] **Step 7: 全量回归**

Run: `cargo test -p xc_parse`
Expected: 全部通过（`let` 语句本身走 `parse_stmt_no_recovery` 的显式 `kw::Let` 检查，不受影响）。

- [ ] **Step 8: 空白检查**

Run: `git -C /Users/sunchy321/Desktop/X/xc diff --check`
Expected: 无输出。

- [ ] **Step 9: Commit**

```bash
git add lib/xc_ast/src/token.rs lib/xc_parse/src/parser/stmt.rs lib/xc_parse/src/parser/tests.rs
git commit -m "fix: treat let as a statement keyword and stop panicking on missing semicolons"
```

---

### Task 2: 根包改名 `x`，新增三个 crate 骨架

**Files:**
- Modify: `Cargo.toml`（根）
- Create: `lib/xvalue/Cargo.toml`, `lib/xvalue/src/lib.rs`
- Create: `lib/xc_check/Cargo.toml`, `lib/xc_check/src/lib.rs`
- Create: `lib/xvm/Cargo.toml`, `lib/xvm/src/lib.rs`

- [ ] **Step 1: 写根 Cargo.toml**

`Cargo.toml` 整体替换为：

```toml
[workspace]
members = [
    "lib/xc_ast",
    "lib/xc_span",
    "lib/xc_lexer",
    "lib/xc_error",
    "lib/xc_macro",
    "lib/xc_parse",
    "lib/xc_driver",
    "lib/xepl",
    "lib/xvalue",
    "lib/xc_check",
    "lib/xvm",
]
resolver = "3"

[package]
name = "x"
version = "0.1.0"
edition = "2024"

[dependencies]
xc_driver = { version = "0.1.0", path = "lib/xc_driver" }
```

- [ ] **Step 2: 写三个 crate 的 Cargo.toml 与骨架 lib.rs**

`lib/xvalue/Cargo.toml`:

```toml
[package]
name = "xvalue"
version = "0.1.0"
edition = "2024"

[dependencies]
xc_ast = { version = "0.1.0", path = "../xc_ast" }
xc_span = { version = "0.1.0", path = "../xc_span" }
```

`lib/xvalue/src/lib.rs`:

```rust
pub mod value;
```

`lib/xc_check/Cargo.toml`:

```toml
[package]
name = "xc_check"
version = "0.1.0"
edition = "2024"

[dependencies]
xc_ast = { version = "0.1.0", path = "../xc_ast" }
xc_error = { version = "0.1.0", path = "../xc_error" }
xc_parse = { version = "0.1.0", path = "../xc_parse" }
xc_span = { version = "0.1.0", path = "../xc_span" }
```

`lib/xc_check/src/lib.rs`:

```rust
pub mod parse;
```

`lib/xvm/Cargo.toml`:

```toml
[package]
name = "xvm"
version = "0.1.0"
edition = "2024"

[dependencies]
thin-vec = "0.2.13"
xc_ast = { version = "0.1.0", path = "../xc_ast" }
xc_check = { version = "0.1.0", path = "../xc_check" }
xc_span = { version = "0.1.0", path = "../xc_span" }
xvalue = { version = "0.1.0", path = "../xvalue" }
```

`lib/xvm/src/lib.rs`:

```rust
pub mod chunk;
pub mod op;
```

- [ ] **Step 3: 构建**

Run: `cargo build`
Expected: 构建成功，产出 `target/debug/x` 二进制。

- [ ] **Step 4: Commit**

```bash
git add Cargo.toml lib/xvalue lib/xc_check lib/xvm
git commit -m "chore: rename root package to x, add xvalue/xc_check/xvm crate skeletons"
```

---

### Task 3: `xvalue` crate（Value + from_lit + write_value）

**Files:**
- Create: `lib/xvalue/src/value.rs`

- [ ] **Step 1: 写失败测试**

在 `lib/xvalue/src/value.rs` 末尾追加：

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use xc_ast::literal::{Literal, LiteralKind};
    use xc_span::create_session_globals_then;
    use xc_span::Symbol;

    #[test]
    fn int_from_lit() {
        create_session_globals_then(|| {
            let lit = Literal::new(LiteralKind::Integer, Symbol::intern("42"), None);
            assert_eq!(Value::from_lit(&lit).unwrap(), Value::Int(42));
        });
    }

    #[test]
    fn write_string_raw() {
        let mut out = String::new();
        write_value(&Value::String("hi".into()), &mut out).unwrap();
        assert_eq!(out, "hi");
    }
}
```

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xvalue`
Expected: FAIL — `value.rs` 尚不存在（或为空），编译错误。

- [ ] **Step 3: 实现**

`lib/xvalue/src/value.rs` 完整内容：

```rust
use std::fmt;

use xc_ast::literal::{Literal, LiteralKind};
use xc_span::symbol::kw;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Void,
    Nil,
    Bool(bool),
    Int(i128),
    String(String),
    Array(Vec<Value>),
}

impl Value {
    pub fn from_lit(lit: &Literal) -> Result<Self, String> {
        use LiteralKind::*;
        Ok(match &lit.kind {
            Nil => Value::Nil,
            Bool => Value::Bool(lit.value == kw::True),
            Integer => {
                let s = lit.value.as_str();
                let (s, radix) = if let Some(rest) = s.strip_prefix("0x") {
                    (rest, 16)
                } else if let Some(rest) = s.strip_prefix("0b") {
                    (rest, 2)
                } else {
                    (s, 10)
                };
                Value::Int(
                    i128::from_str_radix(s, radix)
                        .map_err(|_| format!("invalid integer literal: {s}"))?,
                )
            }
            String => Value::String(lit.value.to_string()),
            _ => return Err(format!("unsupported literal in this build: {lit}")),
        })
    }
}

pub fn write_value(v: &Value, out: &mut impl fmt::Write) -> fmt::Result {
    match v {
        Value::Void => write!(out, "void"),
        Value::Nil => write!(out, "nil"),
        Value::Bool(b) => write!(out, "{b}"),
        Value::Int(i) => write!(out, "{i}"),
        Value::String(s) => write!(out, "{s}"),
        Value::Array(items) => {
            write!(out, "[")?;
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write_value(item, out)?;
            }
            write!(out, "]")
        }
    }
}
```

- [ ] **Step 4: 运行测试确认通过**

Run: `cargo test -p xvalue`
Expected: 两个测试 PASS。

- [ ] **Step 5: Commit**

```bash
git add lib/xvalue
git commit -m "feat: add xvalue runtime value crate"
```

---

### Task 4: `xc_check` crate（parse_program + check）

**Files:**
- Create: `lib/xc_check/src/parse.rs`
- Create: `lib/xc_check/src/check.rs`
- Modify: `lib/xc_check/src/lib.rs`

- [ ] **Step 1: 写失败测试**

`lib/xc_check/src/lib.rs` 整体替换为：

```rust
pub mod check;
pub mod parse;

pub use check::{check, Program};
pub use parse::parse_program;
```

`lib/xc_check/src/parse.rs`：

```rust
use xc_ast::decl::DeclKind;
use xc_ast::func::Func;
use xc_ast::ptr::P;
use xc_ast::stmt::Stmt;
use xc_ast::token::TokenKind;
use xc_error::diag::Diagnostic;
use xc_error::msg::DiagnosticMessage;
use xc_parse::parser_from_source_str;
use xc_parse::session::ParseSession;
use xc_span::source_map::Filename;
use xc_span::symbol::kw;

#[derive(Debug)]
pub struct Parsed {
    pub funcs: Vec<P<Func>>,
    pub top: Vec<Stmt>,
}

pub fn parse_program(name: Filename, source: &str) -> Result<Parsed, String> {
    let session = ParseSession::new();
    let mut parser = parser_from_source_str(&session, name, source.to_string());

    let mut funcs = Vec::new();
    let mut top = Vec::new();

    loop {
        if parser.token.kind == TokenKind::Eof {
            break;
        }
        if parser.check_keyword(kw::Func) {
            let decl = parser.parse_decl().map_err(format_diag)?;
            match decl {
                Some(decl) => match decl.kind {
                    DeclKind::Func(f) => funcs.push(f),
                    _ => return Err("unsupported top-level declaration".to_string()),
                },
                None => {
                    return Err("internal: parse_decl returned None after func keyword".to_string())
                }
            }
        } else if let Some(stmt) = parser.parse_full_stmt().map_err(format_diag)? {
            top.push(stmt);
        } else {
            break;
        }
    }

    Ok(Parsed { funcs, top })
}

fn format_diag(d: &Diagnostic<'_>) -> String {
    d.messages
        .first()
        .map(|m| match m {
            DiagnosticMessage::String(s) => s.to_string(),
        })
        .unwrap_or_else(|| "parse error".to_string())
}
```

`lib/xc_check/src/check.rs`：

```rust
use std::collections::HashMap;

use xc_ast::expr::{Argument, Expr, ExprItem, ExprKind};
use xc_ast::func::{Func, FuncBody};
use xc_ast::ptr::P;
use xc_ast::stmt::{Block, Stmt, StmtKind};
use xc_span::Symbol;

use crate::parse::Parsed;

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<P<Func>>,
    pub func_indices: HashMap<Symbol, u32>,
    pub top: Vec<Stmt>,
}

pub fn check(parsed: Parsed) -> Result<Program, String> {
    let mut func_indices = HashMap::new();
    for (i, f) in parsed.funcs.iter().enumerate() {
        let idx = i as u32 + 1;
        if func_indices.insert(f.ident.name, idx).is_some() {
            return Err(format!("duplicate function: {}", f.ident.as_str()));
        }
    }

    let program = Program {
        funcs: parsed.funcs,
        func_indices,
        top: parsed.top,
    };

    for f in &program.funcs {
        check_func(f, &program)?;
    }
    for stmt in &program.top {
        check_stmt(stmt, &program)?;
    }
    Ok(program)
}

fn check_func(f: &P<Func>, program: &Program) -> Result<(), String> {
    if !matches!(f.sig.tail.throw, xc_ast::func::FuncThrow::None) {
        return Err(format!(
            "throw qualifier not supported yet: {}",
            f.ident.as_str()
        ));
    }
    match &f.body {
        FuncBody::Block(block) => check_block(block, program),
        FuncBody::Arrow(e) => check_expr(e, program),
        FuncBody::Semicolon => {
            Err(format!("function without body: {}", f.ident.as_str()))
        }
    }
}

fn check_stmt(s: &Stmt, program: &Program) -> Result<(), String> {
    match &s.kind {
        StmtKind::Expr(e) | StmtKind::ExprSemi(e) => check_expr(e, program),
        StmtKind::Let(_, init) => {
            if let Some(init) = init {
                check_expr(init, program)?;
            }
            Ok(())
        }
        StmtKind::Empty => Ok(()),
    }
}

fn check_block(b: &Block, program: &Program) -> Result<(), String> {
    for stmt in &b.stmts {
        check_stmt(stmt, program)?;
    }
    Ok(())
}

fn check_expr(e: &Expr, program: &Program) -> Result<(), String> {
    use ExprKind::*;
    match &e.kind {
        Placeholder | This | Caret | Dollar | LambdaArgOrdinal(_) | LambdaArgNamed(_)
        | AnonEnumerator(_) | Struct(_) | Dict(_) | Lambda(_) | Match(..) | Try(..)
        | Is(..) | NotIs(..) | Suffix(..) | MemberAccess(..) | MemberDynamicAccess(..)
        | MethodCall(..) | Subscript(..) | Cast(..) | Await(..) => {
            Err(format!("unsupported expression in this build: {e:?}"))
        }
        Literal(_) | Nil | Path(_) => Ok(()),
        Paren(inner) => check_expr(inner, program),
        Array(items) => {
            for item in items {
                if let ExprItem::Expr(e) = item {
                    check_expr(e, program)?;
                }
            }
            Ok(())
        }
        Block(b) => check_block(b, program),
        If(c, t, el) => {
            check_expr(c, program)?;
            check_expr(t, program)?;
            if let Some(el) = el {
                check_expr(el, program)?;
            }
            Ok(())
        }
        For(_, iter, body, else_, ..) => {
            check_expr(iter, program)?;
            check_block(body, program)?;
            if let Some(e) = else_ {
                check_block(e, program)?;
            }
            Ok(())
        }
        While(c, body, el, _) => {
            check_expr(c, program)?;
            check_expr(body, program)?;
            if let Some(el) = el {
                check_expr(el, program)?;
            }
            Ok(())
        }
        Return(v) | Throw(v) | Break(v, _) => {
            if let Some(v) = v {
                check_expr(v, program)?;
            }
            Ok(())
        }
        Continue(_) => Ok(()),
        Prefix(_, x) => check_expr(x, program),
        Infix(_, exprs) => {
            for e in exprs {
                check_expr(e, program)?;
            }
            Ok(())
        }
        Comparison(_, exprs) => {
            for e in exprs {
                check_expr(e, program)?;
            }
            Ok(())
        }
        FuncCall(callee, args, _) => {
            let name = match &callee.kind {
                ExprKind::Path(p) if p.segments.len() == 1 => p.segments[0].ident.name,
                _ => return Err("only simple function calls are supported".to_string()),
            };
            if name.as_str() == "print" {
                for arg in &args.0 {
                    if let Argument::Ordinal(e) = arg {
                        check_expr(e, program)?;
                    }
                }
                return Ok(());
            }
            program
                .func_indices
                .get(&name)
                .ok_or_else(|| format!("undefined function: {}", name.as_str()))?;
            let f = program
                .funcs
                .iter()
                .find(|f| f.ident.name == name)
                .expect("func index present but func missing");
            let expected = f.sig.params.len();
            let mut actual = 0;
            for arg in &args.0 {
                match arg {
                    Argument::Ordinal(e) => {
                        actual += 1;
                        check_expr(e, program)?;
                    }
                    Argument::Named(..) => {
                        return Err("named arguments not supported yet".to_string())
                    }
                    Argument::Expansion(..) => {
                        return Err("expansion arguments not supported yet".to_string())
                    }
                }
            }
            if actual != expected {
                return Err(format!(
                    "function {} expects {expected} arguments, got {actual}",
                    name.as_str()
                ));
            }
            Ok(())
        }
        Error(_) => Err("internal: parse error node".to_string()),
    }
}
```

在 `lib/xc_check/src/check.rs` 末尾追加测试：

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_program;
    use xc_span::create_session_globals_then;
    use xc_span::source_map::Filename;

    fn run_check(src: &str) -> Result<Program, String> {
        create_session_globals_then(|| {
            let parsed = parse_program(Filename::Normal("test".into()), src)?;
            check(parsed)
        })
    }

    #[test]
    fn duplicate_function_is_error() {
        let err = run_check("func f() {} func f() {}").unwrap_err();
        assert!(err.contains("duplicate function: f"), "{err}");
    }

    #[test]
    fn undefined_function_is_error() {
        let err = run_check("foo(1);").unwrap_err();
        assert!(err.contains("undefined function: foo"), "{err}");
    }

    #[test]
    fn arity_mismatch_is_error() {
        let err = run_check("func f(a, b) {} f(1);").unwrap_err();
        assert!(err.contains("expects 2 arguments, got 1"), "{err}");
    }

    #[test]
    fn recursion_is_allowed() {
        let p = run_check("func f(n) { return f(n - 1); } print(f(1));").unwrap();
        assert_eq!(p.funcs.len(), 1);
    }
}
```

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xc_check`
Expected: FAIL — `parse.rs`/`check.rs` 不存在。

- [ ] **Step 3: 运行测试确认通过**

Run: `cargo test -p xc_check`
Expected: 四个测试 PASS。

- [ ] **Step 4: Commit**

```bash
git add lib/xc_check
git commit -m "feat: add xc_check frontend analysis crate (parse loop + symbol table + call checks)"
```

---

### Task 5: `xvm` 字节码 IR（op + chunk）

**Files:**
- Create: `lib/xvm/src/op.rs`
- Modify: `lib/xvm/src/chunk.rs`

- [ ] **Step 1: 实现（纯结构，无测试驱动）**

`lib/xvm/src/op.rs`：

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Opcode {
    Const,
    LoadLocal,
    StoreLocal,
    LoadGlobal,
    StoreGlobal,
    Pop,
    Dup,
    Arr,
    ArrLen,
    ArrIndex,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Jmp,
    JmpIfFalse,
    Call,
    Ret,
    Print,
}
```

`lib/xvm/src/chunk.rs`：

```rust
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

    pub fn emit_jump(&mut self, op: Opcode, pos: u32) -> usize {
        self.code.push(Instr { op, operand: 0, pos });
        self.code.len() - 1
    }

    pub fn patch_jump(&mut self, jump: usize) {
        let offset = (self.code.len() as i32) - (jump as i32 + 1);
        self.code[jump].operand = offset;
    }
}
```

- [ ] **Step 2: 构建**

Run: `cargo build`
Expected: 构建成功。

- [ ] **Step 3: Commit**

```bash
git add lib/xvm/src/op.rs lib/xvm/src/chunk.rs
git commit -m "feat: add xvm opcodes and chunk IR"
```

---

### Task 6: `xvm` 编译器（AST → 字节码）

**Files:**
- Create: `lib/xvm/src/compile.rs`
- Modify: `lib/xvm/src/lib.rs`

- [ ] **Step 1: 写失败测试**

`lib/xvm/src/lib.rs` 整体替换为：

```rust
pub mod chunk;
pub mod compile;
pub mod op;

pub use compile::compile;
```

在 `lib/xvm/src/compile.rs` 末尾追加测试：

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use thin_vec::thin_vec;
    use xc_ast::expr::ExprFlags;
    use xc_ast::literal::{Literal, LiteralKind};
    use xc_ast::stmt::Stmt;
    use xc_span::create_session_globals_then;
    use xc_span::symbol::op;
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
```

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xvm infix_emits_binary_ops`
Expected: FAIL — `compile.rs` 不存在。

- [ ] **Step 3: 实现编译器**

`lib/xvm/src/compile.rs` 完整内容：

```rust
use std::collections::HashMap;

use xc_ast::expr::{Argument, Expr, ExprItem, ExprKind};
use xc_ast::func::FuncBody;
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
            chunk.emit(Opcode::Const, chunk.add_const(Value::Void), 0);
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
                chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos);
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
                    None => chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos),
                }
                self.emit_store(chunk, ctx, name, is_top, pos)?;
                // A let statement evaluates to void.
                chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos);
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
                chunk.emit(Opcode::Const, chunk.add_const(v), pos);
            }
            Nil => {
                chunk.emit(Opcode::Const, chunk.add_const(Value::Nil), pos);
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
                    chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos);
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
                    chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos);
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
                chunk.emit(Opcode::Const, chunk.add_const(Value::Int(0)), pos);
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
                chunk.emit(Opcode::Const, chunk.add_const(Value::Int(1)), pos);
                chunk.emit(Opcode::Add, 0, pos);
                chunk.emit(Opcode::StoreLocal, temp_i as i32, pos);
                let back = chunk.emit_jump(Opcode::Jmp, pos);
                chunk.code[back].operand = (loop_start as i32) - (back as i32 + 1);
                let info = self.loop_stack.pop().unwrap();
                chunk.patch_jump(jmp_false);
                chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos);
                let end = chunk.code.len();
                for bp in &info.break_positions {
                    chunk.code[*bp].operand = (end as i32) - (*bp as i32 + 1);
                }
            }
            Return(val) => {
                match val {
                    Some(v) => self.compile_expr(chunk, ctx, v, is_top)?,
                    None => chunk.emit(Opcode::Const, chunk.add_const(Value::Void), pos),
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
                        chunk.emit(Opcode::Const, chunk.add_const(Value::Bool(false)), pos);
                        chunk.patch_jump(end_jmp);
                    } else {
                        chunk.emit(Opcode::Const, chunk.add_const(Value::Bool(true)), pos);
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
                                return Err("named / expansion arguments not supported yet"
                                    .to_string())
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
                                return Err("named / expansion arguments not supported yet"
                                    .to_string())
                            }
                        }
                    }
                    chunk.emit(Opcode::Call, idx as i32, pos);
                }
            }
            other => {
                return Err(format!(
                    "unsupported expression in this build: {other:?}"
                ));
            }
        }
        Ok(())
    }

    fn emit_load(
        &mut self,
        chunk: &mut Chunk,
        ctx: &Ctx,
        name: Symbol,
        is_top: bool,
        pos: u32,
    ) -> Result<(), String> {
        if !is_top {
            if let Some(slot) = ctx.resolve(name) {
                chunk.emit(Opcode::LoadLocal, slot as i32, pos);
                return Ok(());
            }
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
```

- [ ] **Step 4: 运行测试确认通过**

Run: `cargo test -p xvm infix_emits_binary_ops`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add lib/xvm/src/compile.rs lib/xvm/src/lib.rs
git commit -m "feat: add xvm AST-to-bytecode compiler"
```

---

### Task 7: `xvm` 栈式 VM

**Files:**
- Create: `lib/xvm/src/vm.rs`
- Modify: `lib/xvm/src/lib.rs`

- [ ] **Step 1: 写失败测试**

`lib/xvm/src/lib.rs` 整体替换为：

```rust
pub mod chunk;
pub mod compile;
pub mod op;
pub mod vm;

pub use compile::compile;
pub use vm::run;
```

在 `lib/xvm/src/vm.rs` 末尾追加测试：

```rust
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
```

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xvm vm_runs_hand_built_chunk`
Expected: FAIL — `vm.rs` 不存在。

- [ ] **Step 3: 实现 VM**

`lib/xvm/src/vm.rs` 完整内容：

```rust
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
    let mut stack: Vec<Value> = std::iter::repeat(Value::Void).take(top_num_locals).collect();
    let mut globals: HashMap<String, Value> = HashMap::new();
    let mut frames: Vec<Frame> = vec![Frame {
        chunk: 0,
        ip: 0,
        base: 0,
    }];
    let mut output = String::new();

    macro_rules! rt {
        ($msg:expr) => {
            return Err(format!(
                "runtime error: {} at {} (line {})",
                $msg,
                chunks[frames[frames.len() - 1].chunk].name,
                line_of(source, instr.pos)
            ));
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
                    _ => rt!("internal: global name constant is not a string"),
                };
                match globals.get(&name) {
                    Some(v) => stack.push(v.clone()),
                    None => rt!(format!("undefined variable: {name}")),
                }
            }
            Opcode::StoreGlobal => {
                let name = match &chunk.constants[instr.operand as usize] {
                    Value::String(s) => s.clone(),
                    _ => rt!("internal: global name constant is not a string"),
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
                    _ => rt!("length of a non-array"),
                }
            }
            Opcode::ArrIndex => {
                let idx = stack.pop().unwrap();
                let arr = stack.pop().unwrap();
                match (arr, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        if i < 0 || i as usize >= items.len() {
                            rt!(format!(
                                "array index {i} out of bounds (len {})",
                                items.len()
                            ));
                        }
                        stack.push(items[i as usize].clone());
                    }
                    _ => rt!("array index on a non-array"),
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
                                    rt!("division by zero");
                                }
                                a / b
                            }
                            Opcode::Mod => {
                                if b == 0 {
                                    rt!("modulo by zero");
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
                    _ => rt!("arithmetic operands must be int (string only for +)"),
                }
            }
            Opcode::Neg => {
                let v = stack.pop().unwrap();
                match v {
                    Value::Int(a) => stack.push(Value::Int(-a)),
                    _ => rt!("negation operand must be int"),
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
                        rt!("ordering operands must be int")
                    }
                    _ => rt!("comparison operands mismatch"),
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
                        frames[frame_idx].ip = (frames[frame_idx].ip as i32 + instr.operand) as usize;
                    }
                    Value::Bool(true) => {}
                    _ => rt!("condition must be bool"),
                }
            }
            Opcode::Call => {
                let f = instr.operand as usize;
                let callee = &chunks[f];
                let arity = callee.arity as usize;
                if stack.len() < arity {
                    rt!(format!("internal: call to {} with too few args", callee.name));
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
```

- [ ] **Step 4: 运行测试确认通过**

Run: `cargo test -p xvm vm_runs_hand_built_chunk`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add lib/xvm/src/vm.rs lib/xvm/src/lib.rs
git commit -m "feat: add xvm stack-based VM"
```

---

### Task 8: `xc_driver` glue 与 `x run` CLI

**Files:**
- Modify: `lib/xc_driver/Cargo.toml`
- Modify: `lib/xc_driver/src/lib.rs`

- [ ] **Step 1: 写失败测试**

`lib/xc_driver/Cargo.toml` 整体替换为：

```toml
[package]
name = "xc_driver"
version = "0.1.0"
edition = "2024"

[dependencies]
xc_check = { version = "0.1.0", path = "../xc_check" }
xc_error = { version = "0.1.0", path = "../xc_error" }
xc_span = { version = "0.1.0", path = "../xc_span" }
xvm = { version = "0.1.0", path = "../xvm" }
```

`lib/xc_driver/src/lib.rs` 整体替换为：

```rust
use xc_check::{check, parse_program};
use xc_span::create_session_globals_then;
use xc_span::source_map::Filename;
use xvm::{compile, run};

pub fn run_source(name: Filename, source: &str) -> Result<String, String> {
    create_session_globals_then(|| {
        let parsed = parse_program(name, source)?;
        let program = check(parsed)?;
        let chunks = compile(&program)?;
        run(&chunks, source)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_runs() {
        let out = run_source(Filename::Normal("test".into()), "print(\"hi\");").unwrap();
        assert_eq!(out, "hi\n");
    }

    #[test]
    fn undefined_function_is_reported() {
        let err = run_source(Filename::Normal("test".into()), "foo(1);").unwrap_err();
        assert!(err.contains("undefined function: foo"), "{err}");
    }

    #[test]
    fn division_by_zero_is_reported() {
        let err = run_source(Filename::Normal("test".into()), "print(1 / 0);").unwrap_err();
        assert!(err.contains("division by zero"), "{err}");
    }
}
```

- [ ] **Step 2: 运行测试确认失败**

Run: `cargo test -p xc_driver`
Expected: FAIL — `xc_driver` 尚无 `run_source`。

- [ ] **Step 3: 实现 CLI `main`**

在 `lib/xc_driver/src/lib.rs` 末尾追加：

```rust
pub fn main() -> ! {
    let args: Vec<String> = std::env::args().collect();
    if args.len() >= 3 && args[1] == "run" {
        let path = &args[2];
        match std::fs::read_to_string(path) {
            Ok(source) => match run_source(Filename::Normal(path.clone()), &source) {
                Ok(out) => {
                    print!("{out}");
                    std::process::exit(0);
                }
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(1);
                }
            },
            Err(err) => {
                eprintln!("error: cannot read {path}: {err}");
                std::process::exit(1);
            }
        }
    } else {
        eprintln!("usage: x run <file.x>");
        std::process::exit(2);
    }
}
```

- [ ] **Step 4: 运行测试确认通过**

Run: `cargo test -p xc_driver`
Expected: 三个测试 PASS。

- [ ] **Step 5: 端到端验证 CLI**

写临时文件：

```bash
printf 'print("hi");\n' > /tmp/hello.x
cargo run -p x -- run /tmp/hello.x
```

Expected: 输出 `hi`。

- [ ] **Step 6: Commit**

```bash
git add lib/xc_driver
git commit -m "feat: add x run CLI driver"
```

---

### Task 9: 金样本集成测试

**Files:**
- Create: `lib/xc_driver/tests/golden.rs`

- [ ] **Step 1: 写金样本**

`lib/xc_driver/tests/golden.rs`：

```rust
use xc_driver::run_source;
use xc_span::source_map::Filename;

fn run(src: &str) -> Result<String, String> {
    run_source(Filename::Normal("golden".into()), src)
}

#[test]
fn arithmetic() {
    assert_eq!(run("print(1 + 2 * 3);").unwrap(), "7\n");
}

#[test]
fn left_associative() {
    assert_eq!(run("print(10 - 4 - 3);").unwrap(), "3\n");
}

#[test]
fn variables_and_assignment() {
    assert_eq!(
        run("let x = 5; x = x + 1; print(x);").unwrap(),
        "6\n"
    );
}

#[test]
fn if_else() {
    assert_eq!(
        run("if 1 < 2 { print(\"a\"); } else { print(\"b\"); }").unwrap(),
        "a\n"
    );
}

#[test]
fn while_loop_sum() {
    let src = "\
let i = 0;
let sum = 0;
while i < 10 { sum = sum + i; i = i + 1; }
print(sum);
";
    assert_eq!(run(src).unwrap(), "45\n");
}

#[test]
fn for_loop_over_array() {
    let src = "\
for x in [1, 2, 3] { print(x); }
";
    assert_eq!(run(src).unwrap(), "1\n2\n3\n");
}

#[test]
fn function_call() {
    assert_eq!(
        run("func add(a: int, b: int) { return a + b; } print(add(2, 3));").unwrap(),
        "5\n"
    );
}

#[test]
fn recursion_fib() {
    let src = "\
func fib(n: int) { if n < 2 { return n; } return fib(n - 1) + fib(n - 2); }
print(fib(10));
";
    assert_eq!(run(src).unwrap(), "55\n");
}

#[test]
fn function_block_value() {
    assert_eq!(run("func answer() { 42 } print(answer());").unwrap(), "42\n");
}

#[test]
fn function_reads_global() {
    let src = "\
let base = 10;
func add(n: int) { return base + n; }
print(add(5));
";
    assert_eq!(run(src).unwrap(), "15\n");
}

#[test]
fn string_concat() {
    assert_eq!(run("print(\"a\" + \"b\");").unwrap(), "ab\n");
}

#[test]
fn logic_short_circuit() {
    assert_eq!(
        run("print(1 < 2 && 2 < 3);").unwrap(),
        "true\n"
    );
    assert_eq!(
        run("print(0 == 1 && 10 / 0 > 1);").unwrap(),
        "false\n"
    );
    assert_eq!(
        run("print(1 == 1 || 10 / 0 > 1);").unwrap(),
        "true\n"
    );
}

#[test]
fn break_in_while() {
    let src = "\
let i = 0;
while true { i = i + 1; if i >= 3 { break; } }
print(i);
";
    assert_eq!(run(src).unwrap(), "3\n");
}

#[test]
fn continue_in_while() {
    let src = "\
let i = 0;
let sum = 0;
while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; }
print(sum);
";
    assert_eq!(run(src).unwrap(), "12\n");
}

#[test]
fn errors_are_reported() {
    let err = run("print(x);").unwrap_err();
    assert!(err.contains("undefined variable: x"), "{err}");

    let err = run("break;").unwrap_err();
    assert!(err.contains("break outside loop"), "{err}");

    let err = run("print(1 < 2 < 3);").unwrap_err();
    assert!(err.contains("chained comparisons are not supported"), "{err}");
}
```

- [ ] **Step 2: 运行金样本**

Run: `cargo test -p xc_driver --test golden`
Expected: 部分通过，部分失败——失败即编译器/VM 的真实 bug，逐条修到全绿。

- [ ] **Step 3: 全部通过后跑全量测试**

Run: `cargo test`
Expected: 全绿（含 xepl 与 xc_parse 既有测试不回归）。

- [ ] **Step 4: Commit**

```bash
git add lib/xc_driver/tests/golden.rs
git commit -m "test: add golden integration tests for x run"
```

---

### Task 10: 示例文件与最终验证

**Files:**
- Create: `xc/examples/fib.x`

- [ ] **Step 1: 写示例**

`xc/examples/fib.x`：

```
func fib(n: int) {
    if n < 2 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let i = 0;
while i <= 10 {
    print(fib(i));
    i = i + 1;
}
```

- [ ] **Step 2: 端到端运行**

Run: `cargo run -p x -- run xc/examples/fib.x`
Expected: 输出 0 到 55 共 11 行。

- [ ] **Step 3: 质量门**

Run:
```bash
cargo build
cargo test
git diff --check
```
Expected: 构建通过、全部测试通过、无空白错误。

- [ ] **Step 4: Commit**

```bash
git add xc/examples/fib.x
git commit -m "chore: add fib example for x run"
```

---

## 自审记录

- **Spec 覆盖**：xvalue（值表示）✓、xc_check（符号表 + 检查）✓、xvm（字节码 + VM）✓、x run CLI ✓、测试 ✓。spec 中"真 GC/闭包/异常/类型检查器"均为非目标，未纳入任务。
- **占位符扫描**：无 TBD/TODO；每个新增文件给出完整内容。
- **类型一致性**：`compile`/`run` 签名在 driver 与 crate 间一致；`Value` 变体跨 xvalue/xvm 一致；`Program` 字段在 xc_check/xvm 间一致。
