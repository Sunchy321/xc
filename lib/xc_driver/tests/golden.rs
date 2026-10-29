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
    assert_eq!(run("let mut x = 5; x = x + 1; print(x);").unwrap(), "6\n");
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
let mut i = 0;
let mut sum = 0;
while i < 10 { sum = sum + i; i = i + 1; }
print(sum);
";
    assert_eq!(run(src).unwrap(), "45\n");
}

#[test]
fn for_loop_over_array() {
    let src = "\
for x: [1, 2, 3] { print(x); }
print(\"done\");
";
    assert_eq!(run(src).unwrap(), "1\n2\n3\ndone\n");
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
    // 拼接是 `~`（CONCAT）；`+` 仅算术，对 string 无定义。
    assert_eq!(run("print(\"a\" ~ \"b\");").unwrap(), "ab\n");
    let err = run("print(\"a\" + \"b\");").unwrap_err();
    assert!(err.contains("arithmetic operands must be int"), "{err}");
}

#[test]
fn logic_short_circuit() {
    assert_eq!(run("print(1 < 2 && 2 < 3);").unwrap(), "true\n");
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
let mut i = 0;
while true { i = i + 1; if i >= 3 { break; } }
print(i);
";
    assert_eq!(run(src).unwrap(), "3\n");
}

#[test]
fn continue_in_while() {
    let src = "\
let mut i = 0;
let mut sum = 0;
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
