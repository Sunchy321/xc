//! X 程序语料测试：`tests/programs/*.x` 一文件一用例。
//! 每个文件用 `// expect:` 按序匹配 print 输出，或用 `// error:` 匹配错误消息；
//! 两者至少其一。新语言特性落地时在此目录补用例，这是跨里程碑的长期回归面。

mod common;

use common::all_cases;
use xc_driver::run_source;
use xc_span::source_map::Filename;

#[test]
fn programs_suite() {
    let mut failures = Vec::new();
    for case in all_cases() {
        if case.expects.is_empty() && case.errors.is_empty() {
            failures.push(format!(
                "{}: no `// expect:` or `// error:` annotations",
                case.name
            ));
            continue;
        }
        let result = run_source(Filename::Normal(case.name.clone()), &case.src);
        match (case.errors.first(), result) {
            (Some(needle), Ok(out)) => failures.push(format!(
                "{}: expected error containing {needle:?}, got output {out:?}",
                case.name
            )),
            (Some(needle), Err(msg)) => {
                if !msg.contains(needle.as_str()) {
                    failures.push(format!(
                        "{}: error {msg:?} does not contain {needle:?}",
                        case.name
                    ));
                }
            }
            (None, Err(msg)) => failures.push(format!("{}: unexpected error {msg:?}", case.name)),
            (None, Ok(out)) => {
                let lines: Vec<&str> = out.lines().collect();
                let want: Vec<&str> = case.expects.iter().map(|s| s.as_str()).collect();
                if lines != want {
                    failures.push(format!(
                        "{}: output {lines:?} != expected {want:?}",
                        case.name
                    ));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} program failure(s):\n{}",
        failures.len(),
        failures.join("\n")
    );
}
