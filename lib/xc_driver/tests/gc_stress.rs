//! GC 压力测试：XVM_GC_EVERY_ALLOC=1 时每个分配 safepoint 都回收——
//! 任何漏根都会让存活对象被清扫，程序要么崩溃要么输出错误。全语料
//! 必须在压力模式下逐字节复现普通模式的结果。

mod common;

use common::all_cases;
use xc_driver::run_source;
use xc_span::source_map::Filename;

#[test]
fn programs_corpus_under_gc_stress() {
    // SAFETY: 本测试目标为独立进程；环境变量在用例运行前设置一次。
    unsafe {
        std::env::set_var("XVM_GC_EVERY_ALLOC", "1");
    }
    let mut failures = Vec::new();
    for case in all_cases() {
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
        "{} failure(s) under GC stress:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
