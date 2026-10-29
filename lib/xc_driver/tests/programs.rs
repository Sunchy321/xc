//! X 程序语料测试：`tests/programs/*.x` 一文件一用例。
//!
//! 约定见 `tests/programs/README.md`：`// expect:` 按序匹配 print 输出行，
//! `// error:` 匹配错误消息子串；两者至少其一。新语言特性落地时在此目录
//! 补用例——这是跨里程碑的长期回归面。

use std::fs;
use std::path::PathBuf;

use xc_driver::run_source;
use xc_span::source_map::Filename;

fn program_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/programs")
}

#[test]
fn programs_suite() {
    let dir = program_dir();
    let mut entries: Vec<PathBuf> = fs::read_dir(&dir)
        .expect("programs directory exists")
        .map(|e| e.expect("readable entry").path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "x"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no .x programs found");

    let mut failures = Vec::new();
    for path in &entries {
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        let src = fs::read_to_string(path).expect("readable program");
        let (expects, errors) = parse_annotations(&src);
        if expects.is_empty() && errors.is_empty() {
            failures.push(format!(
                "{name}: no `// expect:` or `// error:` annotations"
            ));
            continue;
        }
        let result = run_source(Filename::Normal(name.to_string()), &src);
        match (errors.first(), result) {
            (Some(needle), Ok(out)) => failures.push(format!(
                "{name}: expected error containing {needle:?}, got output {out:?}"
            )),
            (Some(needle), Err(msg)) => {
                if !msg.contains(needle.as_str()) {
                    failures.push(format!(
                        "{name}: error {msg:?} does not contain {needle:?}"
                    ));
                }
            }
            (None, Err(msg)) => failures.push(format!("{name}: unexpected error {msg:?}")),
            (None, Ok(out)) => {
                let lines: Vec<&str> = out.lines().collect();
                let want: Vec<&str> = expects.iter().map(|s| s.as_str()).collect();
                if lines != want {
                    failures.push(format!("{name}: output {lines:?} != expected {want:?}"));
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

fn parse_annotations(src: &str) -> (Vec<String>, Vec<String>) {
    let mut expects = Vec::new();
    let mut errors = Vec::new();
    for line in src.lines() {
        let line = line.trim_start();
        if let Some(rest) = line.strip_prefix("// expect: ") {
            expects.push(rest.to_string());
        } else if let Some(rest) = line.strip_prefix("// error: ") {
            errors.push(rest.to_string());
        }
    }
    (expects, errors)
}
