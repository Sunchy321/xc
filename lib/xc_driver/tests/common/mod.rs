//! 程序语料共享工具：注解解析与语料遍历（programs / gc_stress 共用）。

use std::fs;
use std::path::PathBuf;

pub struct Case {
    pub name: String,
    pub src: String,
    /// `// expect:` 注解，按序对应 print 输出行。
    pub expects: Vec<String>,
    /// `// error:` 注解：错误消息子串。
    pub errors: Vec<String>,
}

pub fn program_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/programs")
}

pub fn all_cases() -> Vec<Case> {
    let mut entries: Vec<PathBuf> = fs::read_dir(program_dir())
        .expect("programs directory exists")
        .map(|e| e.expect("readable entry").path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "x"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no .x programs found");
    entries
        .into_iter()
        .map(|path| {
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            let src = fs::read_to_string(&path).expect("readable program");
            let (expects, errors) = parse_annotations(&src);
            Case {
                name,
                src,
                expects,
                errors,
            }
        })
        .collect()
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
