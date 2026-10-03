use xc_check::{check, parse_file, parse_program};
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
    } else if args.len() >= 3 && args[1] == "parse" {
        let path = &args[2];
        match std::fs::read_to_string(path) {
            Ok(source) => {
                match create_session_globals_then(|| {
                    parse_file(Filename::Normal(path.clone()), &source)
                }) {
                    Ok(decls) => {
                        println!("ok: {} declarations", decls.len());
                        std::process::exit(0);
                    }
                    Err(err) => {
                        eprintln!("{path}: {err}");
                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                eprintln!("error: cannot read {path}: {err}");
                std::process::exit(1);
            }
        }
    } else {
        eprintln!("usage: x run <file.x> | x parse <file.x>");
        std::process::exit(2);
    }
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
