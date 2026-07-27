mod eval;
mod store;
mod r#type;
mod value;

use std::borrow::Cow;
use std::io::Write;

use eval::eval;
use rustyline::config::Config;
use rustyline::highlight::Highlighter;
use rustyline::history::DefaultHistory;
use rustyline::{Completer, Editor, Helper, Hinter, Validator};
use store::Store;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use value::println_value;
use xc_parse::parser_from_source_str;
use xc_parse::session::ParseSession;
use xc_span::create_session_globals_then;
use xc_span::source_map::Filename;

fn create_spec(color: Color) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(color));
    spec.set_bold(true);
    spec
}

#[derive(Completer, Helper, Hinter, Validator)]
struct ReplHelper;

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        highlight_x(line)
    }

    fn highlight_char(&self, line: &str, _pos: usize, _kind: rustyline::highlight::CmdKind) -> bool {
        true
    }
}

fn highlight_x(input: &str) -> Cow<'_, str> {
    let mut result = String::with_capacity(input.len() + 64);

    let keywords: &[&str] = &[
        "func", "let", "if", "else", "return", "throw", "try", "catch", "break", "continue",
        "while", "for", "match", "async", "const", "unsafe", "extern", "this", "nil", "true",
        "false", "mut", "operator", "in", "is", "as", "do", "class", "trait", "impl", "type",
    ];

    let mut chars = input.chars().peekable();
    while let Some(&ch) = chars.peek() {
        if ch == '/' && input.len() > 0 {
            // Check for comments
            let rest: String = chars.clone().take(2).collect();
            if rest == "//" {
                let comment: String = chars.by_ref().collect();
                result.push_str("\x1b[90m"); // gray
                result.push_str(&comment);
                result.push_str("\x1b[0m");
                break;
            }
        }

        if ch == '"' {
            let mut s = String::new();
            s.push(chars.next().unwrap()); // opening "
            for ch in chars.by_ref() {
                s.push(ch);
                if ch == '"' {
                    break;
                }
            }
            result.push_str("\x1b[32m"); // green
            result.push_str(&s);
            result.push_str("\x1b[0m");
            continue;
        }

        if ch.is_ascii_digit() {
            let mut num = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_ascii_digit() || ch == '.' || ch == 'x' || ch == 'b' || ch == '_' {
                    num.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            result.push_str("\x1b[36m"); // cyan
            result.push_str(&num);
            result.push_str("\x1b[0m");
            continue;
        }

        if ch.is_alphabetic() || ch == '_' {
            let mut word = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_alphanumeric() || ch == '_' {
                    word.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            if keywords.contains(&word.as_str()) {
                result.push_str("\x1b[1;34m"); // bold blue
                result.push_str(&word);
                result.push_str("\x1b[0m");
            } else {
                result.push_str(&word);
            }
            continue;
        }

        result.push(chars.next().unwrap());
    }

    Cow::Owned(result)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let hint = create_spec(Color::Ansi256(8));
    let error = create_spec(Color::Red);

    let session = ParseSession::new();
    let mut store = Store::new();
    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    let config = Config::default();
    let h = ReplHelper;
    let mut rl = Editor::<ReplHelper, DefaultHistory>::with_config(config)?;
    rl.set_helper(Some(h));

    create_session_globals_then::<Result<(), Box<dyn std::error::Error>>>(|| loop {
        let line = match rl.readline("> ") {
            Ok(line) => line,
            Err(_) => return Ok(()),
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed == ":q" || trimmed == ":quit" {
            return Ok(());
        }

        let name = Filename::Repl;

        // Wrap input in { } to support both expressions and statements
        let source = format!("{{ {} }}", trimmed);

        let mut parser = parser_from_source_str(&session, name, source);

        let expr = match parser.parse_expr() {
            Ok(expr) => expr,
            Err(_) => {
                stdout.set_color(&error)?;
                writeln!(stdout, "Error while parsing!")?;
                stdout.reset()?;
                continue;
            }
        };

        let result = match eval(&*expr, &mut store) {
            Ok(result) => result,
            Err(err) => {
                stdout.set_color(&error)?;
                writeln!(stdout, "Error while evaluating: {}", err)?;
                stdout.reset()?;
                continue;
            }
        };

        println_value(result, &mut stdout, &hint)?;
    })
}
