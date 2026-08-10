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
use xc_ast::decl::DeclKind;
use xc_lexer::cursor::Cursor;
use xc_lexer::{LiteralKind, TokenKind};
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

    fn highlight_char(&self, _line: &str, _pos: usize, _kind: rustyline::highlight::CmdKind) -> bool {
        true
    }
}

fn highlight_x(input: &str) -> Cow<'_, str> {
    // Phase 1: tokenize
    struct Tok<'a> { text: &'a str, kind: TokenKind, pos: usize }
    let mut tokens: Vec<Tok> = Vec::new();
    let mut cursor = Cursor::new(input);
    let mut pos = 0usize;
    loop {
        let token = cursor.next_token();
        if matches!(token.kind, TokenKind::Eof) { break; }
        let len = token.len as usize;
        tokens.push(Tok { text: &input[pos..pos + len], kind: token.kind, pos });
        pos += len;
    }

    // Phase 2: mark which identifiers should be highlighted as keywords
    let mut highlight = vec![false; tokens.len()];
    let hard_kw: &[&str] = &[
        "func", "let", "if", "else", "return", "throw", "try", "catch", "break", "continue",
        "while", "for", "match", "async", "const", "unsafe", "extern", "this", "nil", "true",
        "false", "mut", "operator", "in", "is", "as", "do", "class", "trait", "impl", "type",
        "defer", "linear",
    ];
    let ctx_kw: &[&str] = &["then", "get", "set", "didSet", "willSet", "lazy", "once",
                            "root", "super", "infix", "prefix", "suffix"];

    for i in 0..tokens.len() {
        let t = &tokens[i];
        if !matches!(t.kind, TokenKind::Identifier) { continue; }

        let is_hard_kw = hard_kw.iter().any(|&kw| kw == t.text);
        let is_ctx_kw = ctx_kw.iter().any(|&kw| kw == t.text);

        if is_hard_kw {
            highlight[i] = true;
        } else if is_ctx_kw {
            let prev_is = |text: &str| i > 0 && tokens[i - 1].text == text;
            let next_is = |text: &str| i + 1 < tokens.len() && tokens[i + 1].text == text;

            let should_highlight = match t.text {
                "then" => prev_is("if"),
                "get" | "set" | "didSet" | "willSet" => prev_is("{") || prev_is("}"),
                "lazy" => prev_is("("),
                "once" => prev_is("do") || prev_is("{") || prev_is(","),
                "root" | "super" => next_is("::"),
                "infix" | "prefix" | "suffix" => prev_is("operator"),
                _ => false,
            };

            highlight[i] = should_highlight;
        }
    }

    // Phase 3: render with colors
    let kw_blue = "\x1b[1;34m";
    let num_cyan = "\x1b[36m";
    let str_green = "\x1b[32m";
    let comment_gray = "\x1b[90m";
    let reset = "\x1b[0m";

    let mut result = String::with_capacity(input.len() + 64);
    let mut prev_end = 0usize;
    for (i, t) in tokens.iter().enumerate() {
        // Copy any whitespace between tokens
        if t.pos > prev_end {
            result.push_str(&input[prev_end..t.pos]);
        }
        prev_end = t.pos + t.text.len();

        let colored = match &t.kind {
            TokenKind::Identifier => {
                if highlight[i] {
                    format!("{}{}{}", kw_blue, t.text, reset)
                } else {
                    t.text.to_string()
                }
            }
            TokenKind::Literal { kind, .. } => match kind {
                LiteralKind::Integer { .. } | LiteralKind::Floating { .. } => {
                    format!("{}{}{}", num_cyan, t.text, reset)
                }
                LiteralKind::Char { .. } | LiteralKind::String { .. } | LiteralKind::RawString { .. } => {
                    format!("{}{}{}", str_green, t.text, reset)
                }
                LiteralKind::NameLit => format!("{}{}{}", num_cyan, t.text, reset),
            },
            TokenKind::LineComment | TokenKind::BlockComment { .. } => {
                format!("{}{}{}", comment_gray, t.text, reset)
            }
            _ => t.text.to_string(),
        };
        result.push_str(&colored);
    }
    if prev_end < input.len() {
        result.push_str(&input[prev_end..]);
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

        // Handle function definitions
        if trimmed.starts_with("func ") {
            let mut parser = parser_from_source_str(&session, name, trimmed.to_string());
            match parser.parse_decl() {
                Ok(Some(decl)) => {
                    if let DeclKind::Func(func) = &decl.kind {
                        let fname = func.ident.name;
                        store.add_func(func.clone());
                        stdout.set_color(&hint)?;
                        writeln!(stdout, "<function {}>", fname.as_str())?;
                        stdout.reset()?;
                    }
                }
                Err(_) => {
                    stdout.set_color(&error)?;
                    writeln!(stdout, "Error while parsing function!")?;
                    stdout.reset()?;
                }
                _ => {}
            }
            continue;
        }

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
