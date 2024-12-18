mod eval;
mod store;
mod r#type;
mod value;

use std::io::{stdin, Write};

use eval::eval;
use store::Store;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use value::println_value;
use xc_parse::parser_from_source_str;
use xc_parse::session::ParseSession;
use xc_span::source_map::Filename;
use xc_span::create_session_globals_then;

fn with_fg(color: Color) -> ColorSpec {
    let mut spec = ColorSpec::new();

    spec.set_fg(Some(color));

    spec
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let hint = with_fg(Color::Ansi256(8));
    let error = with_fg(Color::Red);

    let session = ParseSession::new();

    let store = Store::new();

    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    create_session_globals_then::<Result<(), Box<dyn std::error::Error>>>(|| loop {
        stdout.set_color(&hint)?;
        write!(stdout, "> ")?;
        stdout.reset()?;
        stdout.flush()?;

        let mut source = String::new();

        stdin().read_line(&mut source)?;

        if source == "" {
            continue;
        }

        let name = Filename::Repl;

        let mut parser = parser_from_source_str(&session, name, source);

        let Ok(expr) = parser.parse_expr() else {
            stdout.set_color(&error)?;
            writeln!(stdout, "Error while parsing!")?;
            stdout.reset()?;
            continue;
        };

        let Ok(result) = eval(&*expr) else {
            stdout.set_color(&error)?;
            writeln!(stdout, "Error while evaluating!")?;
            stdout.reset()?;
            continue;
        };

        println_value(result, &mut stdout, &hint)?;
    })
}
