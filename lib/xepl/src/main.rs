mod eval;
mod store;
mod r#type;
mod value;

use std::io::{stdin, stdout, Write};

use eval::eval;
use store::Store;
use xc_parse::parser_from_source_str;
use xc_parse::session::ParseSession;
use xc_span::source_map::Filename;
use xc_span::create_session_globals_then;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let session = ParseSession::new();

    let store = Store::new();

    create_session_globals_then::<Result<(), Box<dyn std::error::Error>>>(|| loop {
        print!("> ");

        stdout().flush()?;

        let mut source = String::new();

        stdin().read_line(&mut source)?;

        if source == "" {
            continue;
        }

        let name = Filename::Repl;

        let mut parser = parser_from_source_str(&session, name, source);

        let Ok(expr) = parser.parse_expr() else {
            println!("Error while parsing!");
            continue;
        };

        let Ok(result) = eval(&*expr) else {
            println!("Error while evaluating!");
            continue;
        };

        println!("{}", result);
    })
}
