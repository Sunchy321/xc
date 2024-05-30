use xc_parse::{from_source_str, session::ParseSession};
use xc_span::{create_session_globals_then, source_map::Filename};

fn main() {
    create_session_globals_then(|| {
        let session = ParseSession::new();

        let source = String::from("123");

        let mut parser = from_source_str(&session, Filename::Normal(String::new()), source, None);

        let expr = parser.parse_expr();

        match expr {
            Ok(expr) => println!("{:?}", expr),
            Err(_) => panic!(),
        }
    });
}
