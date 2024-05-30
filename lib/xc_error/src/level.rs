use termcolor::{Color, ColorSpec};
use self::Level::*;


#[derive(Clone, Copy, Debug)]
pub enum Level {
    Fatal,
    Error,
    Warning,
    Note,
}

impl Level {
    fn color(self) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match self {
            Fatal | Error => {
                spec.set_fg(Some(Color::Red)).set_intense(true);
            }
            Warning => {
                spec.set_fg(Some(Color::Yellow)).set_intense(cfg!(windows));
            }
            Note => {
                spec.set_fg(Some(Color::Green)).set_intense(true);
            }
        }
        spec
    }

    pub fn to_str(self) -> &'static str {
        match self {
            Fatal | Error => "error",
            Warning => "warning",
            Note => "note",
        }
    }
}