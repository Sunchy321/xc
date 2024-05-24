use std::rc::Rc;

pub enum Filename {
    Normal(String),
}

pub struct SourceFile {
    /// The name of the source file.
    pub name: Filename,

    /// The source code of the file.
    pub source: Option<Rc<String>>,
}

pub struct SourceMap {

}

impl SourceMap {
    pub fn new() -> Self {
        Self {}
    }
}