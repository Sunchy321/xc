use std::rc::Rc;

use crate::fatal::FatalError;
use crate::source_file::{HashAlgorithm, OffsetOverflowError, SourceFile};
use crate::{BytePos, Span};

pub enum Filename {
    Normal(String),
    Repl
}

pub struct SourceMap {
    hash_kind: HashAlgorithm,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            hash_kind: HashAlgorithm::MD5,
        }
    }

    pub fn new_source_file(&self, filename: Filename, source: String) -> Rc<SourceFile> {
        self.try_new_source_file(filename, source)
            .unwrap_or_else(|OffsetOverflowError| FatalError::raise())
    }

    fn try_new_source_file(
        &self,
        filename: Filename,
        source: String,
    ) -> Result<Rc<SourceFile>, OffsetOverflowError> {
        let source_file = SourceFile::new(filename, source, self.hash_kind)?;

        Ok(Rc::new(source_file))
    }

    pub fn start_point(&self, span: Span) -> Span {
        // TODO: consider multibyte character
        span.with_hi(span.lo + BytePos(1))
    }
}
