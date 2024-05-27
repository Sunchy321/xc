use crate::token::Delimiter;
use crate::tokenstream::{DelimSpan, TokenStream};

#[derive(Clone, Debug)]
pub struct DelimitedArgs {
    pub delim_span: DelimSpan,
    pub delim: Delimiter,
    pub tokens: TokenStream
}