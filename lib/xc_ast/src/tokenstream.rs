use std::rc::Rc;

use xc_span::Span;

use crate::token::{Delimiter, Token};

#[derive(Clone)]
pub enum TokenTree {
    Token(Token, Spacing),

    Delimited(TokenStream, Delimiter, DelimSpan, DelimSpacing),
}

#[derive(Clone)]
pub struct TokenStream(pub(crate) Rc<Vec<TokenTree>>);

impl TokenStream {
    pub fn new(streams: Vec<TokenTree>) -> Self {
        Self(Rc::new(streams))
    }

    pub fn into_trees(self) -> TokenTreeCursor {
        TokenTreeCursor::new(self)
    }
}

#[derive(Clone)]
pub struct TokenTreeCursor {
    pub stream: TokenStream,
    index: usize,
}

impl TokenTreeCursor {
    fn new(stream: TokenStream) -> Self {
        Self {
            stream,
            index: 0,
        }
    }

    pub fn next_ref(&mut self) -> Option<&TokenTree> {
        self.stream.0.get(self.index).map(|tree| {
            self.index += 1;
            tree
        })
    }

    pub fn look_ahead(&self, n: usize) -> Option<&TokenTree> {
        self.stream.0.get(self.index + n)
    }
}

#[derive(Clone)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Clone)]
pub struct DelimSpan {
    pub open: Span,
    pub close: Span,
}

impl DelimSpan {
    pub fn from(span: Span) -> Self {
        Self {
            open: span,
            close: span,
        }
    }

    pub fn from_pair(open: Span, close: Span) -> Self {
        Self { open, close }
    }
}

#[derive(Clone)]
pub struct DelimSpacing {
    pub open: Spacing,
    pub close: Spacing,
}

impl DelimSpacing {
    pub fn new(open: Spacing, close: Spacing) -> DelimSpacing {
        DelimSpacing { open, close }
    }
}
