use std::mem;

use xc_ast::token::{Delimiter, Token, TokenKind};
use xc_ast::tokenstream::{DelimSpacing, DelimSpan, Spacing, TokenTree, TokenTreeCursor};
use xc_span::Span;

#[derive(Clone)]
pub(crate) struct TokenCursor {
    pub(crate) tree_cursor: TokenTreeCursor,

    pub(crate) stack: Vec<(TokenTreeCursor, Delimiter, DelimSpan, DelimSpacing)>,
}

impl TokenCursor {
    pub(crate) fn next(&mut self) -> (Token, Spacing) {
        loop {
            if let Some(tree) = self.tree_cursor.next_ref() {
                match tree {
                    &TokenTree::Token(ref token, spacing) => {
                        debug_assert!(!matches!(
                            token.kind,
                            TokenKind::OpenDelim(..) | TokenKind::CloseDelim(..)
                        ));

                        return (token.clone(), spacing);
                    }

                    &TokenTree::Delimited(ref stream, delim, span, spacing) => {
                        let trees = stream.clone().into_trees();

                        self.stack.push((
                            mem::replace(&mut self.tree_cursor, trees),
                            delim,
                            span,
                            spacing,
                        ));

                        if delim != Delimiter::Invisible {
                            return (
                                Token { kind: TokenKind::OpenDelim(delim), span: span.open },
                                spacing.open,
                            )
                        }
                    }
                }
            } else if let Some((cursor, delim, span, spacing)) = self.stack.pop() {
                self.tree_cursor = cursor;

                if delim != Delimiter::Invisible {
                    return (
                        Token { kind: TokenKind::CloseDelim(delim), span: span.close },
                        spacing.close
                    );
                }
            } else {
                return (Token { kind: TokenKind::Eof, span: Span::DUMMY }, Spacing::Alone);
            }
        }
    }
}
