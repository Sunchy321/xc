use xc_ast::{token::{Delimiter, Token, TokenKind}, tokenstream::{DelimSpacing, DelimSpan, TokenStream, TokenTree}};
use xc_ast::tokenstream::Spacing;
use xc_error::diag::Diagnostic;

use super::lexer::Lexer;

pub struct TokenTreeReader<'a, 'src> {
    lexer: Lexer<'a, 'src>,
    token: Token,
}

impl<'a, 'src> TokenTreeReader<'a, 'src> {
    pub(crate) fn parse_all_token_tree(
        lexer: Lexer<'a, 'src>
    ) -> (TokenStream, Result<(), Vec<Diagnostic<'a>>>) {
        let mut reader = Self {
            lexer,
            token: Token::dummy()
        };

        let (stream, _spacing, res) = reader.parse_token_tree(false);

        (stream, res)
    }

    fn parse_token_tree(&mut self, is_delimited: bool) -> (TokenStream, Spacing, Result<(), Vec<Diagnostic<'a>>>) {
        let (_, open_spacing) = self.next();

        let mut buffer = Vec::new();

        loop {
            match self.token.kind {
                TokenKind::OpenDelim(delim) => {
                    buffer.push(match self.parse_token_tree_with_delim(delim) {
                        Ok(val) => val,
                        Err(errs) => return (TokenStream::new(buffer), open_spacing, Err(errs)),
                    })
                }
                TokenKind::CloseDelim(delim) => {
                    return (
                        TokenStream::new(buffer),
                        open_spacing,
                        if is_delimited {
                            Ok(())
                        } else {
                            Err(vec![self.raise_unexpected_close_delim(delim)])
                        }
                    )
                }
                TokenKind::Eof => {
                    return (
                        TokenStream::new(buffer),
                        open_spacing,
                        if is_delimited {
                            Err(vec![self.raise_unexpected_eof()])
                        } else {
                            Ok(())
                        }
                    )
                }
                _ => {
                    let (this_token, this_spacing) = self.next();

                    buffer.push(TokenTree::Token(this_token, this_spacing));
                }
            }
        }
    }

    fn next(&mut self) -> (Token, Spacing) {
        let (next_token, spacing) = loop {
            let (next_token, is_precedence_by_space) = self.lexer.next_token();

            if is_precedence_by_space {
                break (next_token, Spacing::Alone);
            } else {
                let spacing = match next_token.kind {
                    ref k if k.is_punc() => Spacing::Joint,
                    TokenKind::Eof => Spacing::Alone,
                    _ => Spacing::Alone, // TODO: rust use JointHidden here
                };

                break (next_token, spacing);
            }
        };

        let this_tok = std::mem::replace(&mut self.token, next_token);

        (this_tok, spacing)
    }

    fn parse_token_tree_with_delim(&mut self, open: Delimiter) -> Result<TokenTree, Vec<Diagnostic<'a>>> {
        let pre_span = self.token.span;

        let (tts, open_spacing, res) = self.parse_token_tree(true);

        if let Err(errs) = res {
            return Err(self.raise_unclosed_delim(tts, errs));
        }

        let delim_span = DelimSpan::from_pair(pre_span, self.token.span);

        let close_spacing = match self.token.kind {
            TokenKind::CloseDelim(close) if close == open => {
                self.next().1
            }

            TokenKind::CloseDelim(close) => {
                // TODO: unimplemented
                Spacing::Alone
            }

            TokenKind::Eof => {
                Spacing::Alone
            }

            _ => unreachable!()
        };

        let delim_spacing = DelimSpacing::new(open_spacing, close_spacing);

        Ok(TokenTree::Delimited(tts, open, delim_span, delim_spacing))
    }
}