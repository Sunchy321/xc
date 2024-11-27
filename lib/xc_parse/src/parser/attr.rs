use thin_vec::ThinVec;
use xc_ast::ast::DelimitedArgs;
use xc_ast::attr::{AttrArgs, AttrKind, AttrStyle, Attribute, NormalAttr};
use xc_ast::ptr::P;
use xc_ast::token::{Delimiter, TokenKind};
use xc_ast::tokenstream::TokenTree;

use super::{parser::Parser, ParseResult};

impl<'a> Parser<'a> {
    pub(super) fn parse_outer_attrs(&mut self) -> ParseResult<'a, ThinVec<Attribute>> {
        let mut attrs = ThinVec::new();

        loop {
            let attr = if self.check(&TokenKind::At) {
                // TODO: check inner attribute

                Some(self.parse_attribute()?)
            } else {
                None
            };

            if let Some(attr) = attr {
                if attr.style == AttrStyle::Outer {
                    attrs.push(attr);
                }
            } else {
                break;
            }
        }

        Ok(attrs)
    }

    pub fn parse_attribute(&mut self) -> ParseResult<'a, Attribute> {
        let lo = self.token.span;

        self.collect_tokens_no_attr(|this| {
            assert!(
                this.eat(&TokenKind::At),
                "no @ found while parsing attribute"
            );

            let style = if this.eat(&TokenKind::Colon) {
                AttrStyle::Inner
            } else {
                AttrStyle::Outer
            };

            let ident = this.parse_identifier()?;
            let args = this.parse_attr_args()?;

            let span = lo.to(this.prev_token.span);

            let normal_attr = NormalAttr {
                name: ident.name,
                args,
            };

            let kind = AttrKind::Normal(P(normal_attr));

            let attr = Attribute { kind, style, span };

            Ok(attr)
        })
    }

    pub fn parse_attr_args(&mut self) -> ParseResult<'a, AttrArgs> {
        let args = if let Some(args) = self.parse_delimited_args() {
            AttrArgs::Delimited(args)
        } else {
            AttrArgs::Empty
        };

        Ok(args)
    }

    fn parse_delimited_args(&mut self) -> Option<DelimitedArgs> {
        let delimited = self.check(&TokenKind::OpenDelim(Delimiter::Paren))
            || self.check(&TokenKind::OpenDelim(Delimiter::Bracket))
            || self.check(&TokenKind::OpenDelim(Delimiter::Brace));

        delimited.then(|| {
            let TokenTree::Delimited(tokens, delim, span, _) = self.parse_token_tree() else {
                unreachable!()
            };

            DelimitedArgs {
                delim_span: span,
                delim,
                tokens,
            }
        })
    }

    fn parse_token_tree(&mut self) -> TokenTree {
        match self.token.kind {
            TokenKind::OpenDelim(_) => {
                let stream = self.token_cursor.tree_cursor.stream.clone();

                let (_, delim, span, spacing) = *self.token_cursor.stack.last().unwrap();

                let target_depth = self.token_cursor.stack.len() - 1;

                loop {
                    self.next();

                    if self.token_cursor.stack.len() == target_depth {
                        debug_assert!(matches!(self.token.kind, TokenKind::CloseDelim(..)));
                        break;
                    }
                }

                self.next();
                TokenTree::Delimited(stream, delim, span, spacing)
            }
            TokenKind::CloseDelim(_) | TokenKind::Eof => unreachable!(),
            _ => {
                let prev_spacing = self.spacing;

                self.next();

                TokenTree::Token(self.prev_token.clone(), prev_spacing)
            }
        }
    }

    pub fn collect_tokens_no_attr<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> ParseResult<'a, R>,
    ) -> ParseResult<'a, R> {
        self.collect_tokens(ThinVec::new(), ForceCollect::Yes, |this, _attrs| {
            Ok((f(this)?, TrailingToken::None))
        })
    }

    pub fn collect_tokens<R>(
        &mut self,
        attrs: ThinVec<Attribute>,
        force_collect: ForceCollect,
        f: impl FnOnce(&mut Self, ThinVec<Attribute>) -> ParseResult<'a, (R, TrailingToken)>,
    ) -> ParseResult<'a, R> {
        // TODO: Complete function

        let (ret, trailing) = f(self, attrs)?;

        Ok(ret)
    }
}

#[derive(Clone, Debug)]
pub enum ForceCollect {
    Yes,
    No,
}

#[derive(Clone, Debug)]
pub enum TrailingToken {
    None,
    Semicolon,
    Greater,
    MaybeComma,
}
