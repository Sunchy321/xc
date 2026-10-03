use thin_vec::ThinVec;
use xc_ast::decl::{Qual, QualKind};
use xc_ast::module::{Constness, Safety};
use xc_ast::ptr::P;
use xc_ast::r#trait::{
    ConstConstraintItem, MemberItem, PropBody, PropConstraintItem, Trait, TypeConstraintItem,
};
use xc_ast::token::{Delimiter, TokenKind};
use xc_span::symbol::{kw, op};
use xc_span::Span;

use super::parser::Parser;
use super::{ParseResult, SequenceSeparator, TokenExpectType};

impl<'a> Parser<'a> {
    pub(crate) fn parse_trait(&mut self, quals: ThinVec<Qual>, _lo: Span) -> ParseResult<'a, Trait> {
        self.expect_keyword(kw::Trait)?;

        let ident = self.parse_identifier()?;
        let generics = self.parse_generic()?;

        let mut constness = Constness::Default;
        let mut safety = Safety::Default;

        for qual in &quals {
            match qual.kind {
                QualKind::Const => constness = Constness::Const(qual.span),
                QualKind::Unsafe => safety = Safety::Unsafe(qual.span),
                _ => {}
            }
        }

        // `trait A : B, C`
        let mut supertraits = ThinVec::new();

        if self.eat(&TokenKind::Colon) {
            let (parsed, ..) = self.parse_seq_before(
                &[&TokenKind::OpenDelim(Delimiter::Brace)],
                SequenceSeparator::new(TokenKind::Comma),
                TokenExpectType::Yes,
                |this| this.parse_type(),
            )?;

            supertraits = parsed;
        }

        self.expect(&TokenKind::OpenDelim(Delimiter::Brace))?;

        let mut items = ThinVec::new();

        loop {
            if self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
                break;
            }
            if self.token.kind == TokenKind::Eof {
                break;
            }

            items.push(self.parse_member_item()?);
        }

        Ok(Trait {
            safety,
            constness,
            ident,
            generics,
            supertraits,
            items,
        })
    }

    /// Parse one member of a trait body, impl block or class body.
    pub(crate) fn parse_member_item(&mut self) -> ParseResult<'a, MemberItem> {
        // Redundant separators between members are errors: a member ends with
        // its own `;`, and a block body needs no terminator at all.
        if self.check(&TokenKind::Semicolon) {
            let msg = if self.prev_token.kind == TokenKind::CloseDelim(Delimiter::Brace) {
                "a member with a block body is not followed by a semicolon"
            } else {
                "unexpected extra `;` between members"
            };

            return Err(self.diag_ctx().struct_span_error(self.token.span, msg));
        }

        if self.check(&TokenKind::Comma) {
            return Err(self.diag_ctx().struct_span_error(
                self.token.span,
                "members are not separated by `,`",
            ));
        }

        let _attrs = self.parse_outer_attrs()?;
        let lo = self.token.span;

        // `const NAME;` / `const NAME = expr;` — note the ambiguity with the
        // `const` qualifier (`const func`), resolved by lookahead.
        let is_const_item = self.token.is_keyword(kw::Const)
            && !self.is_keyword_ahead(1, &[kw::Func, kw::Let]);

        if is_const_item {
            self.next();

            let ident = self.parse_identifier()?;

            let value = if self.eat_operator(op::Assign) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            self.expect_semi()?;

            let span = lo.to(self.prev_token.span);

            return Ok(MemberItem::Const(ConstConstraintItem {
                ident,
                value,
                span,
            }));
        }

        let quals = self.parse_qual_list()?;

        if self.token.is_keyword(kw::Type) {
            self.next();

            let ident = self.parse_identifier()?;

            let bound = if self.eat(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let value = if self.eat_operator(op::Assign) {
                Some(self.parse_type()?)
            } else {
                None
            };

            self.expect_semi()?;

            let span = lo.to(self.prev_token.span);

            return Ok(MemberItem::Type(TypeConstraintItem {
                ident,
                bound,
                value,
                span,
            }));
        }

        if self.token.is_keyword(kw::Func) || self.token.is_keyword(kw::Init) {
            if self.token.is_keyword(kw::Func) {
                self.eat_keyword(kw::Func);
            }

            let func = self.parse_func(quals, lo)?;

            return Ok(MemberItem::Func(P(func)));
        }

        if self.eat_keyword(kw::Let) {
            let ident = self.parse_identifier()?;

            let ty = if self.eat(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let body = if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
                self.next();

                let mut blocks = ThinVec::new();

                loop {
                    if self.eat(&TokenKind::CloseDelim(Delimiter::Brace)) {
                        break;
                    }
                    if self.token.kind == TokenKind::Eof {
                        break;
                    }

                    if self.eat_keyword_noexpect(kw::Get)
                        || self.eat_keyword_noexpect(kw::Set)
                        || self.eat_keyword_noexpect(kw::WillSet)
                        || self.eat_keyword_noexpect(kw::DidSet)
                    {
                        if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
                            blocks.push(self.parse_block()?);
                        } else if self.eat(&TokenKind::FatArrow) {
                            let _ = self.parse_expr()?;
                            self.expect_semi()?;
                        } else {
                            self.eat(&TokenKind::Semicolon);
                        }
                    } else if self.check(&TokenKind::OpenDelim(Delimiter::Brace)) {
                        blocks.push(self.parse_block()?);
                    } else {
                        return Err(self.expected_tokens_error("expected"));
                    }
                }

                // A property directly followed by its accessor block needs no
                // trailing `;` — writing one is an error.
                if self.check(&TokenKind::Semicolon) {
                    return Err(self.diag_ctx().struct_span_error(
                        self.token.span,
                        "a property with an accessor block is not followed by a semicolon",
                    ));
                }

                PropBody::Accessor(blocks)
            } else if self.eat(&TokenKind::FatArrow) {
                let expr = self.parse_expr()?;
                self.expect_semi()?;
                PropBody::Expr(expr)
            } else if self.eat_operator(op::Assign) {
                let expr = self.parse_expr()?;
                self.expect_semi()?;
                PropBody::Expr(expr)
            } else {
                self.expect_semi()?;
                PropBody::Semicolon
            };

            let span = lo.to(self.prev_token.span);

            return Ok(MemberItem::Prop(PropConstraintItem {
                ident,
                ty,
                body,
                span,
            }));
        }

        Err(self.expected_tokens_error("expected"))
    }
}
