use std::ops::Mul;

use thin_vec::ThinVec;
use xc_span::symbol::op::MultiplyAssign;
use xc_span::{Identifier, Span};

use crate::attr::Attribute;
use crate::decl::Qual;
use crate::expr::Expr;
use crate::pattern::{BindKey, Pattern, PatternKind};
use crate::ptr::P;
use crate::stmt::Block;
use crate::ty::{Type, TypeKind};
use crate::Mutability;

#[derive(Clone, Debug)]
pub struct Func {
    pub sig: P<FuncSignature>,
    pub body: FuncBody,
}

#[derive(Clone, Debug)]
pub struct FuncSignature {
    // pub header: FuncHeader,
    pub params: ThinVec<FuncParam>,
    pub tail: FuncTail,
}

#[derive(Clone, Debug)]
pub struct FuncHeader {
    pub safety: Safety,
}

#[derive(Clone, Debug)]
pub enum Safety {
    Unsafe(Span),
    Default,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub attrs: ThinVec<Attribute>,
    pub ty: P<Type>,
    pub pattern: P<Pattern>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FuncTail {
    pub throw: FuncThrow,
    pub ret: FuncReturn,
}

#[derive(Clone, Debug)]
pub enum FuncThrow {
    None,
    Inferred,
    Type(ThinVec<Type>),
}

#[derive(Clone, Debug)]
pub enum FuncReturn {
    Inferred,
    Type(P<Type>),
}

#[derive(Clone, Debug)]
pub enum FuncBody {
    Semicolon,
    Block(P<Block>),
    Arrow(P<Expr>),
}

#[derive(Clone, Debug)]
pub enum ThisParamKind {
    Value(Mutability),
    Reference(Mutability),
    Explicit(Mutability, P<Type>),
}

#[derive(Clone, Debug)]
pub struct ThisParam {
    pub kind: ThisParamKind,
    pub span: Span,
}

impl FuncParam {
    pub fn from_this_param(
        attrs: ThinVec<Attribute>,
        this_param: ThisParam,
        this_key: Identifier,
    ) -> FuncParam {
        let span = this_param.span.to(this_key.span);

        let infer_type = P(Type {
            kind: TypeKind::ImplicitThis,
            span: this_key.span,
        });

        let (mutability, ty) = match this_param.kind {
            ThisParamKind::Explicit(mutability, ty) => (mutability, ty),
            ThisParamKind::Value(mutability) => (mutability, infer_type),
            ThisParamKind::Reference(mutability) => (
                Mutability::Immut,
                P(Type {
                    kind: TypeKind::Reference(infer_type, mutability),
                    span,
                }),
            ),
        };

        let pattern = P(Pattern {
            kind: PatternKind::Bind(this_key, mutability),
            assert: None,
            span
        });

        FuncParam {
            attrs,
            ty,
            pattern,
            span,
        }
    }
}
