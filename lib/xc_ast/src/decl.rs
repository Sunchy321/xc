use thin_vec::ThinVec;
use xc_span::{Identifier, Span};

use crate::ast::DelimitedArgs;
use crate::attr::Attribute;
use crate::expr::Expr;
use crate::func::Func;
use crate::generic::Generic;
use crate::import::Import;
use crate::module::Visibility;
use crate::path::Path;
use crate::ptr::P;
use crate::r#trait::{ConstConstraintItem, MemberItem, PropConstraintItem, Trait, TypeConstraintItem};
use crate::ty::{StructType, Type};

#[derive(Clone, Debug)]
pub enum DeclKind {
    QualDecl(ThinVec<Qual>),
    Import(Import),
    Func(P<Func>),
    Trait(P<Trait>),
    Type(P<TypeDecl>),
    Enum(P<EnumDecl>),
    Impl(P<ImplDecl>),
    Module(P<ModuleDecl>),
    Macro(P<MacroDecl>),
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub attrs: ThinVec<Attribute>,
    pub kind: DeclKind,
    pub span: Span,
}

/// A `type` or `class` declaration.
#[derive(Clone, Debug)]
pub struct TypeDecl {
    /// `true` for `class` declarations.
    pub is_class: bool,
    pub ident: Identifier,
    pub generics: Generic,
    /// Traits listed after `:`, as in `class C : A, B`.
    pub traits: ThinVec<P<Type>>,
    pub body: TypeDeclBody,
}

#[derive(Clone, Debug)]
pub enum TypeDeclBody {
    /// `type X = T;` and `class C B;` (equivalently `type C = class B`).
    Alias(P<Type>),
    /// `{ field: T, ... func ... }` — fields plus further members.
    Struct {
        fields: StructType,
        members: ThinVec<MemberItem>,
    },
}

/// An `enum` declaration.
#[derive(Clone, Debug)]
pub struct EnumDecl {
    pub ident: Identifier,
    pub generics: Generic,
    /// `enum E : T` — the base type (or a trait such as `ErrorCode`).
    pub base: Option<P<Type>>,
    pub variants: ThinVec<EnumVariant>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub attrs: ThinVec<Attribute>,
    pub ident: Identifier,
    /// The payload, as a type: `(T)`, `[T]` or `(field: T, ...)`.
    pub payload: Option<P<Type>>,
    /// A traditional enum value: `A = 1`.
    pub value: Option<P<Expr>>,
}

/// An `impl` block.
#[derive(Clone, Debug)]
pub struct ImplDecl {
    pub generics: Generic,
    pub self_ty: P<Type>,
    /// `impl T : Trait` — the trait being implemented.
    pub trait_ty: Option<P<Type>>,
    /// An `if` condition making the impl conditional.
    pub condition: Option<P<Expr>>,
    pub body: ImplBody,
}

#[derive(Clone, Debug)]
pub enum ImplBody {
    /// `impl X : T;`
    Semicolon,
    /// `impl X : T = never;`
    Never,
    Block(ThinVec<ImplItem>),
}

#[derive(Clone, Debug)]
pub enum ImplItem {
    Func(P<Func>),
    Type(TypeConstraintItem),
    Const(ConstConstraintItem),
    Prop(PropConstraintItem),
}

/// A `module` declaration: `module a.b.c;`, `module a.b { ... }` or the
/// alias form `module name = this.path;`.
#[derive(Clone, Debug)]
pub struct ModuleDecl {
    pub path: Option<Path>,
    pub alias: Option<Path>,
    pub body: Option<ThinVec<Decl>>,
    pub span: Span,
}

/// A `macro` definition. The body is kept as raw delimited token trees.
#[derive(Clone, Debug)]
pub struct MacroDecl {
    pub ident: Identifier,
    pub body: DelimitedArgs,
}

#[derive(Clone, Debug)]
pub enum QualKind {
    Vis(Visibility),

    Async,
    Const,
    Extern,
    Linear,
    Partial,
    Static,
    Unsafe,
}

#[derive(Clone, Debug)]
pub struct Qual {
    pub kind: QualKind,
    pub span: Span,
}
