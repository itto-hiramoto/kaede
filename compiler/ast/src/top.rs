use std::{collections::VecDeque, rc::Rc};

use kaede_ast_type::{Mutability, Ty};
use kaede_common::LangLinkage;
use kaede_span::Span;
use kaede_symbol::Ident;

use crate::{expr::StringLiteral, stmt::Block};

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PathSegment {
    Segment(Ident),
    Star,
}

impl PathSegment {
    pub fn is_std(&self) -> bool {
        matches!(self, PathSegment::Segment(ident) if ident.symbol().as_str() == "std")
    }
}

/// Accessibility
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(self) -> bool {
        self == Self::Public
    }

    pub fn is_private(self) -> bool {
        self == Self::Private
    }
}

impl From<bool> for Visibility {
    fn from(value: bool) -> Self {
        if value {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericParams {
    pub names: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    pub ty: Rc<Ty>,
    pub vis: Visibility,
    pub offset: u64,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub vis: Visibility,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: Rc<Ty>,
}

#[derive(Debug, Clone, Default)]
pub enum VariadicKind {
    // (a, b, ...)
    C,
    // (a, b, ...args)
    Default(Ident),
    #[default]
    // (a, b)
    None,
}

/// Deque because sometimes it is necessary to insert self (C++ style: this) at the front
#[derive(Debug, Clone, Default)]
pub struct Params {
    pub v: VecDeque<Param>,
    pub span: Span,
    pub variadic: VariadicKind,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub lang_linkage: LangLinkage,
    pub vis: Visibility,
    pub self_: Option<Mutability>,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub params: Params,
    pub return_ty: Option<Rc<Ty>>,
    pub span: Span,
    // For generic functions
    pub link_once: bool,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub decl: FnDecl,
    pub body: Rc<Block>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Import {
    pub module_path: Path,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub ty: Ty,
    pub generic_params: Option<GenericParams>,
    pub items: Rc<Vec<TopLevel>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Ident,
    pub ty: Option<Ty>,
    pub vis: Visibility,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub vis: Visibility,
    pub name: Ident,
    pub generic_params: Option<GenericParams>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Extern {
    pub vis: Visibility,
    pub lang_linkage: Option<StringLiteral>,
    pub fn_decl: FnDecl,
    pub span: Span,
}

#[derive(Debug)]
pub struct Use {
    pub vis: Visibility,
    pub path: Path,
    pub span: Span,
}

#[derive(Debug)]
pub struct Bridge {
    pub vis: Visibility,
    pub lang: StringLiteral,
    pub fn_decl: FnDecl,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub vis: Visibility,
    pub name: Ident,
    pub aliased_type: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TopLevelKind {
    Fn(Fn),
    Struct(Struct),
    Import(Import),
    Impl(Impl),
    Enum(Enum),
    Extern(Extern),
    Use(Use),
    Bridge(Bridge),
    TypeAlias(TypeAlias),
}
