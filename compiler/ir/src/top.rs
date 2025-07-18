use std::rc::Rc;

use kaede_symbol::Symbol;

use crate::{qualified_symbol::QualifiedSymbol, stmt::Block, ty::Ty};

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Symbol,
    pub ty: Rc<Ty>,
    pub offset: u64,
}

#[derive(Debug)]
pub struct Struct {
    pub name: QualifiedSymbol,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Symbol,
    pub ty: Rc<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangLinkage {
    Default,
    C,
    Rust,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub lang_linkage: LangLinkage,
    // For generic functions
    pub link_once: bool,
    pub name: QualifiedSymbol,
    pub params: Vec<Param>,
    pub is_c_variadic: bool,
    pub return_ty: Option<Rc<Ty>>,
}

#[derive(Debug)]
pub struct Fn {
    pub decl: FnDecl,
    // If the function is a declaration, the body is None.
    pub body: Option<Block>,
}

impl PartialEq for Fn {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl
    }
}

impl Eq for Fn {}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Symbol,
    pub ty: Option<Rc<Ty>>,
    pub offset: u32,
}

#[derive(Debug)]
pub struct Enum {
    pub name: QualifiedSymbol,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Impl {
    pub methods: Vec<Rc<Fn>>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Fn(Rc<Fn>),
    Struct(Rc<Struct>),
    Enum(Rc<Enum>),
    Impl(Rc<Impl>),
}
