use std::rc::Rc;

use kaede_common::LangLinkage;
use kaede_symbol::Symbol;

use crate::{
    expr,
    qualified_symbol::QualifiedSymbol,
    stmt::Block,
    ty::{GenericInstanceInfo, Mutability, Ty},
};

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Symbol,
    pub ty: Rc<Ty>,
    pub offset: u64,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: QualifiedSymbol,
    pub fields: Vec<StructField>,
    pub generic_instance: Option<GenericInstanceInfo>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Symbol,
    pub ty: Rc<Ty>,
    pub default: Option<Rc<expr::Expr>>,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.ty == other.ty
            && match (&self.default, &other.default) {
                (None, None) => true,
                (Some(lhs), Some(rhs)) => Rc::ptr_eq(lhs, rhs),
                _ => false,
            }
    }
}

impl Eq for Param {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub lang_linkage: LangLinkage,
    // For generic functions
    pub link_once: bool,
    pub name: QualifiedSymbol,
    pub params: Vec<Param>,
    pub is_c_variadic: bool,
    pub return_ty: Rc<Ty>,
    pub generic_instance: Option<GenericInstanceInfo>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Symbol,
    pub ty: Option<Rc<Ty>>,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: QualifiedSymbol,
    pub variants: Vec<EnumVariant>,
    pub generic_instance: Option<GenericInstanceInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Impl {
    pub methods: Vec<Rc<Fn>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceMethod {
    pub name: Symbol,
    pub self_: Option<Mutability>,
    pub params: Vec<Param>,
    pub return_ty: Rc<Ty>,
    /// True if any param or return type references the interface itself.
    /// Such methods are not safe to dispatch through a fat pointer.
    pub is_self_shaped: bool,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub name: QualifiedSymbol,
    pub methods: Vec<InterfaceMethod>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Fn(Rc<Fn>),
    Struct(Rc<Struct>),
    Enum(Rc<Enum>),
    Impl(Rc<Impl>),
    Interface(Rc<Interface>),
}
