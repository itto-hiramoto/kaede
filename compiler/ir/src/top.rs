use std::rc::Rc;

use kaede_ir_type::{QualifiedSymbol, Ty};
use kaede_symbol::Symbol;

use crate::stmt::Block;

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

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Symbol,
    pub ty: Rc<Ty>,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: QualifiedSymbol,
    pub params: Vec<Param>,
    pub is_var_args: bool,
    pub return_ty: Option<Rc<Ty>>,
}

#[derive(Debug)]
pub struct Fn {
    pub decl: FnDecl,
    pub body: Block,
}

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

#[derive(Debug)]
pub struct Extern {
    pub lang_linkage: Option<Symbol>,
    pub fn_decl: FnDecl,
}

#[derive(Debug)]
pub enum TopLevel {
    Fn(Rc<Fn>),
    Struct(Rc<Struct>),
    Enum(Rc<Enum>),
    Extern(Extern),
}
