use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    types::StructType,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::top::{Enum, Fn, GenericParams, Impl, Struct, StructField, Visibility};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};
use kaede_ast_type::{GenericArgs, Ty, TyKind};

use crate::error::CodegenError;

#[derive(Debug)]
pub struct FunctionInfo<'ctx> {
    pub value: FunctionValue<'ctx>,
    pub return_type: ReturnType,
    pub param_types: Vec<Rc<Ty>>,
}

#[derive(Debug)]
pub struct StructInfo<'ctx> {
    pub mangled_name: Symbol,
    pub ty: StructType<'ctx>,
    pub fields: HashMap<Symbol, StructField>,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub name: Ident,
    pub _vis: Visibility,
    pub offset: u32,
    pub ty: Option<Rc<Ty>>,
}

#[derive(Debug, Clone)]
pub struct EnumInfo<'ctx> {
    pub ty: StructType<'ctx>,
    pub mangled_name: Symbol,
    pub name: Symbol, // Non-mangled
    pub variants: HashMap<Symbol, EnumVariantInfo>,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug, Clone)]
pub struct AlreadyGeneratedGenericImpl {
    pub table: Vec<GenericArgs>,
}

impl AlreadyGeneratedGenericImpl {
    pub fn new() -> Self {
        Self { table: Vec::new() }
    }

    pub fn insert(&mut self, args: GenericArgs) {
        self.table.push(args);
    }

    pub fn contains(&self, args: &GenericArgs) -> bool {
        let types = &args.types;

        for args in &self.table {
            if types == &args.types {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Clone)]
pub struct GenericImplInfo {
    pub impl_: Impl,
    pub visibility: Visibility,
    pub span: Span,
    pub already_generated: AlreadyGeneratedGenericImpl,
}

impl GenericImplInfo {
    pub fn new(impl_: Impl, visibility: Visibility, span: Span) -> Self {
        Self {
            impl_,
            visibility,
            span,
            already_generated: AlreadyGeneratedGenericImpl::new(),
        }
    }
}

#[derive(Debug)]
pub struct GenericStructInfo {
    pub ast: Struct,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericStructInfo {
    pub fn new(ast: Struct) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub struct GenericEnumInfo {
    pub ast: Enum,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericEnumInfo {
    pub fn new(ast: Enum) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub enum GenericKind {
    Struct(GenericStructInfo),
    Enum(GenericEnumInfo),
    Func((Fn, Visibility)),
}

#[derive(Debug)]
pub struct GenericInfo {
    pub kind: GenericKind,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug)]
pub enum SymbolTableValue<'ctx> {
    Variable((PointerValue<'ctx>, Rc<Ty> /* Variable type */)),
    Function(FunctionInfo<'ctx>),
    Struct(StructInfo<'ctx>),
    Enum(EnumInfo<'ctx>),
    Generic(GenericInfo),
    Module(Symbol),
}

pub type SymbolTable<'ctx> = HashMap<Symbol, Rc<RefCell<SymbolTableValue<'ctx>>>>;

#[derive(Debug, Clone)]
pub enum ReturnType {
    Type(Rc<Ty>),
    Void,
}

impl From<Option<Rc<Ty>>> for ReturnType {
    fn from(value: Option<Rc<Ty>>) -> Self {
        match value {
            Some(ty) => ReturnType::Type(ty),
            None => ReturnType::Void,
        }
    }
}

pub struct GenericArgTable {
    map: HashMap<Symbol, Rc<Ty>>,
}

impl GenericArgTable {
    pub fn lookup(&self, symbol: Symbol) -> Option<Rc<Ty>> {
        self.map.get(&symbol).cloned()
    }
}

impl From<(GenericParams, GenericArgs)> for GenericArgTable {
    fn from((params, args): (GenericParams, GenericArgs)) -> Self {
        let mut map = HashMap::new();

        for (param, arg) in params.names.iter().zip(args.types.iter()) {
            map.insert(param.symbol(), arg.clone());
        }

        Self { map }
    }
}

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    // Pushed when create a new scope.
    symbol_tables: Vec<SymbolTable<'ctx>>,

    generic_arg_table_stack: Vec<GenericArgTable>,
}

impl<'ctx> TypeCtx<'ctx> {
    #[allow(dead_code)]
    pub fn dump_generic_arg_table(&self) {
        eprintln!("----------");
        for table in &self.generic_arg_table_stack {
            eprintln!("-----");
            for (name, ty) in &table.map {
                eprintln!("{} -> {:?}", name.as_str(), ty);
            }
            eprintln!("-----");
        }
        eprintln!("----------");
    }

    pub fn lookup_symbol(
        &self,
        symbol: Symbol,
        span: Span,
    ) -> anyhow::Result<Rc<RefCell<SymbolTableValue<'ctx>>>> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.get(&symbol) {
                return Ok(value.clone());
            }
        }

        Err(CodegenError::Undeclared { name: symbol, span }.into())
    }

    pub fn bind_symbol_to_new_name(
        &mut self,
        new_name: Symbol,
        bindee: Rc<RefCell<SymbolTableValue<'ctx>>>,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .symbol_tables
            .first_mut()
            .unwrap()
            .insert(new_name, bindee)
            .is_some()
        {
            return Err(CodegenError::AlreadyDeclared {
                name: new_name,
                span,
            }
            .into());
        }

        Ok(())
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue<'ctx>,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .symbol_tables
            .first_mut()
            .unwrap()
            .insert(symbol, Rc::new(RefCell::new(value)))
            .is_some()
        {
            return Err(CodegenError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue<'ctx>,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .symbol_tables
            .last_mut()
            .unwrap()
            .insert(symbol, Rc::new(RefCell::new(value)))
            .is_some()
        {
            return Err(CodegenError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    pub fn push_symbol_table(&mut self, table: SymbolTable<'ctx>) {
        self.symbol_tables.push(table);
    }

    pub fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop().unwrap();
    }

    pub fn push_generic_arg_table(&mut self, table: GenericArgTable) {
        self.generic_arg_table_stack.push(table);
    }

    pub fn pop_generic_arg_table(&mut self) {
        self.generic_arg_table_stack.pop().unwrap();
    }

    // Returns the actual type in preference to generic types.
    pub fn lookup_generic_arg(&self, symbol: Symbol) -> Option<Rc<Ty>> {
        for table in self.generic_arg_table_stack.iter().rev() {
            if let Some(ty) = table.lookup(symbol) {
                match ty.kind.as_ref() {
                    TyKind::Generic(gty) => {
                        if gty.name.symbol() == symbol {
                            continue;
                        } else {
                            return self.lookup_generic_arg(gty.name.symbol());
                        }
                    }
                    _ => return Some(ty),
                }
            }
        }

        None
    }
}
