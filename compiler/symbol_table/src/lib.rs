mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use kaede_ir::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    ty::{self as ir_type},
};
use kaede_span::Span;
use kaede_symbol::Symbol;

use kaede_ast as ast;
use kaede_ir as ir;

#[derive(Debug)]
pub struct GenericImplInfo {
    pub impl_: ast::top::Impl,
    pub span: Span,
    // Contains already generated generic arguments
    pub generateds: Vec<Vec<Rc<ir_type::Ty>>>,
}

impl GenericImplInfo {
    pub fn new(impl_: ast::top::Impl, span: Span) -> Self {
        Self {
            impl_,
            span,
            generateds: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct GenericStructInfo {
    pub ast: ast::top::Struct,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericStructInfo {
    pub fn new(ast: ast::top::Struct) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub struct GenericEnumInfo {
    pub ast: ast::top::Enum,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericEnumInfo {
    pub fn new(ast: ast::top::Enum) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub struct GenericFuncInfo {
    pub ast: ast::top::Fn,
}

#[derive(Debug)]
pub enum GenericKind {
    Struct(GenericStructInfo),
    Enum(GenericEnumInfo),
    Func(GenericFuncInfo),
}

#[derive(Debug)]
pub struct GenericInfo {
    pub kind: GenericKind,
    pub module_path: ModulePath,
}

impl GenericInfo {
    pub fn new(kind: GenericKind, module_path: ModulePath) -> Self {
        Self { kind, module_path }
    }

    pub fn get_generic_argument_length(&self) -> usize {
        match &self.kind {
            GenericKind::Struct(info) => info.ast.generic_params.as_ref().unwrap().names.len(),
            GenericKind::Enum(info) => info.ast.generic_params.as_ref().unwrap().names.len(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub ty: Rc<ir_type::Ty>,
}

#[derive(Debug)]
pub enum SymbolTableValueKind {
    Function(Rc<ir::top::FnDecl>),
    Variable(VariableInfo),
    Struct(Rc<ir::top::Struct>),
    Enum(Rc<ir::top::Enum>),
    Generic(Box<GenericInfo>),
    Placeholder(QualifiedSymbol),
}

#[derive(Debug)]
pub struct SymbolTableValue {
    pub kind: SymbolTableValueKind,
    pub module_path: ModulePath,
}

impl SymbolTableValue {
    pub fn new(kind: SymbolTableValueKind, module_path: ModulePath) -> Self {
        Self { kind, module_path }
    }
}

pub struct SymbolTable {
    table: HashMap<Symbol, Rc<RefCell<SymbolTableValue>>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    /// Clone the symbol table for type inference purposes
    pub fn clone_for_inference(&self) -> Self {
        Self {
            table: self.table.clone(),
        }
    }

    pub fn clear(&mut self) {
        self.table.clear();
    }

    #[cfg(debug_assertions)]
    pub fn dump(&self) {
        for (symbol, value) in self.table.iter() {
            println!("{}: {:?}", symbol, value.borrow().kind);
        }
    }

    #[cfg(debug_assertions)]
    pub fn dump_variables(&self) {
        for (symbol, value) in self.table.iter() {
            if let SymbolTableValueKind::Variable(_) = value.borrow().kind {
                println!("{}: {:?}", symbol, value.borrow().kind);
            }
        }
    }

    pub fn lookup(&self, symbol: &Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.table.get(symbol).cloned()
    }

    pub fn insert(
        &mut self,
        symbol: Symbol,
        value: Rc<RefCell<SymbolTableValue>>,
        span: Span,
    ) -> anyhow::Result<()> {
        if let Some(existing) = self.table.insert(symbol, value) {
            if let SymbolTableValueKind::Placeholder(_) = existing.borrow().kind {
                return Ok(());
            }

            return Err(error::SymbolTableError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    // Accepts already declared symbols
    pub fn bind(&mut self, symbol: Symbol, value: Rc<RefCell<SymbolTableValue>>) {
        self.table.insert(symbol, value);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &Rc<RefCell<SymbolTableValue>>)> {
        self.table.iter()
    }
}

/// Scoped view of multiple symbol tables that respects shadowing.
/// Lookup walks the tables from the most recent (last) to the oldest (first).
pub struct ScopedSymbolTableView {
    tables: Vec<SymbolTable>,
}

impl ScopedSymbolTableView {
    /// Clone symbol tables for inference while preserving scope order.
    pub fn merge_for_inference(tables: &[SymbolTable]) -> Self {
        Self {
            tables: tables
                .iter()
                .map(SymbolTable::clone_for_inference)
                .collect(),
        }
    }

    pub fn lookup(&self, symbol: &Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.tables
            .iter()
            .rev()
            .find_map(|table| table.lookup(symbol))
    }
}

pub struct GenericArgumentTable {
    pub map: HashMap<Symbol, Rc<ir_type::Ty>>,
}

impl Default for GenericArgumentTable {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericArgumentTable {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.map.get(&symbol).cloned()
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    pub fn dump(&self) {
        for (symbol, ty) in self.map.iter() {
            println!("{symbol}: {ty:?}");
        }
    }
}
