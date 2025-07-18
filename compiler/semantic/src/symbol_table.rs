use std::{cell::RefCell, collections::HashMap, rc::Rc};

use kaede_ir::{
    module_path::ModulePath,
    ty::{self as ir_type},
};
use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{error::SemanticError, SemanticAnalyzer};

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

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct SymbolTableValue {
    pub kind: SymbolTableValueKind,
    pub module_path: ModulePath,
}

impl SymbolTableValue {
    pub fn new(kind: SymbolTableValueKind, analyzer: &SemanticAnalyzer) -> Self {
        Self {
            kind,
            module_path: analyzer.current_module_path().clone(),
        }
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

    pub fn clear(&mut self) {
        self.table.clear();
    }

    #[cfg(debug_assertions)]
    pub fn dump(&self) {
        for (symbol, value) in self.table.iter() {
            println!("{}: {:?}", symbol, value.borrow().kind);
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
        if self.table.insert(symbol, value).is_some() {
            return Err(SemanticError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &Rc<RefCell<SymbolTableValue>>)> {
        self.table.iter()
    }
}

pub struct GenericArgumentTable {
    map: HashMap<Symbol, Rc<ir_type::Ty>>,
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

impl SemanticAnalyzer {
    pub fn with_generic_arguments<T>(
        &mut self,
        generic_params: &ast::top::GenericParams,
        generic_args: &[Rc<ir_type::Ty>],
        f: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        // Check the length of the generic arguments
        if generic_params.names.len() != generic_args.len() {
            return Err(SemanticError::GenericArgumentLengthMismatch {
                expected: generic_params.names.len(),
                actual: generic_args.len(),
                span: generic_params.span,
            }
            .into());
        }

        let mut generic_argument_table = GenericArgumentTable::new();

        generic_argument_table.map.extend(
            generic_params
                .names
                .iter()
                .zip(generic_args.iter())
                .map(|(ident, ty)| (ident.symbol(), ty.clone())),
        );

        let current_module_path = self.current_module_path().clone();

        self.modules
            .get_mut(&current_module_path)
            .unwrap()
            .push_generic_argument_table(generic_argument_table);

        let result = f(self);

        // Remove the generic arguments
        self.modules
            .get_mut(&current_module_path)
            .unwrap()
            .pop_generic_argument_table();

        result
    }
}
