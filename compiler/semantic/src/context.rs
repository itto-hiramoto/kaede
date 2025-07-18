use std::{cell::RefCell, rc::Rc};

use kaede_ast::top::Visibility;
use kaede_ir::{self as ir, module_path::ModulePath, ty::Ty};
use kaede_span::{file::FilePath, Span};
use kaede_symbol::Symbol;

use crate::{
    symbol_table::{GenericArgumentTable, SymbolTable, SymbolTableValue},
    SemanticAnalyzer,
};

#[derive(Debug, Clone)]
pub struct AnalysisContext {
    module_path: ModulePath,
    is_inside_loop: bool,
    current_function: Vec<Option<Rc<ir::top::FnDecl>>>,
}

impl Default for AnalysisContext {
    fn default() -> Self {
        Self::new()
    }
}

impl AnalysisContext {
    pub fn new() -> Self {
        Self {
            module_path: ModulePath::new(vec![]),
            is_inside_loop: false,
            current_function: vec![None],
        }
    }

    pub fn set_current_function(&mut self, function: Rc<ir::top::FnDecl>) {
        self.current_function.push(Some(function));
    }

    pub fn pop_current_function(&mut self) {
        self.current_function.pop();
    }

    pub fn get_current_function(&self) -> Option<Rc<ir::top::FnDecl>> {
        self.current_function.last().unwrap().clone()
    }

    pub fn set_module_path(&mut self, path: ModulePath) {
        self.module_path = path;
    }
}

impl SemanticAnalyzer {
    pub fn current_module_path(&self) -> &ModulePath {
        &self.context.module_path
    }

    // Temporarily changes the current context, executes the provided closure.
    pub fn with_context<F, R>(&mut self, context: AnalysisContext, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_context = std::mem::replace(&mut self.context, context);
        let result = f(self);
        self.context = old_context;
        result
    }

    // Temporarily changes the current module path, executes the provided closure.
    pub fn with_module<F, R>(&mut self, path: ModulePath, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let mut new_context = self.context.clone();
        new_context.module_path = path;
        self.with_context(new_context, f)
    }

    pub fn with_root_module<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.with_module(ModulePath::new(vec![]), f)
    }

    // Temporarily sets the context to be inside a loop, executes the provided closure.
    pub fn with_inside_loop<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.context.is_inside_loop = true;
        let result = f(self);
        self.context.is_inside_loop = false;
        result
    }

    pub fn is_inside_loop(&self) -> bool {
        self.context.is_inside_loop
    }
}

pub struct ModuleContext {
    symbol_table_stack: Vec<SymbolTable>,
    // This is used to store private items when importing modules
    private_symbol_table: SymbolTable,
    generic_argument_table: Vec<GenericArgumentTable>,
    file_path: FilePath,
}

impl ModuleContext {
    pub fn new(file_path: FilePath) -> Self {
        Self {
            symbol_table_stack: vec![SymbolTable::new()], // Root scope
            private_symbol_table: SymbolTable::new(),
            generic_argument_table: vec![],
            file_path,
        }
    }

    #[allow(dead_code)]
    pub fn get_symbol_tables(&self) -> &[SymbolTable] {
        &self.symbol_table_stack
    }

    pub fn get_all_symbols(&self) -> Vec<Symbol> {
        self.symbol_table_stack
            .iter()
            .flat_map(|table| table.iter().map(|(symbol, _)| *symbol))
            .collect()
    }

    pub fn file_path(&self) -> FilePath {
        self.file_path
    }

    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    pub fn dump(&self) {
        for table in self.symbol_table_stack.iter() {
            println!("Symbol table:");
            table.dump();
            println!();
        }
    }

    pub fn push_generic_argument_table(&mut self, generic_argument_table: GenericArgumentTable) {
        self.generic_argument_table.push(generic_argument_table);
    }

    pub fn pop_generic_argument_table(&mut self) -> Option<GenericArgumentTable> {
        self.generic_argument_table.pop()
    }

    pub fn push_scope(&mut self, symbol_table: SymbolTable) {
        self.symbol_table_stack.push(symbol_table);
    }

    pub fn pop_scope(&mut self) -> Option<SymbolTable> {
        if self.symbol_table_stack.len() > 1 {
            self.symbol_table_stack.pop()
        } else {
            // The root scope is not popped
            None
        }
    }

    pub fn get_current_symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_table_stack.last_mut().unwrap()
    }

    pub fn lookup_symbol(&self, symbol: &Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        // Search from the top of the stack (most recent scope)
        for table in self.symbol_table_stack.iter().rev() {
            if let Some(value) = table.lookup(symbol) {
                return Some(value);
            }
        }

        // Search from the private symbol table
        if let Some(value) = self.private_symbol_table.lookup(symbol) {
            return Some(value);
        }

        None
    }

    pub fn lookup_generic_argument(&self, symbol: Symbol) -> Option<Rc<Ty>> {
        // Search from the top of the stack
        for table in self.generic_argument_table.iter().rev() {
            if let Some(ty) = table.lookup(symbol) {
                return Some(ty);
            }
        }
        None
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        self.get_current_symbol_table()
            .insert(symbol, Rc::new(RefCell::new(value)), span)
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        vis: Visibility,
        span: Span,
    ) -> anyhow::Result<()> {
        if vis.is_private() {
            self.private_symbol_table
                .insert(symbol, Rc::new(RefCell::new(value)), span)
        } else {
            self.symbol_table_stack.first_mut().unwrap().insert(
                symbol,
                Rc::new(RefCell::new(value)),
                span,
            )
        }
    }

    pub fn bind_symbol(
        &mut self,
        symbol: Symbol,
        value: Rc<RefCell<SymbolTableValue>>,
        vis: Visibility,
        span: Span,
    ) -> anyhow::Result<()> {
        if vis.is_private() {
            self.private_symbol_table.insert(symbol, value, span)
        } else {
            self.symbol_table_stack
                .first_mut()
                .unwrap()
                .insert(symbol, value, span)
        }
    }

    pub fn clear_private_symbol_table(&mut self) {
        self.private_symbol_table.clear();
    }
}
