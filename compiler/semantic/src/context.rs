use std::{cell::RefCell, rc::Rc};

use kaede_ir::module_path::ModulePath;
use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{
    symbol_table::{SymbolTable, SymbolTableValue},
    SemanticAnalyzer,
};

#[derive(Debug, Clone)]
pub struct AnalysisContext {
    module_path: ModulePath,
    is_inside_loop: bool,
}

impl AnalysisContext {
    pub fn new() -> Self {
        Self {
            module_path: ModulePath::new(vec![]),
            is_inside_loop: false,
        }
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
}

impl ModuleContext {
    pub fn new() -> Self {
        Self {
            symbol_table_stack: vec![SymbolTable::new()], // Root scope
        }
    }

    #[cfg(debug_assertions)]
    pub fn dump(&self) {
        for table in self.symbol_table_stack.iter() {
            println!("Symbol table:");
            table.dump();
            println!();
        }
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
        None
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        self.get_current_symbol_table().insert(symbol, value, span)
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        self.symbol_table_stack
            .first_mut()
            .unwrap()
            .insert(symbol, value, span)
    }
}
