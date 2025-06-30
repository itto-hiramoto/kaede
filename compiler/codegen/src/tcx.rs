use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::values::{FunctionValue, PointerValue};
use kaede_symbol::Symbol;

#[derive(Debug)]
pub enum SymbolTableValue<'ctx> {
    Variable(PointerValue<'ctx>),
    Function(FunctionValue<'ctx>),
}

pub type SymbolTable<'ctx> = HashMap<Symbol, Rc<RefCell<SymbolTableValue<'ctx>>>>;

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    // Pushed when create a new scope.
    symbol_tables: Vec<SymbolTable<'ctx>>,
}

impl<'ctx> TypeCtx<'ctx> {
    pub fn lookup_symbol(&self, symbol: Symbol) -> Option<Rc<RefCell<SymbolTableValue<'ctx>>>> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.get(&symbol) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn insert_symbol_to_root_scope(&mut self, symbol: Symbol, value: SymbolTableValue<'ctx>) {
        if self
            .symbol_tables
            .first_mut()
            .unwrap()
            .insert(symbol, Rc::new(RefCell::new(value)))
            .is_some()
        {
            panic!("Symbol already declared: {}", symbol);
        }
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue<'ctx>,
    ) {
        if self
            .symbol_tables
            .last_mut()
            .unwrap()
            .insert(symbol, Rc::new(RefCell::new(value)))
            .is_some()
        {
            panic!("Symbol already declared: {}", symbol);
        }
    }

    pub fn push_symbol_table(&mut self, table: SymbolTable<'ctx>) {
        self.symbol_tables.push(table);
    }

    pub fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop().unwrap();
    }
}
