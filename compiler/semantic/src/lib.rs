use std::{cell::RefCell, rc::Rc};

use context::AnalysisContext;
use kaede_ir_type as ir_type;

use kaede_symbol::Symbol;
use symbol_table::{GenericArgumentTable, SymbolTable, SymbolTableValue};

mod context;
mod error;
mod expr;
mod mangle;
mod stmt;
mod symbol_table;
mod top;
mod ty;

use kaede_ast as ast;
use kaede_ir as ir;
use top::TopLevelAnalysisResult;

struct SemanticAnalyzer {
    symbol_tables: Vec<SymbolTable>,
    context: AnalysisContext,
    generic_argument_table: GenericArgumentTable,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_tables: vec![SymbolTable::new()],
            context: AnalysisContext::new(),
            generic_argument_table: GenericArgumentTable::new(),
        }
    }

    pub fn lookup_symbol(&self, symbol: Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(value) = symbol_table.lookup(&symbol) {
                return Some(value);
            }
        }

        None
    }

    pub fn lookup_generic_argument(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.generic_argument_table.lookup(symbol)
    }

    pub fn get_root_symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_tables.first_mut().unwrap()
    }

    pub fn get_current_symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_tables.last_mut().unwrap()
    }

    pub fn analyze(&mut self, compile_unit: ast::CompileUnit) -> anyhow::Result<ir::CompileUnit> {
        let mut top_level_irs = vec![];

        for top_level in compile_unit.top_levels {
            if let TopLevelAnalysisResult::TopLevel(top_level) =
                self.analyze_top_level(top_level)?
            {
                top_level_irs.push(top_level);
            }
        }

        Ok(ir::CompileUnit {
            top_levels: top_level_irs,
        })
    }
}
