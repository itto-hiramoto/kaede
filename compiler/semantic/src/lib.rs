use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use context::AnalysisContext;
use kaede_ir::{module_path::ModulePath, qualified_symbol::QualifiedSymbol, ty as ir_type};

use kaede_span::{file::FilePath, Span};
use kaede_symbol::Symbol;
use symbol_table::{GenericArgumentTable, SymbolTableValue};

mod context;
mod error;
mod expr;
mod stmt;
mod symbol_table;
mod top;
mod ty;

pub use error::SemanticError;
use kaede_ast as ast;
use kaede_ir as ir;
pub use top::TopLevelAnalysisResult;

use crate::{context::ModuleContext, symbol_table::SymbolTable};

pub struct SemanticAnalyzer {
    modules: HashMap<ModulePath, ModuleContext>,
    context: AnalysisContext,
    generic_argument_table: GenericArgumentTable,
    generated_generic_impl_table: Vec<Rc<ir::top::Impl>>,
}

impl SemanticAnalyzer {
    pub fn new(file_path: FilePath) -> Self {
        let modules_from_root = file_path
            .path()
            .iter()
            .map(|s| {
                PathBuf::from(s)
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .to_string()
                    .into()
            })
            .collect::<Vec<_>>();

        // Set the current module name in the context.
        let mut context = AnalysisContext::new();
        let module_path = ModulePath::new(modules_from_root);
        context.set_module_path(module_path.clone());

        let mut module_context = ModuleContext::new();
        module_context.push_scope(SymbolTable::new());

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generic_argument_table: GenericArgumentTable::new(),
            generated_generic_impl_table: Vec::new(),
        }
    }

    pub fn lookup_symbol(&self, symbol: Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.modules
            .get(&self.current_module_path())
            .unwrap()
            .lookup_symbol(&symbol)
    }

    pub fn lookup_qualified_symbol(
        &self,
        symbol: QualifiedSymbol,
    ) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.modules
            .get(symbol.module_path())
            .unwrap()
            .lookup_symbol(&symbol.symbol())
    }

    pub fn lookup_generic_argument(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.generic_argument_table.lookup(symbol)
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .insert_symbol_to_current_scope(symbol, value, span)
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .insert_symbol_to_root_scope(symbol, value, span)
    }

    pub fn create_generated_generic_key(&self, name: Symbol, args: Vec<Rc<ir_type::Ty>>) -> Symbol {
        format!(
            "{}_{}",
            name,
            args.iter()
                .map(|ty| ty.kind.to_string())
                .collect::<Vec<_>>()
                .join("_")
        )
        .into()
    }

    pub fn create_method_key(&self, parent_name: Symbol, method_name: Symbol) -> Symbol {
        format!("{}.{}", parent_name, method_name).into()
    }

    pub fn push_scope(&mut self, symbol_table: SymbolTable) {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .push_scope(symbol_table);
    }

    pub fn pop_scope(&mut self) {
        let module_path = self.current_module_path().clone();
        self.modules.get_mut(&module_path).unwrap().pop_scope();
    }

    fn inject_generic_impl_to_compile_unit(
        &mut self,
        mut compile_unit: ir::CompileUnit,
    ) -> ir::CompileUnit {
        for impl_ in self.generated_generic_impl_table.iter() {
            compile_unit
                .top_levels
                .push(ir::top::TopLevel::Impl(impl_.clone()));
        }

        compile_unit
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

        Ok(self.inject_generic_impl_to_compile_unit(ir::CompileUnit {
            top_levels: top_level_irs,
        }))
    }
}
