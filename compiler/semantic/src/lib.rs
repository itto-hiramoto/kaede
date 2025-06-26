use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Component, PathBuf},
    rc::Rc,
};

use context::AnalysisContext;
use kaede_common::kaede_autoload_dir;
use kaede_ir::{module_path::ModulePath, qualified_symbol::QualifiedSymbol, ty as ir_type};

use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};
use symbol_table::SymbolTableValue;

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
    generated_generics: Vec<ir::top::TopLevel>,
    imported_module_paths: HashSet<PathBuf>,
    root_dir: PathBuf,
}

impl SemanticAnalyzer {
    pub fn new(file_path: FilePath, root_dir: PathBuf) -> Self {
        if !root_dir.is_dir() {
            panic!("Root directory is not a directory");
        }

        // Set the current module name in the context.
        let mut context = AnalysisContext::new();
        let module_path =
            Self::create_module_path_from_file_path(root_dir.clone(), file_path).unwrap();
        context.set_module_path(module_path.clone());

        let mut module_context = ModuleContext::new(file_path);
        module_context.push_scope(SymbolTable::new());

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generated_generics: Vec::new(),
            imported_module_paths: HashSet::new(),
            root_dir,
        }
    }

    #[cfg(debug_assertions)]
    pub fn new_for_single_file_test() -> Self {
        let mut context = AnalysisContext::new();
        let module_path = ModulePath::new(vec![Symbol::from("test".to_string())]);
        context.set_module_path(module_path.clone());

        let mut module_context = ModuleContext::new(FilePath::from(PathBuf::from("test.kd")));
        module_context.push_scope(SymbolTable::new());

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generated_generics: Vec::new(),
            imported_module_paths: HashSet::new(),
            root_dir: PathBuf::from("."),
        }
    }

    fn create_module_path_from_file_path(
        root_dir: PathBuf,
        file_path: FilePath,
    ) -> anyhow::Result<ModulePath> {
        let mut diff_from_root = {
            // Get the canonical paths
            let kaede_lib_src_dir = kaede_autoload_dir()
                .parent()
                .unwrap()
                .to_path_buf()
                .canonicalize()?;
            let file_parent = file_path.path().parent().unwrap().canonicalize()?;

            // Try to strip the project root first, if that fails try the standard library root
            if let Ok(relative_path) = file_parent.strip_prefix(&root_dir) {
                relative_path
                    .components()
                    .map(|c| {
                        // The path is canonicalized, so that the components are all normal.
                        if let Component::Normal(os_str) = c {
                            Symbol::from(os_str.to_string_lossy().to_string())
                        } else {
                            unreachable!();
                        }
                    })
                    .collect::<Vec<_>>()
            } else if let Ok(relative_path) = file_parent.strip_prefix(&kaede_lib_src_dir) {
                // For standard library modules, prepend "std" to the module path
                let mut modules = vec![Symbol::from("std".to_string())];
                modules.extend(relative_path.components().map(|c| {
                    if let Component::Normal(os_str) = c {
                        Symbol::from(os_str.to_string_lossy().to_string())
                    } else {
                        unreachable!();
                    }
                }));
                modules
            } else {
                return Err(anyhow::anyhow!(
                    "File path '{}' is not within project root '{}' or standard library root '{}'",
                    file_parent.display(),
                    root_dir.display(),
                    kaede_lib_src_dir.display()
                ));
            }
        };

        // Add the file name to the module path
        diff_from_root.push(
            file_path
                .path()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string()
                .into(),
        );

        Ok(ModulePath::new(diff_from_root))
    }

    pub fn lookup_symbol(&self, symbol: Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.modules
            .get(&self.current_module_path())
            .expect(&format!(
                "Module not found: {:?}",
                self.current_module_path().get_module_names_from_root()
            ))
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
        self.modules
            .get(&self.current_module_path())
            .unwrap()
            .lookup_generic_argument(symbol)
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

    pub fn create_generated_generic_key(&self, name: Symbol, args: &[Rc<ir_type::Ty>]) -> Symbol {
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

    pub fn create_method_key(
        &self,
        parent_name: Symbol,
        method_name: Symbol,
        is_static: bool,
    ) -> Symbol {
        if is_static {
            format!("{}::{}", parent_name, method_name).into()
        } else {
            format!("{}.{}", parent_name, method_name).into()
        }
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

    fn inject_generated_generics_to_compile_unit(
        &mut self,
        mut compile_unit: ir::CompileUnit,
    ) -> ir::CompileUnit {
        for top_level in self.generated_generics.iter() {
            compile_unit.top_levels.push(top_level.clone());
        }

        compile_unit
    }

    fn import_autoloads(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        let autoload_dir = kaede_autoload_dir();

        if !autoload_dir.exists() {
            panic!("Autoload directory not found!");
        }

        let autoload_libs = std::fs::read_dir(autoload_dir)?
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|e| e == "kd")) // Exclude non-source files
            .collect::<Vec<_>>();

        for lib in autoload_libs {
            let segments = lib
                .iter()
                .map(|s| Ident::new(s.to_string_lossy().to_string().into(), Span::dummy()))
                .collect::<Vec<_>>();

            let import_ast = ast::top::TopLevel {
                kind: ast::top::TopLevelKind::Import(ast::top::Import {
                    module_path: ast::top::Path {
                        segments,
                        span: Span::dummy(),
                    },
                    span: Span::dummy(),
                }),
                visibility: ast::top::Visibility::Private,
                span: Span::dummy(),
            };

            let import_ir = self.analyze_top_level(import_ast)?;

            if let TopLevelAnalysisResult::Imported(imported_irs) = import_ir {
                imported_irs.iter().for_each(|top_level| {
                    top_level_irs.push(top_level.clone());
                });
            } else {
                unreachable!("{:?}", import_ir);
            }
        }

        Ok(())
    }

    pub fn analyze(&mut self, compile_unit: ast::CompileUnit) -> anyhow::Result<ir::CompileUnit> {
        let mut top_level_irs = vec![];

        self.import_autoloads(&mut top_level_irs)?;

        for top_level in compile_unit.top_levels {
            match self.analyze_top_level(top_level)? {
                TopLevelAnalysisResult::TopLevel(top_level) => {
                    top_level_irs.push(top_level);
                }

                TopLevelAnalysisResult::Imported(imported_irs) => {
                    imported_irs.iter().for_each(|top_level| {
                        top_level_irs.push(top_level.clone());
                    });
                }

                _ => {}
            }
        }

        Ok(
            self.inject_generated_generics_to_compile_unit(ir::CompileUnit {
                top_levels: top_level_irs,
            }),
        )
    }
}
