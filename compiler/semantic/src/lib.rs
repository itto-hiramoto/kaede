use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Component, PathBuf},
    rc::Rc,
};

use context::AnalysisContext;
use kaede_common::{kaede_autoload_dir, kaede_lib_src_dir};
use kaede_ir::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    ty::{self as ir_type},
};

use kaede_parse::Parser;
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
use kaede_ast::{self as ast, top::Visibility};
use kaede_ir as ir;
pub use top::TopLevelAnalysisResult;

use crate::{
    context::{AnalyzeCommand, ModuleContext},
    symbol_table::SymbolTable,
};

pub struct SemanticAnalyzer {
    modules: HashMap<ModulePath, ModuleContext>,
    context: AnalysisContext,
    generated_generics: Vec<ir::top::TopLevel>,
    generating_generics: HashSet<Symbol>,
    imported_module_paths: HashSet<PathBuf>,
    root_dir: PathBuf,
    autoloads_imported: bool,
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

        let module_context = ModuleContext::new(file_path);

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generated_generics: Vec::new(),
            generating_generics: HashSet::new(),
            imported_module_paths: HashSet::new(),
            root_dir,
            autoloads_imported: false,
        }
    }

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
            generating_generics: HashSet::new(),
            imported_module_paths: HashSet::new(),
            root_dir: PathBuf::from("."),
            autoloads_imported: false,
        }
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn dump_symbol_tables(&self) {
        for (module_path, module_context) in self.modules.iter() {
            eprintln!("Module: {module_path:?}");
            eprintln!("--- Symbol tables ---");
            for table in module_context.get_symbol_tables().iter() {
                table.dump();
            }
            eprintln!("---------------------");
            eprintln!("--- Private symbol table ---");
            module_context.get_private_symbol_table().dump();
            eprintln!("---------------------");
        }
    }

    fn create_module_path_from_file_path(
        root_dir: PathBuf,
        file_path: FilePath,
    ) -> anyhow::Result<ModulePath> {
        let mut diff_from_root = {
            // Get the canonical paths
            let kaede_lib_src_dir = kaede_lib_src_dir().canonicalize()?;
            let file_parent = file_path.path().parent().unwrap().canonicalize()?;

            // Try to strip the project root first, if that fails try the standard library root
            if let Ok(relative_path) = file_parent.strip_prefix(&root_dir.canonicalize()?) {
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
                // For standard library modules
                relative_path
                    .components()
                    .map(|c| {
                        if let Component::Normal(os_str) = c {
                            Symbol::from(os_str.to_string_lossy().to_string())
                        } else {
                            unreachable!();
                        }
                    })
                    .collect::<Vec<_>>()
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
            .get(self.current_module_path())
            .unwrap_or_else(|| {
                panic!(
                    "Module not found: {:?}",
                    self.current_module_path().get_module_names_from_root()
                )
            })
            .lookup_symbol(&symbol)
    }

    pub fn lookup_qualified_symbol(
        &self,
        symbol: QualifiedSymbol,
    ) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.modules
            .get(symbol.module_path())
            .unwrap_or_else(|| {
                panic!(
                    "Module not found: {:?}",
                    symbol.module_path().get_module_names_from_root()
                )
            })
            .lookup_symbol(&symbol.symbol())
    }

    pub fn lookup_generic_argument(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.modules
            .get(self.current_module_path())
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
        vis: Visibility,
        span: Span,
    ) -> anyhow::Result<()> {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .insert_symbol_to_root_scope(symbol, value, vis, span)
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
            format!("{parent_name}::{method_name}").into()
        } else {
            format!("{parent_name}.{method_name}").into()
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

    fn analyze_prelude(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        let prelude_file_path = FilePath::from(kaede_lib_src_dir().join("prelude.kd"));
        let prelude_source = std::fs::read_to_string(prelude_file_path.path())?;

        let prelude_ast = Parser::new(prelude_source.as_str(), prelude_file_path).run()?;

        // Extend from the front
        for top_level in prelude_ast.top_levels.into_iter() {
            match self.analyze_top_level(top_level)? {
                TopLevelAnalysisResult::TopLevel(top_level) => {
                    top_level_irs.push(top_level);
                }

                TopLevelAnalysisResult::Imported(imported_irs) => {
                    imported_irs.iter().for_each(|top_level| {
                        top_level_irs.push(top_level.clone());
                    });
                }

                _ => unreachable!(),
            }
        }

        Ok(())
    }

    pub fn insert_prelude(&mut self, compile_unit: &mut ast::CompileUnit) -> anyhow::Result<()> {
        let prelude_file_path = FilePath::from(kaede_lib_src_dir().join("prelude.kd"));
        let prelude_source = std::fs::read_to_string(prelude_file_path.path())?;

        let prelude_ast = Parser::new(prelude_source.as_str(), prelude_file_path).run()?;

        // Extend from the front
        for top_level in prelude_ast.top_levels.into_iter().rev() {
            compile_unit.top_levels.push_front(top_level);
        }

        Ok(())
    }

    fn import_autoloads(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        // Only import autoloads once to prevent duplicate definitions
        if self.autoloads_imported {
            return Ok(());
        }
        self.autoloads_imported = true;

        let autoload_dir = kaede_autoload_dir();

        if !autoload_dir.exists() {
            panic!("Autoload directory not found!");
        }

        let autoload_libs = std::fs::read_dir(autoload_dir)?
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|e| e == "kd")) // Exclude non-source files
            .collect::<Vec<_>>();

        self.with_no_prelude(|analyzer| {
            for lib in autoload_libs {
                let segments = lib
                    .iter()
                    .map(|s| {
                        ast::top::PathSegment::Segment(Ident::new(
                            s.to_string_lossy().to_string().into(),
                            Span::dummy(),
                        ))
                    })
                    .collect::<Vec<_>>();

                let import_ast = ast::top::TopLevel {
                    kind: ast::top::TopLevelKind::Import(ast::top::Import {
                        module_path: ast::top::Path {
                            segments,
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    }),
                    span: Span::dummy(),
                };

                let import_ir = analyzer.analyze_top_level(import_ast)?;

                if let TopLevelAnalysisResult::Imported(imported_irs) = import_ir {
                    imported_irs.iter().for_each(|top_level| {
                        top_level_irs.push(top_level.clone());
                    });
                } else {
                    unreachable!("{:?}", import_ir);
                }
            }

            Ok(())
        })
    }

    pub fn analyze(
        &mut self,
        compile_unit: ast::CompileUnit,
        no_autoload: bool,
        no_prelude: bool,
    ) -> anyhow::Result<ir::CompileUnit> {
        let mut top_level_irs = vec![];

        // Create root module
        let mut root_module = ModuleContext::new(FilePath::dummy());
        root_module.push_scope(SymbolTable::new());
        self.modules.insert(ModulePath::new(vec![]), root_module);

        self.context.set_no_prelude(no_prelude);

        if !no_autoload {
            self.import_autoloads(&mut top_level_irs)?;
        }

        if !no_prelude {
            self.analyze_prelude(&mut top_level_irs)?;
        }

        let (types, others): (Vec<_>, Vec<_>) =
            compile_unit.top_levels.into_iter().partition(|top| {
                matches!(
                    top.kind,
                    ast::top::TopLevelKind::Struct(_) | ast::top::TopLevelKind::Enum(_)
                )
            });

        let (imports, others): (Vec<_>, Vec<_>) = others
            .into_iter()
            .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Import(_)));

        let (uses, others): (Vec<_>, Vec<_>) = others
            .into_iter()
            .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Use(_)));

        // Analyze all imports
        for top_level in imports {
            if let TopLevelAnalysisResult::Imported(imported_irs) =
                self.analyze_top_level(top_level)?
            {
                imported_irs.iter().for_each(|top_level| {
                    top_level_irs.push(top_level.clone());
                });
            } else {
                unreachable!()
            }
        }

        // Analyze all use directives
        for top_level in uses {
            if let TopLevelAnalysisResult::Imported(imported_irs) =
                self.analyze_top_level(top_level)?
            {
                if !imported_irs.is_empty() {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }

        // Declare all types
        // (This is necessary to avoid errors when declaring functions and methods)
        for top_level in types {
            match self.analyze_top_level(top_level)? {
                TopLevelAnalysisResult::TopLevel(top_level) => {
                    top_level_irs.push(top_level);
                }
                TopLevelAnalysisResult::GenericTopLevel => {}
                _ => unreachable!(),
            }
        }

        // Declare all functions and methods
        // (This process removes the need to worry about function declaration order)
        self.with_analyze_command(AnalyzeCommand::OnlyFnDeclare, |analyzer| {
            for top_level in others.iter() {
                match &top_level.kind {
                    ast::top::TopLevelKind::Fn(function) => {
                        analyzer.analyze_fn(function.clone())?;
                    }

                    ast::top::TopLevelKind::Impl(impl_block) => {
                        analyzer.analyze_impl(impl_block.clone())?;
                    }

                    _ => {}
                }
            }
            Ok::<(), anyhow::Error>(())
        })?;

        // Analyze all top levels
        self.with_analyze_command(AnalyzeCommand::WithoutFnDeclare, |analyzer| {
            for top_level in others {
                match analyzer.analyze_top_level(top_level)? {
                    TopLevelAnalysisResult::TopLevel(top_level) => {
                        top_level_irs.push(top_level);
                    }

                    TopLevelAnalysisResult::GenericTopLevel => {}

                    _ => unreachable!(),
                }
            }
            Ok::<(), anyhow::Error>(())
        })?;

        Ok(
            self.inject_generated_generics_to_compile_unit(ir::CompileUnit {
                top_levels: top_level_irs,
            }),
        )
    }
}
