use std::{
    cell::RefCell,
    fs::{self},
    rc::Rc,
};

use crate::{
    context::{AnalyzeCommand, ModuleContext},
    SemanticAnalyzer, SemanticError,
};
use kaede_symbol_table::{
    GenericEnumInfo, GenericFuncInfo, GenericImplInfo, GenericInfo, GenericKind, GenericStructInfo,
    SymbolTable, SymbolTableValue, SymbolTableValueKind, VariableInfo,
};

use kaede_ast::{self as ast};
use kaede_ast_type as ast_type;
use kaede_common::kaede_lib_src_dir;
use kaede_ir::{self as ir, module_path::ModulePath, qualified_symbol::QualifiedSymbol};
use kaede_parse::Parser;
use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};

/// If a top-level is generic, there is no IR that can be generated immediately, so this enum is used.
#[derive(Debug, Clone)]
pub enum TopLevelAnalysisResult {
    GenericTopLevel,
    Imported(Vec<ir::top::TopLevel>),
    TopLevel(ir::top::TopLevel),
}

impl SemanticAnalyzer {
    fn fn_decl_contains_type_var(decl: &ir::top::FnDecl) -> bool {
        decl.params.iter().any(|p| ir::ty::contains_type_var(&p.ty))
            || ir::ty::contains_type_var(&decl.return_ty)
    }

    pub fn analyze_top_level(
        &mut self,
        top_level: ast::top::TopLevel,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        use ast::top::TopLevelKind;

        match top_level.kind {
            TopLevelKind::Fn(node) => self.analyze_fn(node),
            TopLevelKind::Struct(node) => self.analyze_struct(node),
            TopLevelKind::Enum(node) => self.analyze_enum(node),
            TopLevelKind::Impl(node) => self.analyze_impl(node),
            TopLevelKind::Extern(node) => self.analyze_extern(node),
            TopLevelKind::Import(node) => self.analyze_import(node),
            TopLevelKind::Use(node) => self.analyze_use(node),
            TopLevelKind::Bridge(node) => self.analyze_bridge(node),
            TopLevelKind::TypeAlias(node) => self.analyze_type_alias(node),
        }
    }

    pub fn analyze_use(&mut self, node: ast::top::Use) -> anyhow::Result<TopLevelAnalysisResult> {
        let modules = node.path.segments[..node.path.segments.len() - 1].to_vec();
        let current_module_path = self.current_module_path().get_module_names_from_root();
        let parent_module_path = if node.path.segments[0].is_std() {
            vec![]
        } else {
            current_module_path[..current_module_path.len() - 1].to_vec()
        };

        let access_chain = parent_module_path
            .into_iter()
            .chain(modules.into_iter().map(|s| match s {
                ast::top::PathSegment::Segment(ident) => ident.symbol(),
                ast::top::PathSegment::Star => todo!(),
            }))
            .collect::<Vec<_>>();

        let (_, path_to_use) =
            self.create_module_path_from_access_chain(&access_chain, node.span)?;

        let bind = |analyzer: &mut SemanticAnalyzer, name: Symbol| -> anyhow::Result<()> {
            let symbol = analyzer
                .lookup_qualified_symbol(QualifiedSymbol::new(path_to_use.clone(), name))
                .ok_or(SemanticError::Undeclared {
                    name,
                    span: node.span,
                })?;

            let current_module_path = analyzer.current_module_path().clone();
            // TODO: Use the visibility of the use directive
            // Why using public?
            // Because generic structures, etc., are generated later,
            // but if the module's use is not available at that time, an error will occur.
            analyzer
                .modules
                .get_mut(&current_module_path)
                .unwrap()
                .bind_symbol(name, symbol, ast::top::Visibility::Public);

            Ok(())
        };

        match node.path.segments.last().unwrap() {
            ast::top::PathSegment::Segment(ident) => bind(self, ident.symbol())?,

            // Use all symbols in the module
            ast::top::PathSegment::Star => {
                let symbols = self.modules[&path_to_use].get_all_symbols();

                for symbol in symbols {
                    bind(self, symbol)?;
                }
            }
        }

        Ok(TopLevelAnalysisResult::Imported(vec![]))
    }

    pub fn create_module_path_from_access_chain(
        &self,
        access_chain: &[Symbol],
        span: Span,
    ) -> anyhow::Result<(FilePath, ModulePath)> {
        assert!(!access_chain.is_empty());

        let path_prefix = if self
            .current_module_path()
            .get_module_names_from_root()
            .is_empty()
        {
            // Root module
            return Ok((FilePath::dummy(), ModulePath::new(Vec::from(access_chain))));
        } else if access_chain.first().unwrap().as_str() == "std" {
            // Standard library
            kaede_lib_src_dir()
        } else {
            self.modules
                .get(self.current_module_path())
                .unwrap()
                .file_path()
                .path()
                .parent()
                .unwrap()
                .to_path_buf()
        };

        let mut path = path_prefix;

        // Build the file path to be imported
        for (idx, segment) in access_chain.iter().enumerate() {
            if idx == access_chain.len() - 1 {
                path = path.join(segment.as_str()).with_extension("kd");
                break;
            }

            path = path.join(segment.as_str());
        }

        // Check if the file exists
        if !path.exists() {
            return Err(SemanticError::FileNotFoundForModule {
                span,
                file_path: path.to_string_lossy().to_string(),
                mod_name: *access_chain.last().unwrap(),
            }
            .into());
        }

        let path = path.to_path_buf().into();

        let module_path = Self::create_module_path_from_file_path(self.root_dir.clone(), path)?;

        Ok((path, module_path))
    }

    pub fn analyze_import(
        &mut self,
        node: ast::top::Import,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let (path, module_path) = self.create_module_path_from_access_chain(
            node.module_path
                .segments
                .iter()
                .map(|s| match s {
                    ast::top::PathSegment::Segment(ident) => ident.symbol(),
                    ast::top::PathSegment::Star => todo!(),
                })
                .collect::<Vec<_>>()
                .as_slice(),
            node.span,
        )?;

        // Prevent duplicate imports
        if self.imported_module_paths.contains(&path.path()) {
            return Ok(TopLevelAnalysisResult::Imported(vec![]));
        } else {
            self.imported_module_paths
                .insert(path.path().canonicalize()?);
        }

        // Add the module to the module table
        let mut module_context = ModuleContext::new(path);
        module_context.push_scope(SymbolTable::new());
        self.modules.insert(module_path.clone(), module_context);

        let mut parsed_module = Parser::new(&fs::read_to_string(path.path()).unwrap(), path)
            .run()
            .unwrap();

        if !self.context.no_prelude()
            && module_path
                .get_module_names_from_root()
                .first()
                .unwrap()
                .as_str()
                != "std"
        {
            self.insert_prelude(&mut parsed_module)?;
        }

        let mut top_level_irs = vec![];

        self.with_module(module_path.clone(), |analyzer| {
            let mut results = vec![];

            let (types, others): (Vec<_>, Vec<_>) =
                parsed_module.top_levels.into_iter().partition(|top| {
                    matches!(
                        top.kind,
                        ast::top::TopLevelKind::Struct(_)
                            | ast::top::TopLevelKind::Enum(_)
                            | ast::top::TopLevelKind::TypeAlias(_)
                    )
                });

            let (imports, others): (Vec<_>, Vec<_>) = others
                .into_iter()
                .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Import(_)));

            let (uses, others): (Vec<_>, Vec<_>) = others
                .into_iter()
                .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Use(_)));

            let (funcs, others): (Vec<_>, Vec<_>) = others.into_iter().partition(|top| {
                matches!(
                    top.kind,
                    ast::top::TopLevelKind::Fn(_) | ast::top::TopLevelKind::Impl(_)
                )
            });

            // Analyze all imports
            for top_level in imports {
                results.push(analyzer.analyze_top_level(top_level)?);
            }

            // Analyze all use directives
            for top_level in uses {
                results.push(analyzer.analyze_top_level(top_level)?);
            }

            // Declare all types
            // (This is necessary to avoid errors when declaring functions and methods)
            for top_level in types {
                results.push(analyzer.analyze_top_level(top_level)?);
            }

            // Declare all functions and methods
            // (This process removes the need to worry about function declaration order)
            analyzer.with_analyze_command(AnalyzeCommand::OnlyFnDeclare, |analyzer| {
                for top_level in funcs.iter() {
                    match &top_level.kind {
                        ast::top::TopLevelKind::Fn(function) => {
                            results.push(analyzer.analyze_fn(function.clone())?);
                        }

                        ast::top::TopLevelKind::Impl(impl_block) => {
                            results.push(analyzer.analyze_impl(impl_block.clone())?);
                        }

                        _ => {}
                    }
                }
                Ok::<(), anyhow::Error>(())
            })?;

            // Analyze all top levels
            for top_level in others {
                results.push(analyzer.analyze_top_level(top_level)?);
            }

            for result in results {
                match result {
                    TopLevelAnalysisResult::TopLevel(top_level) => top_level_irs.push(top_level),
                    TopLevelAnalysisResult::Imported(imported_irs) => {
                        top_level_irs.extend(imported_irs)
                    }
                    _ => continue,
                }
            }

            // Clear the private symbol table after analyzing the module
            analyzer
                .modules
                .get_mut(&analyzer.current_module_path().clone())
                .unwrap()
                .clear_private_symbol_table();

            Ok::<(), anyhow::Error>(())
        })?;

        Ok(TopLevelAnalysisResult::Imported(top_level_irs))
    }

    pub fn analyze_bridge(
        &mut self,
        node: ast::top::Bridge,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let mut fn_decl = self.analyze_fn_decl(node.fn_decl)?;

        fn_decl.lang_linkage = match node.lang.syb.as_str() {
            "Rust" => kaede_common::LangLinkage::Rust,
            _ => unreachable!(),
        };

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Fn(
            Rc::new(ir::top::Fn {
                decl: fn_decl,
                body: None,
            }),
        )))
    }

    pub fn analyze_extern(
        &mut self,
        node: ast::top::Extern,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let mut fn_decl = self.analyze_fn_decl(node.fn_decl)?;

        if let Some(lang_linkage) = node.lang_linkage {
            fn_decl.lang_linkage = match lang_linkage.syb.as_str() {
                "C" => kaede_common::LangLinkage::C,
                _ => unreachable!(),
            };
        }

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Fn(
            Rc::new(ir::top::Fn {
                decl: fn_decl,
                body: None,
            }),
        )))
    }

    pub fn analyze_impl(&mut self, node: ast::top::Impl) -> anyhow::Result<TopLevelAnalysisResult> {
        let span = node.span;

        if let Some(_generic_params) = node.generic_params.as_ref() {
            let base_ty = if let ast_type::TyKind::Reference(rty) = node.ty.kind.as_ref() {
                rty.get_base_type()
            } else {
                todo!("Error")
            };

            match base_ty.kind.as_ref() {
                ast_type::TyKind::UserDefined(udt) => {
                    let symbol_kind = self.lookup_symbol(udt.name.symbol()).ok_or_else(|| {
                        SemanticError::Undeclared {
                            name: udt.name.symbol(),
                            span: node.span,
                        }
                    })?;

                    match &mut symbol_kind.borrow_mut().kind {
                        SymbolTableValueKind::Generic(ref mut generic_info) => {
                            match &mut generic_info.kind {
                                GenericKind::Struct(info) => {
                                    info.impl_info = Some(GenericImplInfo::new(node, span));
                                }
                                GenericKind::Enum(info) => {
                                    info.impl_info = Some(GenericImplInfo::new(node, span));
                                }
                                _ => todo!("Error"),
                            }
                        }

                        _ => todo!("Error"),
                    }

                    // Generic impls are not created immediately, but are created when they are used.
                    return Ok(TopLevelAnalysisResult::GenericTopLevel);
                }

                ast_type::TyKind::Slice(_) => {
                    let slice_symbol: Symbol = "__builtin_slice".to_owned().into();
                    let symbol_kind =
                        self.lookup_symbol(slice_symbol)
                            .ok_or(SemanticError::Undeclared {
                                name: slice_symbol,
                                span: node.span,
                            })?;

                    match &mut symbol_kind.borrow_mut().kind {
                        SymbolTableValueKind::Generic(ref mut generic_info) => {
                            match &mut generic_info.kind {
                                GenericKind::Slice(info) => {
                                    info.impl_info = Some(GenericImplInfo::new(node, span));
                                }
                                _ => todo!("Error"),
                            }
                        }

                        _ => todo!("Error"),
                    }

                    return Ok(TopLevelAnalysisResult::GenericTopLevel);
                }

                _ => todo!("Error"),
            }
        }

        let mut methods = vec![];

        let ast_ty = Rc::new(node.ty);

        for item in node.items.iter() {
            match &item.kind {
                ast::top::TopLevelKind::Fn(fn_) => {
                    methods.push(self.analyze_method(ast_ty.clone(), fn_.clone())?)
                }
                _ => todo!("Error"),
            }
        }

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Impl(
            Rc::new(ir::top::Impl { methods }),
        )))
    }

    fn analyze_method(
        &mut self,
        ty: Rc<ast_type::Ty>,
        mut node: ast::top::Fn,
    ) -> anyhow::Result<Rc<ir::top::Fn>> {
        // If the method isn't static, insert self to the front of the parameters
        if let Some(mutability) = node.decl.self_ {
            node.decl.params.v.insert(
                0,
                ast::top::Param {
                    name: Ident::new("self".to_owned().into(), node.span),
                    ty: ast_type::change_mutability_dup(ty.clone(), mutability),
                    default: None,
                },
            );
        }

        let parent_ty = self.analyze_type(&ty)?;

        let (parent_name, is_builtin) = match parent_ty.kind.as_ref() {
            ir::ty::TyKind::Reference(ty) => {
                let base_ty = ty.get_base_type();
                match base_ty.kind.as_ref() {
                    ir::ty::TyKind::UserDefined(udt) => (udt.name(), false),
                    ir::ty::TyKind::Slice(elem_ty) => {
                        (self.slice_method_parent_name(elem_ty), false)
                    }
                    _ => (Symbol::from(base_ty.kind.to_string()), true),
                }
            }

            // Built-in types
            ir::ty::TyKind::Fundamental(fty) => (Symbol::from(fty.kind.to_string()), true),
            ir::ty::TyKind::Slice(elem_ty) => (self.slice_method_parent_name(elem_ty), false),

            _ => unreachable!(),
        };

        node.decl.name = Ident::new(
            self.create_method_key(
                parent_name,
                node.decl.name.symbol(),
                node.decl.self_.is_none(),
            ),
            node.decl.name.span(),
        );

        node.decl.self_ = None;

        if is_builtin {
            self.with_root_module(|analyzer| analyzer.analyze_fn_internal(node))
        } else {
            self.analyze_fn_internal(node)
        }
    }

    pub fn analyze_fn(&mut self, node: ast::top::Fn) -> anyhow::Result<TopLevelAnalysisResult> {
        assert_eq!(node.decl.self_, None);

        let vis = node.decl.vis;
        let name = node.decl.name.symbol();

        // If the function is generic, register it in the symbol table and return early.
        if node.decl.generic_params.is_some() {
            if matches!(
                self.context.analyze_command(),
                AnalyzeCommand::WithoutFnDeclare
            ) {
                return Ok(TopLevelAnalysisResult::GenericTopLevel);
            }

            let span = node.span;

            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(
                    GenericInfo::new(
                        GenericKind::Func(GenericFuncInfo { ast: node }),
                        self.current_module_path().clone(),
                    )
                    .into(),
                ),
                self.current_module_path().clone(),
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

            // Generic functions are not generated immediately, but are generated when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Fn(
            self.analyze_fn_internal(node)?,
        )))
    }

    pub fn analyze_fn_internal(&mut self, node: ast::top::Fn) -> anyhow::Result<Rc<ir::top::Fn>> {
        assert_eq!(node.decl.self_, None);

        let fn_decl = if let AnalyzeCommand::WithoutFnDeclare = self.context.analyze_command() {
            // Lookup the function declaration from the symbol table
            let symbol_table_value =
                self.lookup_symbol(node.decl.name.symbol())
                    .ok_or(SemanticError::Undeclared {
                        name: node.decl.name.symbol(),
                        span: node.span,
                    })?;

            let borrowed = symbol_table_value.borrow();
            match &borrowed.kind {
                SymbolTableValueKind::Function(fn_decl) => fn_decl.as_ref().clone(),
                _ => unreachable!(),
            }
        } else {
            self.analyze_fn_decl(node.decl)?
        };

        if matches!(
            self.context.analyze_command(),
            AnalyzeCommand::OnlyFnDeclare
        ) {
            return Ok(Rc::new(ir::top::Fn {
                decl: fn_decl,
                body: None,
            }));
        }

        self.context.set_current_function(Rc::new(fn_decl.clone()));

        // Create a new symbol table for the function parameters.
        {
            let mut symbol_table = SymbolTable::new();
            for param in fn_decl.params.iter() {
                let symbol_table_value = SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo {
                        ty: param.ty.clone(),
                    }),
                    self.current_module_path().clone(),
                );

                symbol_table.insert(
                    param.name,
                    Rc::new(RefCell::new(symbol_table_value)),
                    node.span,
                )?;
            }
            self.push_scope(symbol_table);
        }

        let mut fn_body = self.analyze_block(&node.body)?;

        // Delay inference for generated generic functions that still carry unresolved
        // type variables in their signatures. These are resolved from call-site
        // constraints later in type inference.
        if !Self::fn_decl_contains_type_var(&fn_decl) {
            self.infer_function_body_inline(&mut fn_body, &fn_decl)?;
        }

        let fn_ = Rc::new(ir::top::Fn {
            decl: fn_decl,
            body: Some(fn_body),
        });

        // Pop the function symbol table.
        self.pop_scope();

        self.context.pop_current_function();

        Ok(fn_)
    }

    fn analyze_fn_decl(&mut self, node: ast::top::FnDecl) -> anyhow::Result<ir::top::FnDecl> {
        let name = node.name.symbol();

        let qualified_name = if name.as_str() == "main" {
            QualifiedSymbol::new(ModulePath::new(vec![]), "kdmain".to_owned().into())
        } else {
            QualifiedSymbol::new(self.current_module_path().clone(), name)
        };

        let params = node
            .params
            .v
            .into_iter()
            .map(|p| -> anyhow::Result<ir::top::Param> {
                let ty = self.analyze_type(&p.ty)?;
                let default = if let Some(default) = p.default {
                    let analyzed_default =
                        self.analyze_expr_with_expected_type(default.as_ref(), ty.clone())?;

                    if !ir::ty::is_same_type(&ty, &analyzed_default.ty) {
                        return Err(SemanticError::MismatchedTypes {
                            types: (format!("{ty:?}"), format!("{:?}", analyzed_default.ty)),
                            span: analyzed_default.span,
                        }
                        .into());
                    }

                    Some(Rc::new(analyzed_default))
                } else {
                    None
                };

                Ok(ir::top::Param {
                    name: p.name.symbol(),
                    ty,
                    default,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let fn_decl = ir::top::FnDecl {
            lang_linkage: node.lang_linkage,
            link_once: node.link_once,
            name: qualified_name,
            is_c_variadic: matches!(node.params.variadic, ast::top::VariadicKind::C),
            params,
            return_ty: self.analyze_type(&node.return_ty)?,
        };

        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Function(Rc::new(fn_decl.clone())),
            self.current_module_path().clone(),
        );

        self.insert_symbol_to_root_scope(name, symbol_table_value, node.vis, node.span)?;

        Ok(fn_decl)
    }

    pub fn analyze_struct(
        &mut self,
        node: ast::top::Struct,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let vis = node.vis;
        let name = node.name.symbol();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(
                    GenericInfo::new(
                        GenericKind::Struct(GenericStructInfo::new(node)),
                        self.current_module_path().clone(),
                    )
                    .into(),
                ),
                self.current_module_path().clone(),
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

            // Generic structs are not created immediately, but are created when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        let qualified_name = QualifiedSymbol::new(self.current_module_path().clone(), name);

        // This placeholder allows recursive struct definitions
        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Placeholder(qualified_name.clone()),
            self.current_module_path().clone(),
        );
        self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

        let fields = node
            .fields
            .into_iter()
            .map(|field| -> anyhow::Result<ir::top::StructField> {
                Ok(ir::top::StructField {
                    name: field.name.symbol(),
                    ty: self.analyze_type(&field.ty)?,
                    offset: field.offset,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let ir = Rc::new(ir::top::Struct {
            name: qualified_name,
            fields,
        });

        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Struct(ir.clone()),
            self.current_module_path().clone(),
        );

        self.insert_symbol_to_root_scope(name, symbol_table_value, node.vis, span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Struct(
            ir,
        )))
    }

    pub fn analyze_enum(&mut self, node: ast::top::Enum) -> anyhow::Result<TopLevelAnalysisResult> {
        let vis = node.vis;
        let name = node.name.symbol();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(
                    GenericInfo::new(
                        GenericKind::Enum(GenericEnumInfo::new(node)),
                        self.current_module_path().clone(),
                    )
                    .into(),
                ),
                self.current_module_path().clone(),
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

            // Generics are not created immediately, but are created when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        let qualified_name = QualifiedSymbol::new(self.current_module_path().clone(), name);

        // This placeholder allows recursive enum definitions
        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Placeholder(qualified_name.clone()),
            self.current_module_path().clone(),
        );
        self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

        let variants = node
            .variants
            .into_iter()
            .map(|variant| -> anyhow::Result<ir::top::EnumVariant> {
                Ok(ir::top::EnumVariant {
                    name: variant.name.symbol(),
                    ty: match variant.ty {
                        None => None,
                        Some(ty) => Some(self.analyze_type(&ty)?),
                    },
                    offset: variant.offset,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let ir = Rc::new(ir::top::Enum {
            name: qualified_name,
            variants,
        });

        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Enum(ir.clone()),
            self.current_module_path().clone(),
        );

        self.insert_symbol_to_root_scope(name, symbol_table_value, vis, span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Enum(
            ir,
        )))
    }

    pub fn analyze_type_alias(
        &mut self,
        node: ast::top::TypeAlias,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let name = node.name.symbol();
        let span = node.span;

        let resolved_ty = self.analyze_type(&node.aliased_type)?;

        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::TypeAlias(resolved_ty),
            self.current_module_path().clone(),
        );

        self.insert_symbol_to_root_scope(name, symbol_table_value, node.vis, span)?;

        Ok(TopLevelAnalysisResult::Imported(vec![]))
    }
}
