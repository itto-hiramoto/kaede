use std::rc::Rc;

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::ty as ir_type;
use kaede_ir::{self as ir, qualified_symbol::QualifiedSymbol};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

use crate::context::AnalyzeCommand;
use crate::{error::SemanticError, SemanticAnalyzer, TopLevelAnalysisResult};
use kaede_symbol_table::{GenericKind, ResolvedGenericParams, SymbolTableValueKind};

struct GenericTypeOps<T> {
    get_generic_params: fn(&T) -> &ast::top::GenericParams,
    get_name: fn(&T) -> Ident,
    clone_and_clear: fn(&T, Ident) -> T,
    analyze: fn(&mut SemanticAnalyzer, T) -> anyhow::Result<TopLevelAnalysisResult>,
    extract_type: fn(&SymbolTableValueKind) -> ir_type::UserDefinedTypeKind,
}

impl SemanticAnalyzer {
    pub(crate) fn verify_resolved_generic_bounds(
        &self,
        resolved_params: Option<&ResolvedGenericParams>,
        generic_args: &[Rc<ir_type::Ty>],
        span: Span,
    ) -> anyhow::Result<()> {
        let Some(resolved_params) = resolved_params else {
            return Ok(());
        };

        if resolved_params.len() != generic_args.len() {
            return Err(SemanticError::GenericArgumentLengthMismatch {
                expected: resolved_params.len(),
                actual: generic_args.len(),
                span,
            }
            .into());
        }

        for (param, arg) in resolved_params.params.iter().zip(generic_args.iter()) {
            let Some(interface) = &param.bound else {
                continue;
            };

            if matches!(arg.kind.as_ref(), ir_type::TyKind::Var(_)) {
                continue;
            }

            self.verify_type_satisfies_interface(arg, interface, span)?;
        }

        Ok(())
    }

    fn verify_type_satisfies_interface(
        &self,
        actual_ty: &Rc<ir_type::Ty>,
        interface_name: &QualifiedSymbol,
        span: Span,
    ) -> anyhow::Result<()> {
        let interface_symbol = self.lookup_qualified_symbol(interface_name.clone()).ok_or(
            SemanticError::Undeclared {
                name: interface_name.symbol(),
                span,
            },
        )?;

        let interface = {
            let borrowed = interface_symbol.borrow();
            match &borrowed.kind {
                SymbolTableValueKind::Interface(interface) => interface.clone(),
                _ => unreachable!(),
            }
        };

        self.resolve_interface_impl_methods(actual_ty, &interface)
            .map_err(|method_name| SemanticError::GenericBoundNotSatisfied {
                actual: actual_ty.kind.to_string(),
                interface: interface.name.symbol(),
                method_name,
                span,
            })?;

        Ok(())
    }

    /// Resolve the concrete `FnDecl`s implementing each method of `interface` for `actual_ty`.
    ///
    /// Returns the ordered list aligned with `interface.methods`, or the name of the
    /// first method that is missing or has an incompatible signature.
    pub(crate) fn resolve_interface_impl_methods(
        &self,
        actual_ty: &Rc<ir_type::Ty>,
        interface: &ir::top::Interface,
    ) -> Result<Vec<Rc<ir::top::FnDecl>>, Symbol> {
        let mut impls = Vec::with_capacity(interface.methods.len());
        for method in &interface.methods {
            let Some(actual_method) = self.lookup_method_for_interface(actual_ty, method) else {
                return Err(method.name);
            };
            if !self.interface_method_matches_decl(method, &actual_method) {
                return Err(method.name);
            }
            impls.push(actual_method);
        }
        Ok(impls)
    }

    fn lookup_method_for_interface(
        &self,
        actual_ty: &Rc<ir_type::Ty>,
        method: &ir::top::InterfaceMethod,
    ) -> Option<Rc<ir::top::FnDecl>> {
        let (module_path, parent_name) = self.method_lookup_target(actual_ty)?;
        let method_key = self.create_method_key(parent_name, method.name, method.self_.is_none());

        let symbol = self.lookup_qualified_symbol(QualifiedSymbol::new(module_path, method_key))?;
        let borrowed = symbol.borrow();

        match &borrowed.kind {
            SymbolTableValueKind::Function(fn_decl) => Some(fn_decl.clone()),
            _ => None,
        }
    }

    fn method_lookup_target(
        &self,
        ty: &Rc<ir_type::Ty>,
    ) -> Option<(kaede_ir::module_path::ModulePath, Symbol)> {
        match ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => self.method_lookup_target(&rty.get_base_type()),
            ir_type::TyKind::UserDefined(udt) => Some((udt.module_path(), udt.name())),
            ir_type::TyKind::Fundamental(fty) => Some((
                kaede_ir::module_path::ModulePath::new(vec![]),
                fty.kind.to_string().into(),
            )),
            ir_type::TyKind::Slice(elem_ty) => Some((
                kaede_ir::module_path::ModulePath::new(vec![]),
                self.slice_method_parent_name(elem_ty),
            )),
            _ => None,
        }
    }

    fn interface_method_matches_decl(
        &self,
        interface_method: &ir::top::InterfaceMethod,
        actual_method: &Rc<ir::top::FnDecl>,
    ) -> bool {
        let actual_params = &actual_method.params;
        let expected_is_instance = interface_method.self_.is_some();
        let actual_is_instance = actual_params
            .first()
            .is_some_and(|param| param.name.as_str() == "self");

        if expected_is_instance != actual_is_instance {
            return false;
        }

        let actual_params_without_self = if actual_is_instance {
            let self_param = &actual_params[0];
            if self_param.ty.mutability != interface_method.self_.unwrap() {
                return false;
            }
            &actual_params[1..]
        } else {
            &actual_params[..]
        };

        actual_params_without_self.len() == interface_method.params.len()
            && actual_params_without_self
                .iter()
                .zip(interface_method.params.iter())
                .all(|(actual, expected)| ir_type::is_same_type(&actual.ty, &expected.ty))
            && ir_type::is_same_type(&actual_method.return_ty, &interface_method.return_ty)
    }

    pub fn analyze_type(&mut self, ty: &ast_type::Ty) -> anyhow::Result<Rc<ir_type::Ty>> {
        match ty.kind.as_ref() {
            ast_type::TyKind::Fundamental(fty) => Ok(self
                .analyze_fundamental_type(fty, ty.mutability.into())
                .into()),

            ast_type::TyKind::UserDefined(udt) => {
                self.analyze_user_defined_type(udt, ty.mutability.into())
            }

            ast_type::TyKind::Reference(rty) => Ok(self
                .analyze_reference_type(rty, ty.mutability.into())?
                .into()),
            ast_type::TyKind::Pointer(pty) => {
                Ok(self.analyze_pointer_type(pty, ty.mutability.into())?.into())
            }

            ast_type::TyKind::Array((ety, length)) => Ok(self
                .analyze_array_type(ety, *length, ty.mutability.into())?
                .into()),
            ast_type::TyKind::Slice(elem_ty) => Ok(self
                .analyze_slice_type(elem_ty, ty.mutability.into())?
                .into()),
            ast_type::TyKind::Tuple(etys) => Ok(self
                .analyze_tuple_type(
                    etys.iter()
                        .map(AsRef::as_ref)
                        .collect::<Vec<_>>()
                        .as_slice(),
                    ty.mutability.into(),
                )?
                .into()),

            ast_type::TyKind::Generic(gty) => self.analyze_generic_type(gty, ty.span),

            ast_type::TyKind::External(inner_ty, access_chain) => {
                let outer_mutability = ty.mutability;
                let (_, module_path) = self.create_module_path_from_access_chain(
                    access_chain
                        .iter()
                        .map(|i| i.symbol())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    inner_ty.span,
                )?;

                let analyzed_ty =
                    self.with_module(module_path, |analyzer| analyzer.analyze_type(inner_ty))?;

                // Apply outer mutability to the analyzed type
                Ok(ir_type::change_mutability_dup(
                    analyzed_ty,
                    outer_mutability.into(),
                ))
            }

            ast_type::TyKind::Unit => Ok(ir_type::Ty::new_unit().into()),
            ast_type::TyKind::Never => Ok(ir_type::Ty::new_never().into()),
            ast_type::TyKind::Var => Ok(self.infer_context.fresh()),
            ast_type::TyKind::Closure(closure) => {
                self.analyze_closure_type(closure, ty.mutability.into())
            }
        }
    }

    fn analyze_generic_type(
        &self,
        gty: &ast_type::GenericType,
        span: Span,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let name = gty.name.symbol();

        match self.lookup_generic_argument(name) {
            Some(ty) => Ok(ty),
            None => Err(SemanticError::Undeclared { name, span }.into()),
        }
    }

    fn analyze_fundamental_type(
        &self,
        fty: &ast_type::FundamentalType,
        mutability: ir_type::Mutability,
    ) -> ir_type::Ty {
        let make = |kind| ir_type::make_fundamental_type(kind, mutability);

        match fty.kind {
            ast_type::FundamentalTypeKind::Str => make(ir_type::FundamentalTypeKind::Str),
            ast_type::FundamentalTypeKind::Bool => make(ir_type::FundamentalTypeKind::Bool),
            ast_type::FundamentalTypeKind::I8 => make(ir_type::FundamentalTypeKind::I8),
            ast_type::FundamentalTypeKind::U8 => make(ir_type::FundamentalTypeKind::U8),
            ast_type::FundamentalTypeKind::I16 => make(ir_type::FundamentalTypeKind::I16),
            ast_type::FundamentalTypeKind::U16 => make(ir_type::FundamentalTypeKind::U16),
            ast_type::FundamentalTypeKind::I32 => make(ir_type::FundamentalTypeKind::I32),
            ast_type::FundamentalTypeKind::U32 => make(ir_type::FundamentalTypeKind::U32),
            ast_type::FundamentalTypeKind::I64 => make(ir_type::FundamentalTypeKind::I64),
            ast_type::FundamentalTypeKind::U64 => make(ir_type::FundamentalTypeKind::U64),
            ast_type::FundamentalTypeKind::Char => make(ir_type::FundamentalTypeKind::Char),
        }
    }

    fn verify_generic_argument_length(
        &self,
        params: &ast::top::GenericParams,
        args: &[Rc<ir_type::Ty>],
        span: Span,
    ) -> anyhow::Result<()> {
        if params.len() != args.len() {
            return Err(SemanticError::GenericArgumentLengthMismatch {
                expected: params.len(),
                actual: args.len(),
                span,
            }
            .into());
        }

        Ok(())
    }

    fn instantiate_generic_type<T>(
        &mut self,
        ast: &T,
        generic_args: &[Rc<ir_type::Ty>],
        ops: GenericTypeOps<T>,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let generic_params = (ops.get_generic_params)(ast);
        let generic_instance = ir_type::GenericInstanceInfo::new(
            QualifiedSymbol::new(
                self.current_module_path().clone(),
                (ops.get_name)(ast).symbol(),
            ),
            generic_args.to_vec(),
        );

        self.verify_generic_argument_length(generic_params, generic_args, generic_params.span)?;

        let generated_generic_key =
            self.create_generated_generic_key((ops.get_name)(ast).symbol(), generic_args);
        let modified_ast = (ops.clone_and_clear)(
            ast,
            Ident::new(generated_generic_key, (ops.get_name)(ast).span()),
        );

        if self.generating_generics.contains(&generated_generic_key) {
            // Circular dependency detected - return a placeholder type
            return Ok(Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::UserDefined(
                    ir_type::UserDefinedType::with_generic_instance(
                        ir_type::UserDefinedTypeKind::Placeholder(QualifiedSymbol::new(
                            self.current_module_path().clone(),
                            generated_generic_key,
                        )),
                        generic_instance.clone(),
                    ),
                )
                .into(),
                mutability: ir_type::Mutability::Not,
            }));
        }

        self.generating_generics.insert(generated_generic_key);

        let symbol = self.with_generic_arguments(generic_params, generic_args, |analyzer| {
            // If the symbol is already generated, return it
            if let Some(symbol) = analyzer.lookup_symbol(generated_generic_key) {
                return Ok(symbol);
            }

            let top_level = analyzer
                .with_pending_generic_instance(Some(generic_instance.clone()), |analyzer| {
                    (ops.analyze)(analyzer, modified_ast)
                })?;

            if let TopLevelAnalysisResult::TopLevel(top_level) = top_level {
                analyzer.generated_generics.push(top_level);
            } else {
                unreachable!()
            }

            Ok(analyzer.lookup_symbol(generated_generic_key).unwrap())
        })?;

        let symbol_value = symbol.borrow();
        let udt_ir = ir_type::TyKind::UserDefined(ir_type::UserDefinedType::with_generic_instance(
            (ops.extract_type)(&symbol_value.kind),
            generic_instance,
        ));

        Ok(Rc::new(ir_type::Ty {
            kind: udt_ir.into(),
            mutability: ir_type::Mutability::Not,
        }))
    }

    fn create_generic_struct(
        &mut self,
        ast: &ast::top::Struct,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        self.instantiate_generic_type(
            ast,
            generic_args,
            GenericTypeOps {
                get_generic_params: |ast| ast.generic_params.as_ref().unwrap(),
                get_name: |ast| ast.name,
                clone_and_clear: |ast, new_name| {
                    let mut cloned = ast.clone();
                    cloned.generic_params = None;
                    cloned.name = new_name;
                    cloned
                },
                analyze: |analyzer, ast| analyzer.analyze_struct(ast),
                extract_type: |kind| match kind {
                    SymbolTableValueKind::Struct(st) => {
                        ir_type::UserDefinedTypeKind::Struct(st.clone())
                    }
                    _ => unreachable!(),
                },
            },
        )
    }

    fn create_generic_enum(
        &mut self,
        ast: &ast::top::Enum,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        self.instantiate_generic_type(
            ast,
            generic_args,
            GenericTypeOps {
                get_generic_params: |ast| ast.generic_params.as_ref().unwrap(),
                get_name: |ast| ast.name,
                clone_and_clear: |ast, new_name| {
                    let mut cloned = ast.clone();
                    cloned.generic_params = None;
                    cloned.name = new_name;
                    cloned
                },
                analyze: |analyzer, ast| analyzer.analyze_enum(ast),
                extract_type: |kind| match kind {
                    SymbolTableValueKind::Enum(en) => {
                        ir_type::UserDefinedTypeKind::Enum(en.clone())
                    }
                    _ => unreachable!(),
                },
            },
        )
    }

    fn create_generic_type(
        &mut self,
        udt: &ast_type::UserDefinedType,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        assert!(udt.generic_args.is_some());

        let generic_args = udt
            .generic_args
            .as_ref()
            .unwrap()
            .types
            .iter()
            .map(|ty| self.analyze_type(ty))
            .collect::<anyhow::Result<Vec<_>>>()?;

        self.create_generic_type_with_args(udt.name, generic_args)
    }

    fn create_generic_type_with_args(
        &mut self,
        name: Ident,
        generic_args: Vec<Rc<ir_type::Ty>>,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let symbol = self
            .lookup_symbol(name.symbol())
            .ok_or(SemanticError::Undeclared {
                name: name.symbol(),
                span: name.span(),
            })?;

        let borrowed_symbol = symbol.borrow();

        let generic_info = match &borrowed_symbol.kind {
            SymbolTableValueKind::Generic(generic_info) => generic_info,
            _ => unreachable!(),
        };

        self.verify_resolved_generic_bounds(
            generic_info.resolved_generic_params(),
            &generic_args,
            name.span(),
        )?;

        let module_path = generic_info.module_path.clone();

        self.with_module(module_path.clone(), |analyzer| {
            let generic_info = match &borrowed_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            // Create the user defined type with the generic arguments
            let impl_info = match &generic_info.kind {
                GenericKind::Struct(info) => info.impl_info.as_ref(),
                GenericKind::Enum(info) => info.impl_info.as_ref(),
                _ => unreachable!(),
            };

            // Create methods
            if impl_info.is_some() {
                // To avoid borrow checker error
                drop(borrowed_symbol);

                let mut borrowed_mut_symbol = symbol.borrow_mut();

                let generic_info = match &mut borrowed_mut_symbol.kind {
                    SymbolTableValueKind::Generic(generic_info) => generic_info,
                    _ => unreachable!(),
                };

                let impl_info = match &mut generic_info.kind {
                    GenericKind::Struct(info) => info.impl_info.as_mut().unwrap(),
                    GenericKind::Enum(info) => info.impl_info.as_mut().unwrap(),
                    _ => unreachable!(),
                };

                // Check if the impl has already been generated for the exact generic arguments.
                // `Ty` equality treats type vars as wildcards, so we need a stricter comparison here.
                let already_generated = impl_info.generateds.iter().any(|args| {
                    args.len() == generic_args.len()
                        && args.iter().zip(&generic_args).all(|(a, b)| {
                            match (a.kind.as_ref(), b.kind.as_ref()) {
                                (ir_type::TyKind::Var(aid), ir_type::TyKind::Var(bid)) => {
                                    aid == bid
                                }
                                (ir_type::TyKind::Var(_), _) | (_, ir_type::TyKind::Var(_)) => {
                                    false
                                }
                                _ => a.kind == b.kind,
                            }
                        })
                });

                if !already_generated {
                    impl_info.generateds.push(generic_args.clone());

                    let generic_params = impl_info.impl_.generic_params.as_ref().unwrap().clone();
                    let resolved_generic_params = impl_info.resolved_generic_params.clone();

                    // Check if the length of the generic arguments is the same as the number of generic parameters
                    if generic_params.len() != generic_args.len() {
                        return Err(SemanticError::GenericArgumentLengthMismatch {
                            expected: generic_params.len(),
                            actual: generic_args.len(),
                            span: generic_params.span,
                        }
                        .into());
                    }

                    analyzer.verify_resolved_generic_bounds(
                        resolved_generic_params.as_ref(),
                        &generic_args,
                        generic_params.span,
                    )?;

                    let mut impl_ = impl_info.impl_.clone();
                    impl_.generic_params = None;

                    let mut methods = vec![];

                    // Since the generic function is generated multiple times,
                    // we need to set link_once to true to avoid errors
                    for method in impl_.items.iter() {
                        if let ast::top::TopLevelKind::Fn(fn_) = &method.kind {
                            let mut fn_decl = fn_.decl.clone();
                            fn_decl.link_once = true;

                            methods.push(ast::top::TopLevel {
                                kind: ast::top::TopLevelKind::Fn(ast::top::Fn {
                                    decl: fn_decl,
                                    body: fn_.body.clone(),
                                    span: fn_.span,
                                }),
                                span: fn_.span,
                            });
                        }
                    }

                    impl_.items = Rc::new(methods);

                    // To avoid borrow checker error
                    drop(borrowed_mut_symbol);

                    analyzer.with_generic_arguments(
                        &generic_params,
                        &generic_args,
                        |analyzer| {
                            let generic_instance = ir_type::GenericInstanceInfo::new(
                                QualifiedSymbol::new(module_path.clone(), name.symbol()),
                                generic_args.clone(),
                            );
                            analyzer.with_analyze_command(
                                AnalyzeCommand::OnlyFnDeclare,
                                |analyzer| {
                                    analyzer.with_pending_generic_instance(
                                        Some(generic_instance.clone()),
                                        |analyzer| analyzer.analyze_impl(impl_.clone()),
                                    )?;
                                    Ok::<(), anyhow::Error>(())
                                },
                            )?;

                            let impl_ir = analyzer.with_analyze_command(
                                AnalyzeCommand::WithoutFnDeclare,
                                |analyzer| {
                                    analyzer.with_pending_generic_instance(
                                        Some(generic_instance),
                                        |analyzer| analyzer.analyze_impl(impl_),
                                    )
                                },
                            )?;

                            if let TopLevelAnalysisResult::TopLevel(top_level) = impl_ir {
                                assert!(matches!(top_level, ir::top::TopLevel::Impl(_)));
                                analyzer.generated_generics.push(top_level);
                            }

                            Ok::<(), anyhow::Error>(())
                        },
                    )?;
                }
            }

            let borrowed_symbol = symbol.borrow();

            let generic_info = match &borrowed_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            // Create the user defined type with the generic arguments
            let (struct_ast, enum_ast) = match &generic_info.kind {
                GenericKind::Struct(info) => (Some(info.ast.clone()), None),
                GenericKind::Enum(info) => (None, Some(info.ast.clone())),
                _ => unreachable!(),
            };

            // To avoid borrow checker error
            drop(borrowed_symbol);

            if let Some(struct_ast) = struct_ast {
                analyzer.create_generic_struct(&struct_ast, &generic_args)
            } else if let Some(enum_ast) = enum_ast {
                analyzer.create_generic_enum(&enum_ast, &generic_args)
            } else {
                unreachable!()
            }
        })
    }

    pub fn analyze_user_defined_type(
        &mut self,
        udt: &ast_type::UserDefinedType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        // If this is a generic type, the type is generated here.
        if udt.generic_args.is_some() {
            // Generic methods must be defined, so NoCommand
            return self.with_analyze_command(AnalyzeCommand::NoCommand, |analyzer| {
                analyzer.create_generic_type(udt)
            });
        }

        if let Some(generic_arg) = self.lookup_generic_argument(udt.name.symbol()) {
            return Ok(generic_arg);
        }

        let symbol = self
            .lookup_symbol(udt.name.symbol())
            .ok_or(SemanticError::Undeclared {
                name: udt.name.symbol(),
                span: udt.name.span(),
            })?;

        let borrowed_symbol = symbol.borrow();

        let udt_kind = match &borrowed_symbol.kind {
            SymbolTableValueKind::TypeAlias(ty) => {
                return Ok(ir_type::change_mutability_dup(
                    match ty.kind.as_ref() {
                        ir_type::TyKind::Reference(rty) => {
                            match rty.get_base_type().kind.as_ref() {
                                ir_type::TyKind::UserDefined(_) => rty.get_base_type(),
                                _ => ty.clone(),
                            }
                        }
                        _ => ty.clone(),
                    },
                    mutability,
                ))
            }

            SymbolTableValueKind::Struct(st) => ir_type::UserDefinedTypeKind::Struct(st.clone()),
            SymbolTableValueKind::Enum(en) => ir_type::UserDefinedTypeKind::Enum(en.clone()),
            SymbolTableValueKind::Interface(iface) => {
                ir_type::UserDefinedTypeKind::Interface(iface.clone())
            }
            SymbolTableValueKind::Placeholder(placeholder) => {
                ir_type::UserDefinedTypeKind::Placeholder(placeholder.clone())
            }

            SymbolTableValueKind::Generic(generic_info) => {
                // Generic arguments are not provided.
                return Err(SemanticError::GenericArgumentLengthMismatch {
                    expected: generic_info.get_generic_argument_length(),
                    actual: 0,
                    span: udt.name.span(),
                }
                .into());
            }

            _ => unreachable!(),
        };

        Ok(Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::UserDefined(ir_type::UserDefinedType::new(udt_kind)).into(),
            mutability,
        }))
    }

    pub fn analyze_user_defined_type_for_expr(
        &mut self,
        udt: &ast_type::UserDefinedType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        if udt.generic_args.is_some() {
            return self.analyze_user_defined_type(udt, mutability);
        }

        if let Some(generic_arg) = self.lookup_generic_argument(udt.name.symbol()) {
            return Ok(generic_arg);
        }

        let symbol = self
            .lookup_symbol(udt.name.symbol())
            .ok_or(SemanticError::Undeclared {
                name: udt.name.symbol(),
                span: udt.name.span(),
            })?;

        let generic_arg_len = {
            let borrowed = symbol.borrow();
            match &borrowed.kind {
                SymbolTableValueKind::Generic(generic_info) => {
                    Some(generic_info.get_generic_argument_length())
                }
                _ => None,
            }
        };

        if let Some(len) = generic_arg_len {
            let generic_args = (0..len).map(|_| self.infer_context.fresh()).collect();

            let generated = self.with_analyze_command(AnalyzeCommand::NoCommand, |analyzer| {
                analyzer.create_generic_type_with_args(udt.name, generic_args)
            })?;

            return Ok(ir_type::change_mutability_dup(generated, mutability));
        }

        self.analyze_user_defined_type(udt, mutability)
    }

    fn analyze_reference_type(
        &mut self,
        rty: &ast_type::ReferenceType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Reference(ir_type::ReferenceType {
                refee_ty: self.analyze_type(&rty.refee_ty)?,
            })
            .into(),
            mutability,
        })
    }

    fn analyze_pointer_type(
        &mut self,
        pty: &ast_type::PointerType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                pointee_ty: self.analyze_type(&pty.pointee_ty)?,
            })
            .into(),
            mutability,
        })
    }

    fn analyze_array_type(
        &mut self,
        ety: &ast_type::Ty,
        length: u32,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Array((self.analyze_type(ety)?, length)).into(),
            mutability,
        })
    }

    fn analyze_slice_type(
        &mut self,
        elem_ty: &ast_type::Ty,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        let elem_ty = self.analyze_type(elem_ty)?;

        self.generate_slice_impl(elem_ty.clone())?;

        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Slice(elem_ty).into(),
            mutability,
        })
    }

    pub(crate) fn generate_slice_impl(&mut self, elem_ty: Rc<ir_type::Ty>) -> anyhow::Result<()> {
        // Autoload hasn't been processed yet; nothing to generate.
        let Some(slice) = self.slice_intrinsic.as_ref() else {
            return Ok(());
        };

        let generic_args = vec![elem_ty.clone()];

        let already_generated = slice.impl_info.generateds.iter().any(|args| {
            args.len() == generic_args.len()
                && args.iter().zip(&generic_args).all(|(a, b)| {
                    match (a.kind.as_ref(), b.kind.as_ref()) {
                        (ir_type::TyKind::Var(_), ir_type::TyKind::Var(_)) => true,
                        (ir_type::TyKind::Var(_), _) | (_, ir_type::TyKind::Var(_)) => false,
                        _ => a.kind == b.kind,
                    }
                })
        });
        if already_generated {
            return Ok(());
        }

        let defining_module = slice.defining_module.clone();
        let generic_params = slice
            .impl_info
            .impl_
            .generic_params
            .as_ref()
            .unwrap()
            .clone();
        let resolved_generic_params = slice.impl_info.resolved_generic_params.clone();
        let mut impl_ast = slice.impl_info.impl_.clone();

        self.verify_generic_argument_length(&generic_params, &generic_args, generic_params.span)?;
        self.verify_resolved_generic_bounds(
            resolved_generic_params.as_ref(),
            &generic_args,
            generic_params.span,
        )?;

        self.slice_intrinsic
            .as_mut()
            .unwrap()
            .impl_info
            .generateds
            .push(generic_args.clone());

        impl_ast.generic_params = None;

        let methods = impl_ast
            .items
            .iter()
            .filter_map(|method| match &method.kind {
                ast::top::TopLevelKind::Fn(fn_) => {
                    let mut fn_decl = fn_.decl.clone();
                    fn_decl.link_once = true;
                    Some(ast::top::TopLevel {
                        kind: ast::top::TopLevelKind::Fn(ast::top::Fn {
                            decl: fn_decl,
                            body: fn_.body.clone(),
                            span: fn_.span,
                        }),
                        span: fn_.span,
                    })
                }
                _ => None,
            })
            .collect();
        impl_ast.items = Rc::new(methods);

        // Slice methods live in the root module (so every call site can find them with a
        // direct lookup), but symbol references inside the body resolve from the module
        // that declared `impl<T>[T]` — e.g. helpers like `__slice_from_raw_parts`.
        let impl_ir = self.with_lookup_fallback_module(defining_module, |analyzer| {
            analyzer.with_root_module(|analyzer| {
                analyzer.with_generic_arguments(&generic_params, &generic_args, |analyzer| {
                    analyzer.with_analyze_command(AnalyzeCommand::NoCommand, |analyzer| {
                        analyzer.analyze_impl(impl_ast.clone())
                    })
                })
            })
        })?;

        if let TopLevelAnalysisResult::TopLevel(top_level) = impl_ir {
            assert!(matches!(top_level, ir::top::TopLevel::Impl(_)));
            self.generated_generics.push(top_level);
        }

        Ok(())
    }

    fn analyze_tuple_type(
        &mut self,
        etys: &[&ast_type::Ty],
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        let etys = etys
            .iter()
            .map(|ety| self.analyze_type(ety))
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Tuple(etys.into_iter().collect()).into(),
            mutability,
        })
    }

    fn analyze_closure_type(
        &mut self,
        closure: &ast_type::ClosureType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let param_tys = closure
            .param_tys
            .iter()
            .map(|ty| self.analyze_type(ty))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let ret_ty = self.analyze_type(&closure.ret_ty)?;

        Ok(Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::Closure(ir_type::ClosureType {
                param_tys,
                ret_ty,
                captures: Vec::new(),
            })
            .into(),
            mutability,
        }))
    }
}
