use std::rc::Rc;

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::ty as ir_type;
use kaede_ir::{self as ir, qualified_symbol::QualifiedSymbol};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

use crate::context::AnalyzeCommand;
use crate::{error::SemanticError, SemanticAnalyzer, TopLevelAnalysisResult};
use kaede_symbol_table::{
    GenericImplInfo, GenericKind, ResolvedGenericParams, SymbolTableValueKind,
};

/// `GenericImplInfo` fields copied for one monomorphization pass (decl or body).
struct GenericImplSnapshot {
    generic_params: ast::top::GenericParams,
    resolved_generic_params: Option<ResolvedGenericParams>,
    impl_: ast::top::Impl,
}

struct GenericTypeOps<T> {
    get_generic_params: fn(&T) -> &ast::top::GenericParams,
    get_name: fn(&T) -> Ident,
    clone_and_clear: fn(&T, Ident) -> T,
    analyze: fn(&mut SemanticAnalyzer, T) -> anyhow::Result<TopLevelAnalysisResult>,
    extract_type: fn(&SymbolTableValueKind) -> ir_type::UserDefinedTypeKind,
}

impl SemanticAnalyzer {
    fn generic_impl_info(kind: &GenericKind) -> Option<&GenericImplInfo> {
        match kind {
            GenericKind::Struct(info) => info.impl_info.as_ref(),
            GenericKind::Enum(info) => info.impl_info.as_ref(),
            GenericKind::BuiltinSlice(info) => Some(&info.impl_info),
            GenericKind::Func(_) => None,
        }
    }

    fn generic_impl_info_mut(kind: &mut GenericKind) -> Option<&mut GenericImplInfo> {
        match kind {
            GenericKind::Struct(info) => info.impl_info.as_mut(),
            GenericKind::Enum(info) => info.impl_info.as_mut(),
            GenericKind::BuiltinSlice(info) => Some(&mut info.impl_info),
            GenericKind::Func(_) => None,
        }
    }

    fn builtin_slice_defining_module(
        kind: &GenericKind,
    ) -> Option<kaede_ir::module_path::ModulePath> {
        match kind {
            GenericKind::BuiltinSlice(info) => Some(info.defining_module.clone()),
            _ => None,
        }
    }

    fn create_generic_impl_snapshot(impl_info: &GenericImplInfo) -> GenericImplSnapshot {
        GenericImplSnapshot {
            generic_params: impl_info.impl_.generic_params.as_ref().unwrap().clone(),
            resolved_generic_params: impl_info.resolved_generic_params.clone(),
            impl_: impl_info.impl_.clone(),
        }
    }

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
            if !self.interface_method_matches_decl(
                method,
                &actual_method,
                &interface.name,
                actual_ty,
            ) {
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
                kaede_ir::module_path::ModulePath::root(),
                fty.kind.to_string().into(),
            )),
            ir_type::TyKind::Slice(elem_ty) => Some((
                kaede_ir::module_path::ModulePath::root(),
                self.impl_method_parent_name(
                    &Self::slice_generic_origin(),
                    std::slice::from_ref(elem_ty),
                ),
            )),
            _ => None,
        }
    }

    fn interface_method_matches_decl(
        &self,
        interface_method: &ir::top::InterfaceMethod,
        actual_method: &Rc<ir::top::FnDecl>,
        interface_name: &QualifiedSymbol,
        impl_ty: &Rc<ir_type::Ty>,
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

        if actual_params_without_self.len() != interface_method.params.len() {
            return false;
        }

        // Substitute the interface's own name with the implementing type so
        // that `fun eq(self, other: Hashable)` (declared on the interface)
        // matches `impl T { fun eq(self, other: T) }`. The substitute
        // early-exits via `contains_interface` when the interface name does
        // not appear in the type, so calling it unconditionally is cheap.
        let subst =
            |ty: &Rc<ir_type::Ty>| ir_type::substitute_interface(ty, interface_name, impl_ty);

        actual_params_without_self
            .iter()
            .zip(interface_method.params.iter())
            .all(|(actual, expected)| ir_type::is_same_type(&actual.ty, &subst(&expected.ty)))
            && ir_type::is_same_type(
                &actual_method.return_ty,
                &subst(&interface_method.return_ty),
            )
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

            ast_type::TyKind::Generic(gty) => {
                self.analyze_generic_type(gty, ty.span, ty.mutability.into())
            }

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
        outer_mutability: ir_type::Mutability,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let name = gty.name.symbol();

        // The substituted type's mutability comes from the call site's argument,
        // but the source position (`T` vs `mut T`) determines the parameter's
        // mutability. Without this override, a first instantiation with a `mut`
        // argument would poison the cached fn_decl for later immutable callers.
        match self.lookup_generic_argument(name) {
            Some(ty) => Ok(ir_type::change_mutability_dup(ty, outer_mutability)),
            None => Err(SemanticError::Undeclared { name, span }.into()),
        }
    }

    fn analyze_fundamental_type(
        &self,
        fty: &ast_type::FundamentalType,
        mutability: ir_type::Mutability,
    ) -> ir_type::Ty {
        let make = |kind| ir_type::make_fundamental_type(kind, mutability);
        make(Self::fundamental_type_kind_from_ast_kind(fty.kind))
    }

    pub(crate) fn fundamental_type_kind_from_ast_ty(
        ty: &ast_type::Ty,
    ) -> Option<ir_type::FundamentalTypeKind> {
        match ty.kind.as_ref() {
            ast_type::TyKind::Fundamental(fty) => {
                Some(Self::fundamental_type_kind_from_ast_kind(fty.kind))
            }
            ast_type::TyKind::Reference(rty) => {
                Self::fundamental_type_kind_from_ast_ty(&rty.get_base_type())
            }
            _ => None,
        }
    }

    fn fundamental_type_kind_from_ast_kind(
        kind: ast_type::FundamentalTypeKind,
    ) -> ir_type::FundamentalTypeKind {
        match kind {
            ast_type::FundamentalTypeKind::Str => ir_type::FundamentalTypeKind::Str,
            ast_type::FundamentalTypeKind::Bool => ir_type::FundamentalTypeKind::Bool,
            ast_type::FundamentalTypeKind::I8 => ir_type::FundamentalTypeKind::I8,
            ast_type::FundamentalTypeKind::U8 => ir_type::FundamentalTypeKind::U8,
            ast_type::FundamentalTypeKind::I16 => ir_type::FundamentalTypeKind::I16,
            ast_type::FundamentalTypeKind::U16 => ir_type::FundamentalTypeKind::U16,
            ast_type::FundamentalTypeKind::I32 => ir_type::FundamentalTypeKind::I32,
            ast_type::FundamentalTypeKind::U32 => ir_type::FundamentalTypeKind::U32,
            ast_type::FundamentalTypeKind::I64 => ir_type::FundamentalTypeKind::I64,
            ast_type::FundamentalTypeKind::U64 => ir_type::FundamentalTypeKind::U64,
            ast_type::FundamentalTypeKind::F32 => ir_type::FundamentalTypeKind::F32,
            ast_type::FundamentalTypeKind::F64 => ir_type::FundamentalTypeKind::F64,
            ast_type::FundamentalTypeKind::Char => ir_type::FundamentalTypeKind::Char,
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

    fn link_once_impl_methods(impl_: &ast::top::Impl) -> Vec<ast::top::TopLevel> {
        Self::link_once_impl_methods_matching(impl_, |_| true)
    }

    fn link_once_impl_methods_matching(
        impl_: &ast::top::Impl,
        mut include: impl FnMut(&ast::top::Fn) -> bool,
    ) -> Vec<ast::top::TopLevel> {
        impl_
            .items
            .iter()
            .filter_map(|method| match &method.kind {
                ast::top::TopLevelKind::Fn(fn_) if include(fn_) => {
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
            .collect()
    }

    pub(crate) fn ensure_generic_impl_method_declarations(
        &mut self,
        origin: QualifiedSymbol,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<()> {
        let symbol =
            self.lookup_qualified_symbol(origin.clone())
                .ok_or(SemanticError::Undeclared {
                    name: origin.symbol(),
                    span: Span::dummy(),
                })?;

        let (mut snapshot, defining_module) = {
            let mut borrowed = symbol.borrow_mut();
            let generic_info = match &mut borrowed.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => return Ok(()),
            };

            let defining_module = Self::builtin_slice_defining_module(&generic_info.kind);

            let Some(impl_info) = Self::generic_impl_info_mut(&mut generic_info.kind) else {
                return Ok(());
            };

            if Self::generic_instantiation_already_registered(
                &impl_info.method_decl_instantiations,
                generic_args,
            ) {
                return Ok(());
            }

            impl_info
                .method_decl_instantiations
                .push(generic_args.to_vec());

            (
                Self::create_generic_impl_snapshot(impl_info),
                defining_module,
            )
        };

        self.verify_generic_argument_length(
            &snapshot.generic_params,
            generic_args,
            snapshot.generic_params.span,
        )?;

        self.verify_resolved_generic_bounds(
            snapshot.resolved_generic_params.as_ref(),
            generic_args,
            snapshot.generic_params.span,
        )?;

        snapshot.impl_.generic_params = None;
        snapshot.impl_.items = Rc::new(Self::link_once_impl_methods(&snapshot.impl_));

        let register_decls = |analyzer: &mut Self| {
            analyzer.with_generic_arguments(&snapshot.generic_params, generic_args, |analyzer| {
                analyzer.with_analyze_command(AnalyzeCommand::OnlyFnDeclare, |analyzer| {
                    analyzer.with_pending_generic_instance(
                        Some(ir_type::GenericInstanceInfo::new(
                            origin.clone(),
                            generic_args.to_vec(),
                        )),
                        |analyzer| analyzer.analyze_impl(snapshot.impl_),
                    )
                })
            })
        };

        if let Some(defining_module) = defining_module {
            self.with_root_module_and_fallback(defining_module, register_decls)?;
        } else {
            self.with_module(origin.module_path().clone(), register_decls)?;
        }

        Ok(())
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

        let module_path = {
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

            generic_info.module_path.clone()
        };

        self.with_module(module_path.clone(), |analyzer| {
            analyzer.ensure_generic_impl_method_declarations(
                QualifiedSymbol::new(module_path.clone(), name.symbol()),
                &generic_args,
            )?;

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

    pub(crate) fn ensure_generated_impl_method_body(
        &mut self,
        decl: &ir::top::FnDecl,
    ) -> anyhow::Result<()> {
        // Generic type instantiation registers method declarations first. Emit the concrete
        // method body only when the declaration is actually referenced by a call.
        let Some(instance) = &decl.generic_instance else {
            return Ok(());
        };

        let origin = instance.origin.clone();
        let generic_args = instance.args.clone();

        if self.generated_impl_method_bodies.contains(&decl.name) {
            return Ok(());
        }

        let (mut snapshot, defining_module) = {
            let symbol =
                self.lookup_qualified_symbol(origin.clone())
                    .ok_or(SemanticError::Undeclared {
                        name: origin.symbol(),
                        span: Span::dummy(),
                    })?;

            let borrowed_symbol = symbol.borrow();
            let generic_info = match &borrowed_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => return Ok(()),
            };

            let defining_module = Self::builtin_slice_defining_module(&generic_info.kind);

            let Some(impl_info) = Self::generic_impl_info(&generic_info.kind) else {
                return Ok(());
            };

            (
                Self::create_generic_impl_snapshot(impl_info),
                defining_module,
            )
        };

        // Mark before emitting the body so recursive calls (e.g. `len` calling `ne.len()`)
        // do not re-enter this function for the same monomorphized method.
        self.generated_impl_method_bodies.insert(decl.name.clone());

        self.verify_generic_argument_length(
            &snapshot.generic_params,
            &generic_args,
            snapshot.generic_params.span,
        )?;

        self.verify_resolved_generic_bounds(
            snapshot.resolved_generic_params.as_ref(),
            &generic_args,
            snapshot.generic_params.span,
        )?;

        let parent_name = self.impl_method_parent_name(&origin, &generic_args);
        let target_name = decl.name.symbol();
        snapshot.impl_.generic_params = None;
        snapshot.impl_.items = Rc::new(Self::link_once_impl_methods_matching(
            &snapshot.impl_,
            |fn_| {
                self.create_method_key(
                    parent_name,
                    fn_.decl.name.symbol(),
                    fn_.decl.self_.is_none(),
                ) == target_name
            },
        ));

        if snapshot.impl_.items.is_empty() {
            anyhow::bail!(
                "internal compiler error: could not find generated impl method body for `{}`",
                decl.name.mangle()
            );
        }

        let saved_closure_captures = std::mem::take(&mut self.closure_capture_stack);
        let emit_body = |analyzer: &mut Self| {
            analyzer.with_generic_arguments(&snapshot.generic_params, &generic_args, |analyzer| {
                analyzer.with_analyze_command(AnalyzeCommand::WithoutFnDeclare, |analyzer| {
                    analyzer.with_pending_generic_instance(
                        Some(ir_type::GenericInstanceInfo::new(
                            origin.clone(),
                            generic_args.clone(),
                        )),
                        |analyzer| analyzer.analyze_impl(snapshot.impl_),
                    )
                })
            })
        };

        let impl_ir = if let Some(slice_defining_module) = defining_module {
            self.with_root_module_and_fallback(slice_defining_module, emit_body)?
        } else {
            self.with_defining_module(origin.module_path().clone(), emit_body)?
        };
        self.closure_capture_stack = saved_closure_captures;

        if let TopLevelAnalysisResult::TopLevel(top_level) = impl_ir {
            assert!(matches!(top_level, ir::top::TopLevel::Impl(_)));
            self.generated_generics.push(top_level);
        }

        Ok(())
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
            // Source position controls mutability; the substituted type's own
            // mutability must not flow into the parameter. See `analyze_generic_type`.
            return Ok(ir_type::change_mutability_dup(generic_arg, mutability));
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
            return Ok(ir_type::change_mutability_dup(generic_arg, mutability));
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

        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Slice(elem_ty).into(),
            mutability,
        })
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
