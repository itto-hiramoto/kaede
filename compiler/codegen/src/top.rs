use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use inkwell::{module::Linkage, types::StructType, values::FunctionValue};
use kaede_ast::top::{
    Enum, EnumVariant, Extern, Fn, GenericFnInstance, GenericParams, Impl, Param, Params, Path,
    Struct, StructField, TopLevel, TopLevelKind, Use, Visibility,
};
use kaede_common::kaede_lib_src_dir;
use kaede_parse::Parser;
use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};
use kaede_ast_type::{change_mutability_dup, GenericArgs, Mutability, Ty, TyKind, UserDefinedType};

use crate::{
    error::CodegenError,
    mangle::{mangle_method, mangle_name, mangle_static_method, mangle_udt_name},
    stmt::build_block,
    tcx::{
        EnumInfo, EnumVariantInfo, GenericArgTable, GenericEnumInfo, GenericImplInfo, GenericInfo,
        GenericKind, GenericStructInfo, ReturnType, StructInfo, SymbolTable, SymbolTableValue,
    },
    CompileUnitCtx,
};

type FnParams = Vec<(Ident, Rc<Ty>)>;

type FnValueParamsPair<'ctx> = (FunctionValue<'ctx>, FnParams);

pub fn create_struct_type<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    mangled_name: Symbol,
    fields: &[StructField],
) -> anyhow::Result<StructType<'ctx>> {
    let mut field_tys = Vec::new();
    for field in fields.iter() {
        field_tys.push(cucx.conv_to_llvm_type(&field.ty, field.name.span())?);
    }

    let ty = cucx.context().opaque_struct_type(mangled_name.as_str());

    ty.set_body(&field_tys, true);

    Ok(ty)
}

/// Return None if type is not specified for all (like C's enum)
/// The size is returned in bits
fn get_largest_type_size_of_enum(
    cucx: &mut CompileUnitCtx,
    enum_items: &[EnumVariant],
) -> Option<u64> {
    let mut largest = 0;

    for item in enum_items.iter() {
        if let Some(ty) = &item.ty {
            let llvm_ty = cucx.conv_to_llvm_type(ty, item.name.span()).unwrap();
            let size = cucx.get_size_in_bits(&llvm_ty);
            largest = std::cmp::max(size, largest);
        }
    }

    match largest {
        0 => None,
        _ => Some(largest),
    }
}

pub fn create_enum_type<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    node: &Enum,
    mangled_name: Symbol,
    is_external: bool,
) -> (
    HashMap<Symbol, EnumVariantInfo>,
    StructType<'ctx>,
    Option<Vec<Ident>>, // External module names
) {
    let largest_type_size = get_largest_type_size_of_enum(cucx, &node.variants);

    let variants = node
        .variants
        .iter()
        .map(|e| {
            (
                e.name.symbol(),
                EnumVariantInfo {
                    name: e.name,
                    _vis: e.vis,
                    offset: e.offset,
                    ty: e.ty.clone().map(Rc::new),
                },
            )
        })
        .collect();

    let is_external = if is_external {
        Some(cucx.modules_for_mangle.get())
    } else {
        None
    };

    // If there is an item with a specified type
    // Specified: { i32, [i8; LARGEST_TYPE_SIZE_IN_BYTES] }
    // Not specified: { i32 }
    let ty = match largest_type_size {
        Some(size) => {
            let ty = cucx.context().opaque_struct_type(mangled_name.as_str());

            ty.set_body(
                &[
                    cucx.context().i32_type().into(),
                    cucx.context()
                        .i8_type()
                        .array_type((size / 8) as u32)
                        .into(),
                ],
                true,
            );

            ty
        }

        None => {
            let ty = cucx.context().opaque_struct_type(mangled_name.as_str());

            ty.set_body(&[cucx.context().i32_type().into()], true);

            ty
        }
    };

    (variants, ty, is_external)
}

pub fn build_top_level(cucx: &mut CompileUnitCtx, node: TopLevel) -> anyhow::Result<()> {
    let mut builder = TopLevelBuilder::new(cucx);

    builder.build(node)?;

    Ok(())
}

pub fn build_top_level_with_generic_args(
    cucx: &mut CompileUnitCtx,
    node: TopLevel,
    generic_params: GenericParams,
    generic_args: GenericArgs,
) -> anyhow::Result<()> {
    let generic_arg_table = GenericArgTable::from((generic_params, generic_args));

    cucx.tcx.push_generic_arg_table(generic_arg_table);

    let mut builder = TopLevelBuilder { cucx };

    builder.build(node)?;

    cucx.tcx.pop_generic_arg_table();

    Ok(())
}

pub fn push_self_to_front(params: &mut Params, impl_for_ty: Rc<Ty>, mutability: Mutability) {
    params.v.push_front(Param {
        name: Ident::new("self".to_string().into(), params.span),
        ty: change_mutability_dup(impl_for_ty, mutability),
    });
}

struct TopLevelBuilder<'a, 'ctx> {
    cucx: &'a mut CompileUnitCtx<'ctx>,
}

impl<'a, 'ctx> TopLevelBuilder<'a, 'ctx> {
    fn new(cucx: &'a mut CompileUnitCtx<'ctx>) -> Self {
        Self { cucx }
    }

    /// Generate top-level code
    fn build(&mut self, tl: TopLevel) -> anyhow::Result<()> {
        match tl.kind {
            TopLevelKind::Import(node) => self.import_module(node.module_path)?,

            TopLevelKind::Fn(node) => self.func(node, tl.vis)?,

            TopLevelKind::Struct(node) => self.struct_(node)?,

            TopLevelKind::Impl(node) => self.impl_(node, tl.vis, tl.span)?,

            TopLevelKind::Enum(node) => self.enum_(node)?,

            TopLevelKind::Extern(node) => self.extern_(node)?,

            TopLevelKind::GenericFnInstance(node) => self.generic_fn_instance(node)?,

            TopLevelKind::ExternalImpl(node) => {
                self.external_impl(node.impl_, node.external_modules)?
            }

            TopLevelKind::Use(node) => self.use_(node)?,
        }

        Ok(())
    }

    fn use_(&mut self, node: Use) -> anyhow::Result<()> {
        let modules = node.path.segments[..node.path.segments.len() - 1].to_vec();
        let name = node.path.segments.last().unwrap().symbol();

        let bkup = self.cucx.modules_for_mangle.change_for_external(modules);
        // Create actual mangled name.
        let actual_name = mangle_name(self.cucx, name).into();
        self.cucx.modules_for_mangle.change_for_internal(bkup);

        // Mangle for current module.
        let new_name = mangle_name(self.cucx, name).into();

        if actual_name == new_name {
            // No need to bind.
            // This happens when the file name is the same in another hierarchy.
            // For example, `foo.kd` and `bar/foo.kd`.
            return Ok(());
        }

        let bindee = self.cucx.tcx.lookup_symbol(actual_name, node.span)?;
        self.cucx
            .tcx
            .bind_symbol_to_new_name(new_name, bindee, node.span)?;

        Ok(())
    }

    fn generic_fn_instance(&mut self, node: GenericFnInstance) -> anyhow::Result<()> {
        let span = node.fn_.span;
        self.build_fn(node.mangled_name, node.fn_, span)
    }

    fn extern_(&mut self, node: Extern) -> anyhow::Result<()> {
        if node.fn_decl.self_.is_some() {
            todo!("error")
        }

        match node.lang_linkage {
            Some(lang_linkage) => match lang_linkage.syb.as_str() {
                "C" => {
                    let is_var_args = node.fn_decl.params.is_var_args;

                    self.declare_fn(
                        node.fn_decl.name.symbol(),
                        node.fn_decl.params,
                        node.fn_decl.return_ty.into(),
                        Linkage::External,
                        is_var_args,
                        node.span,
                    )
                    .unwrap();
                }

                _ => {
                    return Err(CodegenError::UnsupportedLanguageLinkage {
                        span: lang_linkage.span,
                        lang_linkage: lang_linkage.syb,
                    }
                    .into());
                }
            },

            _ => unimplemented!(),
        };

        Ok(())
    }

    fn enum_(&mut self, node: Enum) -> anyhow::Result<()> {
        self.create_enum_type(node, false)
    }

    fn create_enum_type(&mut self, node: Enum, is_external: bool) -> anyhow::Result<()> {
        let mangled_name = Symbol::from(mangle_name(self.cucx, node.name.symbol()));
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            self.cucx.tcx.insert_symbol_to_root_scope(
                mangled_name,
                SymbolTableValue::Generic(GenericInfo {
                    kind: GenericKind::Enum(GenericEnumInfo::new(node)),
                    is_external: if is_external {
                        Some(self.cucx.modules_for_mangle.get())
                    } else {
                        None
                    },
                }),
                span,
            )?;

            // Generics are not created immediately, but are created when they are used.
            return Ok(());
        }

        let name = node.name.symbol();

        let (variants, ty, is_external) =
            create_enum_type(self.cucx, &node, mangled_name, is_external);

        self.cucx.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Enum(EnumInfo {
                mangled_name,
                name,
                ty,
                variants,
                is_external,
            }),
            span,
        )
    }

    fn external_impl(&mut self, node: Impl, external_modules: Vec<Ident>) -> anyhow::Result<()> {
        assert!(node.generic_params.is_none());

        let ty = Rc::new(node.ty);

        let bkup = self
            .cucx
            .modules_for_mangle
            .change_for_external(external_modules.clone());

        for item in node.items.iter() {
            if item.vis.is_private() {
                continue;
            }

            match &item.kind {
                TopLevelKind::Fn(fn_) => {
                    self.define_method(ty.clone(), fn_, Some(external_modules.clone()))?
                }
                _ => todo!("Error"),
            }
        }

        self.cucx.modules_for_mangle.change_for_internal(bkup);

        Ok(())
    }

    fn impl_(&mut self, node: Impl, vis: Visibility, span: Span) -> anyhow::Result<()> {
        if node.generic_params.is_some() {
            let base_ty = if let TyKind::Reference(rty) = node.ty.kind.as_ref() {
                rty.get_base_type()
            } else {
                todo!("Error")
            };

            if let TyKind::UserDefined(udt) = base_ty.kind.as_ref() {
                let mangled_name = mangle_udt_name(
                    self.cucx,
                    // Remove generic arguments.
                    &UserDefinedType {
                        name: udt.name,
                        generic_args: None,
                    },
                );

                let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, span)?;
                match *symbol_kind.borrow_mut() {
                    SymbolTableValue::Generic(ref mut generic_info) => match &mut generic_info.kind
                    {
                        GenericKind::Struct(info) => {
                            info.impl_info = Some(GenericImplInfo::new(node, vis, span));
                        }
                        GenericKind::Enum(info) => {
                            info.impl_info = Some(GenericImplInfo::new(node, vis, span));
                        }
                        _ => todo!("Error"),
                    },

                    _ => todo!("Error"),
                }

                // Generic impls are not created immediately, but are created when they are used.
                return Ok(());
            } else {
                todo!("Error")
            }
        }

        let ty = Rc::new(node.ty);

        for item in node.items.iter() {
            match &item.kind {
                TopLevelKind::Fn(fn_) => self.define_method(ty.clone(), fn_, None)?,
                _ => todo!("Error"),
            }
        }

        Ok(())
    }

    fn func(&mut self, node: Fn, vis: Visibility) -> anyhow::Result<()> {
        assert_eq!(node.decl.self_, None);

        let mangled_name = if node.decl.name.as_str() == "main" {
            // Suppress mangling of main function
            String::from("kdmain")
        } else {
            mangle_name(self.cucx, node.decl.name.symbol())
        }
        .into();

        // For generic
        if node.decl.generic_params.is_some() {
            let span = node.span;

            self.cucx.tcx.insert_symbol_to_root_scope(
                mangled_name,
                SymbolTableValue::Generic(GenericInfo {
                    kind: GenericKind::Func((node, vis)),
                    is_external: None,
                }),
                span,
            )?;

            // Generic functions are not generated immediately, but are generated when they are used.
            return Ok(());
        }

        let span = node.span;
        self.build_fn(mangled_name, node, span)
    }

    fn mangle_method(&mut self, impl_for_ty: Rc<Ty>, node: &Fn) -> String {
        let base_ty = match impl_for_ty.kind.as_ref() {
            TyKind::Reference(refty) => refty.get_base_type(),
            _ => impl_for_ty.clone(),
        };

        if impl_for_ty.is_udt() {
            let udt = match base_ty.kind.as_ref() {
                TyKind::UserDefined(udt) => udt,
                _ => unreachable!(),
            };

            if node.decl.self_.is_none() {
                return mangle_static_method(self.cucx, udt, node.decl.name.symbol());
            };

            mangle_method(self.cucx, udt, node.decl.name.symbol())
        } else {
            // Builtin types

            if node.decl.self_.is_none() {
                // Static method
                // Without module name
                return format!("{}::{}", base_ty.kind, node.decl.name.symbol().as_str());
            };

            // Without module name
            format!("{}.{}", base_ty.kind, node.decl.name.symbol().as_str())
        }
    }

    /// Static method can also be handled by this function
    ///
    /// If kind is `Normal`, it becomes a static method (said in C++ style)
    fn define_method(
        &mut self,
        impl_for_ty: Rc<Ty>,
        node: &Fn,
        is_external: Option<Vec<Ident> /* Module names */>,
    ) -> anyhow::Result<()> {
        let mangled_name = self.mangle_method(impl_for_ty.clone(), node);
        self.define_method_internal(mangled_name.into(), impl_for_ty, node.clone(), is_external)
    }

    fn define_method_internal(
        &mut self,
        mangled_name: Symbol,
        impl_for_ty: Rc<Ty>,
        mut node: Fn,
        is_external: Option<Vec<Ident> /* Module names */>,
    ) -> anyhow::Result<()> {
        let span = node.span;

        // Preprocess external function (Parameter and return type wrapping)
        if let Some(externals) = is_external {
            self.preprocess_for_external_fn(&mut node, externals.clone())?;
        }

        match node.decl.self_ {
            Some(mutability) => {
                // Method
                push_self_to_front(&mut node.decl.params, impl_for_ty, mutability);
                self.build_fn(mangled_name, node, span)?;
            }

            None => {
                // Static method
                self.build_fn(mangled_name, node, span)?;
            }
        }

        Ok(())
    }

    fn preprocess_for_external_fn(
        &mut self,
        node: &mut Fn,
        external_modules: Vec<Ident>,
    ) -> anyhow::Result<()> {
        // For return type.
        let return_ty = if node.decl.return_ty.is_some() {
            let ty = Rc::new(Ty {
                kind: node.decl.return_ty.as_ref().unwrap().kind.clone(),
                mutability: node.decl.return_ty.as_ref().unwrap().mutability,
            });

            // Wrapping with external type.
            Some(if ty.is_udt() {
                let mut wrapped = ty.clone();
                for module in external_modules.iter().rev() {
                    wrapped = Ty::new_external(*module, wrapped).into();
                }
                wrapped
            } else {
                // Builtin types
                ty
            })
        } else {
            None
        };

        node.decl.return_ty = return_ty;

        // For parameters.
        for param in node.decl.params.v.iter_mut() {
            let ty = Rc::new(Ty {
                kind: param.ty.kind.clone(),
                mutability: param.ty.mutability,
            });

            // Wrapping with external type.
            let wrapped = if ty.is_udt() {
                let mut wrapped = ty.clone();
                for module in external_modules.iter().rev() {
                    wrapped = Ty::new_external(*module, wrapped).into();
                }
                wrapped
            } else {
                // Builtin types
                ty
            };

            param.ty = wrapped;
        }

        Ok(())
    }

    fn declare_method(
        &mut self,
        mangled_name: Symbol,
        impl_for_ty: Rc<Ty>,
        mut node: Fn,
        is_external: Option<Vec<Ident> /* Module names */>,
    ) -> anyhow::Result<()> {
        if let Some(externals) = is_external {
            self.preprocess_for_external_fn(&mut node, externals.clone())?;
        }

        match node.decl.self_ {
            Some(mutability) => {
                // Method
                push_self_to_front(&mut node.decl.params, impl_for_ty, mutability);
                self.declare_fn(
                    mangled_name,
                    node.decl.params,
                    node.decl.return_ty.into(),
                    Linkage::External,
                    false,
                    node.span,
                )?;
            }

            None => {
                // Static method
                self.declare_fn(
                    mangled_name,
                    node.decl.params,
                    node.decl.return_ty.into(),
                    Linkage::External,
                    false,
                    node.span,
                )?;
            }
        }

        Ok(())
    }

    // Return function value and parameter information reflecting mutability for the types
    fn declare_fn(
        &mut self,
        mangled_name: Symbol,
        params: Params,
        return_ty: ReturnType,
        linkage: Linkage,
        is_var_args: bool,
        span: Span,
    ) -> anyhow::Result<FnValueParamsPair<'ctx>> {
        let params = params
            .v
            .into_iter()
            .map(|e| (e.name, e.ty))
            .collect::<Vec<_>>();

        let fn_value = self.cucx.declare_fn(
            mangled_name,
            params.iter().map(|e| e.1.clone()).collect(),
            return_ty,
            Some(linkage),
            is_var_args,
            span,
        )?;

        Ok((fn_value, params))
    }

    fn build_fn(&mut self, mangled_name: Symbol, node: Fn, span: Span) -> anyhow::Result<()> {
        let (fn_value, params) = self.declare_fn(
            mangled_name,
            node.decl.params,
            node.decl.return_ty.into(),
            Linkage::External,
            false,
            span,
        )?;

        let basic_block = self.cucx.context().append_basic_block(fn_value, "entry");
        self.cucx.builder.position_at_end(basic_block);

        // Allocate parameters
        let param_table = self.fn_params_to_symbol_table(fn_value, params)?;

        // Push parameter table
        self.cucx.tcx.push_symbol_table(param_table);

        build_block(self.cucx, &node.body)?;

        self.cucx.tcx.pop_symbol_table();

        if fn_value.get_type().get_return_type().is_none() && self.cucx.no_terminator() {
            // If return type is void and there is no termination, insert return
            self.cucx.builder.build_return(None)?;
        }

        Ok(())
    }

    fn fn_params_to_symbol_table(
        &mut self,
        fn_value: FunctionValue<'ctx>,
        params: FnParams,
    ) -> anyhow::Result<SymbolTable<'ctx>> {
        let mut table = SymbolTable::new();

        assert_eq!(fn_value.count_params(), params.len() as u32);

        for (idx, (name, param_ty)) in params.into_iter().enumerate() {
            let llvm_param_ty = self.cucx.conv_to_llvm_type(&param_ty, name.span())?;
            let alloca = self
                .cucx
                .builder
                .build_alloca(llvm_param_ty, name.as_str())?;

            self.cucx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap())?;

            table.insert(
                name.symbol(),
                Rc::new(RefCell::new(SymbolTableValue::Variable((alloca, param_ty)))),
            );
        }

        Ok(table)
    }

    fn struct_(&mut self, node: Struct) -> anyhow::Result<()> {
        let mangled_name = mangle_name(self.cucx, node.name.symbol()).into();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            self.cucx.tcx.insert_symbol_to_root_scope(
                mangled_name,
                SymbolTableValue::Generic(GenericInfo {
                    kind: GenericKind::Struct(GenericStructInfo::new(node)),
                    is_external: None,
                }),
                span,
            )?;

            // Generic structs are not created immediately, but are created when they are used.
            return Ok(());
        }

        let ty = create_struct_type(self.cucx, mangled_name, &node.fields)?;

        let fields = node
            .fields
            .into_iter()
            .map(|field| (field.name.symbol(), field))
            .collect();

        self.cucx.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Struct(StructInfo {
                mangled_name,
                ty,
                fields,
                is_external: None,
            }),
            node.span,
        )?;

        Ok(())
    }

    fn import_module(&mut self, module_path: Path) -> anyhow::Result<()> {
        let path_prefix = if module_path.segments.first().unwrap().as_str() == "std" {
            // Standard library
            kaede_lib_src_dir()
        } else {
            self.cucx.file_path.path().parent().unwrap().to_path_buf()
        };

        let mut path = path_prefix;

        for (idx, segment) in module_path.segments.iter().enumerate() {
            if idx == module_path.segments.len() - 1 {
                path = path.join(segment.as_str()).with_extension("kd");
                break;
            }

            path = path.join(segment.as_str());
        }

        if !path.exists() {
            return Err(CodegenError::FileNotFoundForModule {
                span: module_path.span,
                mod_name: module_path.segments.last().unwrap().symbol(),
            }
            .into());
        }

        // Prevent duplicate imports
        if self.cucx.imported_module_paths.contains(&path) {
            return Ok(());
        } else {
            self.cucx.imported_module_paths.insert(path.canonicalize()?);
        }

        // TODO: Optimize
        let psd_module = Parser::new(&fs::read_to_string(&path).unwrap(), path.clone().into())
            .run()
            .unwrap();

        // For mangle.
        let file_bkup = self.cucx.file_path;
        self.cucx.file_path = FilePath::from(path.to_path_buf());
        let bkup = self
            .cucx
            .modules_for_mangle
            .change_for_external(vec![*module_path.segments.last().unwrap()]);

        for top_level in psd_module.top_levels {
            // Without checking visibility.
            match top_level.kind {
                TopLevelKind::Impl(impl_) => {
                    self.import_impl(
                        impl_,
                        module_path.segments.clone(),
                        top_level.vis,
                        top_level.span,
                    )?;
                    continue;
                }

                TopLevelKind::Import(import_) => {
                    self.import_module(import_.module_path)?;
                    continue;
                }

                TopLevelKind::Use(use_) => {
                    self.use_(use_)?;
                    continue;
                }

                TopLevelKind::Extern(_) => todo!(),

                _ => {}
            }

            // Check visibility.
            if top_level.vis.is_private() {
                continue;
            }

            match top_level.kind {
                TopLevelKind::Impl(_) => unreachable!(),

                TopLevelKind::Fn(mut func) => {
                    self.preprocess_for_external_fn(&mut func, module_path.segments.clone())?;

                    let mangled_name = mangle_name(self.cucx, func.decl.name.symbol());

                    self.declare_fn(
                        mangled_name.into(),
                        func.decl.params,
                        func.decl.return_ty.into(),
                        Linkage::External,
                        false,
                        func.span,
                    )?;
                }

                TopLevelKind::Struct(struct_) => {
                    self.import_struct(struct_)?;
                }

                TopLevelKind::Enum(enum_) => {
                    self.import_enum(enum_)?;
                }

                TopLevelKind::Use(_) => unreachable!(),
                TopLevelKind::Extern(_) => unreachable!(),
                TopLevelKind::Import(_) => unreachable!(),
                TopLevelKind::GenericFnInstance(_) => unreachable!(),
                TopLevelKind::ExternalImpl(_) => unreachable!(),
            };
        }

        self.cucx.file_path = file_bkup;

        let module_name = module_path.segments.last().unwrap().symbol();
        let module_path_s = module_path
            .segments
            .iter()
            .map(|i| i.as_str())
            .collect::<Vec<_>>()
            .join(".")
            .into();
        // Bind module name to the actual module path
        self.cucx.tcx.insert_symbol_to_root_scope(
            module_name,
            SymbolTableValue::Module(module_path_s),
            module_path.span,
        )?;

        self.cucx.modules_for_mangle.change_for_internal(bkup);

        self.cucx
            .imported_modules
            .insert(module_path.segments.last().unwrap().symbol());

        Ok(())
    }

    fn import_enum(&mut self, node: Enum) -> anyhow::Result<()> {
        self.create_enum_type(node, true)
    }

    fn import_struct(&mut self, struct_: Struct) -> anyhow::Result<()> {
        let span = struct_.span;

        let mangled_name = mangle_name(self.cucx, struct_.name.symbol()).into();

        if struct_.generic_params.is_some() {
            self.cucx.tcx.insert_symbol_to_root_scope(
                mangled_name,
                SymbolTableValue::Generic(GenericInfo {
                    kind: GenericKind::Struct(GenericStructInfo::new(struct_)),
                    is_external: Some(self.cucx.modules_for_mangle.get()),
                }),
                span,
            )?;

            // Generics are not created immediately, but are created when they are used.
            return Ok(());
        }

        let ty = create_struct_type(self.cucx, mangled_name, &struct_.fields)?;

        let fields = struct_
            .fields
            .into_iter()
            .map(|field| (field.name.symbol(), field))
            .collect();

        self.cucx.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Struct(StructInfo {
                mangled_name,
                ty,
                fields,
                is_external: Some(self.cucx.modules_for_mangle.get()),
            }),
            span,
        )?;

        Ok(())
    }

    fn import_impl(
        &mut self,
        impl_: Impl,
        external_modules: Vec<Ident>,
        vis: Visibility,
        span: Span,
    ) -> anyhow::Result<()> {
        if impl_.generic_params.is_some() {
            let base_ty = if let TyKind::Reference(rty) = impl_.ty.kind.as_ref() {
                rty.get_base_type()
            } else {
                todo!("Error")
            };

            if let TyKind::UserDefined(udt) = base_ty.kind.as_ref() {
                let mangled_name = mangle_udt_name(
                    self.cucx,
                    // Remove generic arguments.
                    &UserDefinedType {
                        name: udt.name,
                        generic_args: None,
                    },
                );

                let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, span);
                let symbol_kind = match symbol_kind {
                    Ok(symbol_kind) => symbol_kind,

                    Err(_) => {
                        // It could be that it was a impl of a non-public, user-defined type.
                        // If it's an error, don't make an error here, just make an error when compiling the file.
                        return Ok(());
                    }
                };

                match *symbol_kind.borrow_mut() {
                    SymbolTableValue::Generic(ref mut generic_info) => match &mut generic_info.kind
                    {
                        GenericKind::Struct(info) => {
                            info.impl_info = Some(GenericImplInfo::new(impl_, vis, span));
                        }
                        GenericKind::Enum(info) => {
                            info.impl_info = Some(GenericImplInfo::new(impl_, vis, span));
                        }
                        _ => todo!("Error"),
                    },

                    _ => todo!("Error"),
                }

                // Generic impls are not created immediately, but are created when they are used.
                return Ok(());
            } else {
                todo!("Error")
            }
        }

        let impl_for_ty = Rc::new(impl_.ty);

        for item in impl_.items.iter() {
            match &item.kind {
                TopLevelKind::Fn(func) => {
                    if item.vis.is_private() {
                        continue;
                    }

                    let mangled_name = self.mangle_method(impl_for_ty.clone(), func);

                    self.declare_method(
                        mangled_name.into(),
                        impl_for_ty.clone(),
                        func.clone(),
                        Some(external_modules.clone()),
                    )?;
                }

                _ => todo!(),
            }
        }

        Ok(())
    }
}
