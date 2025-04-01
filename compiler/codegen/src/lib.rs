use core::panic;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::{Component, PathBuf},
    rc::Rc,
};

use error::CodegenError;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use kaede_ast::{
    top::{
        Enum, ExternalImpl, Fn, GenericParams, Impl, Import, Param, Params, Path, Struct,
        StructField, TopLevel, TopLevelKind, Visibility,
    },
    CompileUnit,
};
use kaede_ast_type::{
    ExternalType, FundamentalType, FundamentalTypeKind, GenericArgs, Mutability, PointerType,
    ReferenceType, Ty, TyKind, UserDefinedType,
};
use kaede_common::kaede_autoload_dir;
use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};
use mangle::mangle_udt_name;
use tcx::{
    EnumInfo, EnumVariantInfo, FunctionInfo, GenericArgTable, GenericKind, ReturnType, StructInfo,
    SymbolTable, SymbolTableValue, TypeCtx,
};
use top::{
    build_top_level, build_top_level_with_generic_args, create_enum_type, create_struct_type,
};

pub mod error;
mod expr;
mod mangle;
mod stmt;
mod tcx;
mod top;
mod value;

#[cfg(test)]
mod tests;

/// Used when you want to get a pointer from the evaluated value after evaluating an expression
fn get_loaded_pointer<'ctx>(load_instr: &InstructionValue<'ctx>) -> Option<PointerValue<'ctx>> {
    if let Some(loaded_value) = load_instr.get_operand(0) {
        // Check if the loaded value is a pointer
        if let BasicValueEnum::PointerValue(pointer_value) = loaded_value.left().unwrap() {
            // Return the pointer value as an InstructionValue
            return Some(pointer_value);
        }
    }

    // If the loaded value is not a pointer, return None
    None
}

fn type_to_actual(cucx: &CompileUnitCtx, ty: Rc<Ty>, span: Span) -> anyhow::Result<Rc<Ty>> {
    match ty.kind.as_ref() {
        TyKind::UserDefined(udt) => {
            if udt.generic_args.is_none() {
                Ok(ty)
            } else {
                let mut generic_args = udt.generic_args.clone();
                for ga in generic_args.as_mut().unwrap().types.iter_mut() {
                    *ga = type_to_actual(cucx, ga.clone(), span)?;
                }
                Ok(Ty {
                    kind: TyKind::UserDefined(UserDefinedType {
                        generic_args,
                        ..udt.clone()
                    })
                    .into(),
                    mutability: ty.mutability,
                }
                .into())
            }
        }
        TyKind::Array((elem_ty, size)) => {
            let actual_elem_ty = type_to_actual(cucx, elem_ty.clone(), span)?;

            Ok(Ty {
                kind: TyKind::Array((actual_elem_ty, *size)).into(),
                mutability: ty.mutability,
            }
            .into())
        }
        TyKind::Tuple(types) => {
            let mut actual_types = Vec::new();
            for ty in types.iter() {
                actual_types.push(type_to_actual(cucx, ty.clone(), span)?);
            }

            Ok(Ty {
                kind: TyKind::Tuple(actual_types).into(),
                mutability: ty.mutability,
            }
            .into())
        }
        TyKind::Pointer(pty) => {
            let actual_pointee_type = type_to_actual(cucx, pty.pointee_ty.clone(), span)?;

            Ok(Ty {
                kind: TyKind::Pointer(PointerType {
                    pointee_ty: actual_pointee_type,
                })
                .into(),
                mutability: ty.mutability,
            }
            .into())
        }
        TyKind::Reference(rty) => {
            let actual_refee_ty = type_to_actual(cucx, rty.refee_ty.clone(), span)?;

            Ok(Ty {
                kind: TyKind::Reference(ReferenceType {
                    refee_ty: actual_refee_ty,
                })
                .into(),
                mutability: ty.mutability,
            }
            .into())
        }
        TyKind::Generic(gty) => {
            if let Some(actual_ty) = cucx.tcx.lookup_generic_arg(gty.name.symbol()) {
                Ok(actual_ty)
            } else {
                Err(CodegenError::Undeclared {
                    name: gty.name.symbol(),
                    span,
                }
                .into())
            }
        }
        TyKind::External(ety) => {
            let actual = type_to_actual(cucx, ety.ty.clone(), span)?;
            Ok(Ty {
                kind: TyKind::External(ExternalType {
                    ty: actual,
                    module_name: ety.module_name,
                })
                .into(),
                mutability: ty.mutability,
            }
            .into())
        }
        TyKind::Fundamental(_) => Ok(ty),
        TyKind::Inferred | TyKind::Unit | TyKind::Never => unreachable!(),
    }
}

fn generic_types_to_actual_in_struct(
    cucx: &CompileUnitCtx,
    fields: &[StructField],
) -> Vec<StructField> {
    let mut actual = Vec::new();

    for field in fields.iter() {
        actual.push(StructField {
            ty: type_to_actual(cucx, field.ty.clone(), field.name.span()).unwrap(),
            ..field.clone()
        });
    }

    actual
}

fn generic_types_to_actual_in_enum(
    cucx: &CompileUnitCtx,
    variants: &HashMap<Symbol, EnumVariantInfo>,
) -> HashMap<Symbol, EnumVariantInfo> {
    let mut actual = HashMap::new();

    for variant in variants.iter() {
        let variant_ty = match variant.1.ty.as_ref() {
            Some(ty) => ty.clone(),
            None => {
                actual.insert(*variant.0, variant.1.clone());
                continue;
            }
        };

        actual.insert(
            *variant.0,
            EnumVariantInfo {
                ty: Some(type_to_actual(cucx, variant_ty, variant.1.name.span()).unwrap()),
                ..*variant.1
            },
        );
    }

    actual
}

pub fn codegen_compile_unit<'ctx>(
    cgcx: &'ctx CodegenCtx<'ctx>,
    file_path: FilePath,
    root_dir: &'ctx Option<PathBuf>,
    cu: CompileUnit,
    no_autoload: bool,
) -> anyhow::Result<Module<'ctx>> {
    let cucx = CompileUnitCtx::new(cgcx, file_path, root_dir)?;

    cucx.codegen(cu.top_levels, no_autoload)
}

/// Do **not** create this struct multiple times!
pub struct CodegenCtx<'ctx> {
    _target_machine: TargetMachine,
    target_data: TargetData,

    context: &'ctx Context,
}

impl<'ctx> CodegenCtx<'ctx> {
    fn create_target_machine() -> anyhow::Result<TargetMachine> {
        let triple = TargetMachine::get_default_triple();

        let target = match Target::from_triple(&triple) {
            Ok(tgt) => tgt,
            Err(what) => {
                return Err(CodegenError::FailedToLookupTarget {
                    triple: triple.as_str().to_str().unwrap().to_string(),
                    what: what.to_string(),
                }
                .into())
            }
        };

        match target.create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        ) {
            Some(m) => Ok(m),
            None => Err(CodegenError::FailedToCreateTargetMachine.into()),
        }
    }

    pub fn new(context: &'ctx Context) -> anyhow::Result<Self> {
        // Without initialization, target creation will always fail
        Target::initialize_all(&InitializationConfig::default());

        let machine = Self::create_target_machine()?;

        Ok(Self {
            target_data: machine.get_target_data(),
            _target_machine: machine,
            context,
        })
    }
}

#[derive(Clone)]
struct ModulesForMangle {
    names: Vec<Ident>,
    is_external: bool,
}

impl ModulesForMangle {
    fn new(names: &[Ident], is_external: bool) -> Self {
        Self {
            names: names.to_vec(),
            is_external,
        }
    }

    fn get(&self) -> Vec<Ident> {
        self.names.clone()
    }

    fn create_mangle_prefix(&self) -> String {
        self.names
            .iter()
            .map(|name| name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }

    // Returns a backup.
    fn change_for_external(&mut self, mut v: Vec<Ident>) -> Self {
        let backup = self.clone();

        self.is_external = true;
        self.names.clear();
        self.names.append(&mut v);

        backup
    }

    fn change_for_internal(&mut self, backup: Self) {
        self.is_external = false;

        *self = backup;
    }
}

pub struct CompileUnitCtx<'ctx> {
    cgcx: &'ctx CodegenCtx<'ctx>,
    tcx: TypeCtx<'ctx>,

    module: Module<'ctx>,
    builder: Builder<'ctx>,

    file_path: FilePath,

    imported_modules: HashSet<Symbol>,

    root_dir: &'ctx Option<PathBuf>,

    modules_for_mangle: ModulesForMangle,

    /// Block to jump to when a `break` is executed
    ///
    /// Empty if **not** in a loop
    ///
    /// Each time a loop is nested, a basicblock is pushed
    loop_break_bb_stk: Vec<BasicBlock<'ctx>>,

    is_ifmatch_stmt: bool,

    // The elements of this array will begin to be generated sequentially after the module's code generation is finished.
    lazy_defines: Vec<(TopLevel, GenericParams, GenericArgs)>,

    // The paths of the modules that have been imported.
    imported_module_paths: HashSet<PathBuf>,
}

impl<'ctx> CompileUnitCtx<'ctx> {
    pub fn new(
        cgcx: &'ctx CodegenCtx<'ctx>,
        file_path: FilePath,
        root_dir: &'ctx Option<PathBuf>,
    ) -> anyhow::Result<Self> {
        if let Some(root_dir) = &root_dir {
            if !root_dir.is_dir() {
                panic!("Root directory is not a directory");
            }
        }

        let module_name = file_path
            .path()
            .file_stem()
            .unwrap()
            .to_owned()
            .into_string()
            .unwrap();

        let module = cgcx.context.create_module(&module_name);
        module.set_source_file_name(&file_path.path().to_string_lossy());

        let mut tcx = TypeCtx::default();
        tcx.push_symbol_table(SymbolTable::new()); // Top-level scope

        Ok(Self {
            cgcx,
            tcx,
            module,
            builder: cgcx.context.create_builder(),
            file_path,
            root_dir,
            modules_for_mangle: ModulesForMangle::new(
                &[Ident::new(module_name.into(), Span::dummy())],
                false,
            ),
            imported_modules: HashSet::new(),
            loop_break_bb_stk: Vec::new(),
            is_ifmatch_stmt: false,
            lazy_defines: Vec::new(),
            imported_module_paths: HashSet::new(),
        })
    }

    fn context(&self) -> &'ctx Context {
        self.cgcx.context
    }

    pub fn get_size_in_bits(&self, type_: &dyn AnyType) -> u64 {
        self.cgcx.target_data.get_bit_size(type_)
    }

    /// Canonicalize module name (m -> dir1.dir2.m)
    fn canonicalize_module(&self, module_chain: Symbol) -> Symbol {
        let symbol_kind = self.tcx.lookup_symbol(module_chain, Span::dummy());
        if let Ok(symbol_kind) = symbol_kind {
            if let SymbolTableValue::Module(resolved) = &*symbol_kind.borrow() {
                return *resolved;
            }
        }

        module_chain
    }

    pub fn mangle_prefix(&self) -> anyhow::Result<Symbol> {
        let diff_from_root = if let Some(root_dir) = self.root_dir {
            // Get the canonical paths.
            let root_dir = root_dir.canonicalize()?;
            let file_parent = self.file_path.path().parent().unwrap().canonicalize()?;

            file_parent
                .strip_prefix(root_dir)?
                .components()
                .filter_map(|c| {
                    if let Component::Normal(os_str) = c {
                        os_str.to_str()
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .join(".")
        } else {
            "".to_owned()
        };

        let module = Symbol::from(self.modules_for_mangle.create_mangle_prefix());

        // The following process is for the following case.
        //
        // Define `Apple` in `m.kd` and import `dir/m.kd`.
        // Then, if `Apple`` is also defined in `dir/m.kd`, an error occurs.
        let module = if module.as_str()
            == self.file_path.path().file_stem().unwrap().to_string_lossy()
            && !self.modules_for_mangle.is_external
        {
            module
        } else {
            self.canonicalize_module(module)
        };

        Ok(if diff_from_root.is_empty() {
            module
        } else {
            format!("{}.{}", diff_from_root, module.as_str()).into()
        })
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(
        &mut self,
        name: &str,
        ty: &Ty,
        span: Span,
    ) -> anyhow::Result<PointerValue<'ctx>> {
        let builder = self.context().create_builder();

        let entry = self.get_current_fn().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        Ok(builder.build_alloca(self.conv_to_llvm_type(ty, span)?, name)?)
    }

    // If return_ty is `None`, treat as void
    fn create_fn_type(
        &mut self,
        params: &[Rc<Ty>],
        return_ty: &ReturnType,
        is_var_args: bool,
        span: Span,
    ) -> anyhow::Result<FunctionType<'ctx>> {
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.conv_to_llvm_type(param, span)?.into());
        }

        Ok(match return_ty {
            ReturnType::Type(ty) => self
                .conv_to_llvm_type(ty, span)?
                .fn_type(param_types.as_slice(), is_var_args),

            ReturnType::Void => self
                .context()
                .void_type()
                .fn_type(param_types.as_slice(), is_var_args),
        })
    }

    fn declare_fn(
        &mut self,
        mangled_name: Symbol,
        param_types: Vec<Rc<Ty>>,
        return_type: ReturnType,
        linkage: Option<Linkage>,
        is_var_args: bool,
        span: Span,
    ) -> anyhow::Result<FunctionValue<'ctx>> {
        if let Ok(kind) = self.tcx.lookup_symbol(mangled_name, span) {
            if let SymbolTableValue::Function(fn_) = &*kind.borrow() {
                // If the function is already declared, don't declare it again.
                return Ok(fn_.value);
            }
        }

        let fn_type = self.create_fn_type(&param_types, &return_type, is_var_args, span)?;

        let fn_value = self
            .module
            .add_function(mangled_name.as_str(), fn_type, linkage);

        // Store function information in table
        self.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Function(FunctionInfo {
                value: fn_value,
                return_type,
                param_types,
            }),
            span,
        )?;

        Ok(fn_value)
    }

    fn gc_init(&mut self) -> anyhow::Result<()> {
        // Declare GC_malloc in boehm-gc
        let return_ty = Ty {
            kind: TyKind::Reference(ReferenceType {
                refee_ty: Ty {
                    kind: TyKind::Fundamental(FundamentalType {
                        kind: FundamentalTypeKind::I8,
                    })
                    .into(),
                    mutability: Mutability::Mut,
                }
                .into(),
            })
            .into(),
            mutability: Mutability::Mut,
        }
        .into();

        let param_types = vec![Ty {
            kind: TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::U64,
            })
            .into(),
            mutability: Mutability::Not,
        }
        .into()];

        self.declare_fn(
            Symbol::from("GC_malloc".to_owned()),
            param_types,
            ReturnType::Type(return_ty),
            None,
            false,
            Span::dummy(),
        )
        .map(|_| ())
    }

    fn gc_malloc(&mut self, ty: BasicTypeEnum<'ctx>) -> anyhow::Result<PointerValue<'ctx>> {
        let gc_mallocd = self.module.get_function("GC_malloc").unwrap();

        let size = ty.size_of().unwrap().into();

        let addr = self
            .builder
            .build_call(gc_mallocd, &[size], "")?
            .try_as_basic_value()
            .left()
            .unwrap();

        Ok(addr.into_pointer_value())
    }

    fn get_current_fn(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    /// `True` if there is **not** a terminator in the current block
    fn no_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    /// If already created, this function is not created anew
    fn create_generic_struct_type(
        &mut self,
        mangled_name: Symbol,
        ast: &Struct,
        generic_args: &GenericArgs,
        is_external: Option<Vec<Ident>>,
    ) -> anyhow::Result<StructType<'ctx>> {
        // Check if already created.
        if let Ok(kind) = self.tcx.lookup_symbol(mangled_name, ast.span) {
            match &*kind.borrow() {
                SymbolTableValue::Struct(info) => return Ok(info.ty),
                _ => unreachable!(),
            }
        }

        self.tcx.push_generic_arg_table(GenericArgTable::from((
            ast.generic_params.as_ref().unwrap().clone(),
            generic_args.clone(),
        )));

        let ty = create_struct_type(self, mangled_name, &ast.fields)?;
        // To resolve generics types when using this struct.
        let actual_fields = generic_types_to_actual_in_struct(self, &ast.fields);

        self.tcx.pop_generic_arg_table();

        let fields = actual_fields
            .iter()
            .map(|field| (field.name.symbol(), field.clone()))
            .collect();

        self.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Struct(StructInfo {
                mangled_name,
                ty,
                fields,
                is_external,
            }),
            ast.span,
        )?;

        Ok(ty)
    }

    /// If already created, this function is not created anew
    fn create_generic_enum_type(
        &mut self,
        mangled_name: Symbol,
        ast: &Enum,
        generic_args: &GenericArgs,
        is_external: Option<Vec<Ident>>,
    ) -> anyhow::Result<StructType<'ctx>> {
        // Check if already created.
        if let Ok(kind) = self.tcx.lookup_symbol(mangled_name, ast.span) {
            match &*kind.borrow() {
                SymbolTableValue::Enum(info) => return Ok(info.ty),
                _ => unreachable!(),
            }
        }

        self.tcx.push_generic_arg_table(GenericArgTable::from((
            ast.generic_params.as_ref().unwrap().clone(),
            generic_args.clone(),
        )));

        let (variants, ty, _) = create_enum_type(self, ast, mangled_name, false);
        // To resolve generics types when using this struct.
        let variants = generic_types_to_actual_in_enum(self, &variants);

        self.tcx.pop_generic_arg_table();

        self.tcx.insert_symbol_to_root_scope(
            mangled_name,
            SymbolTableValue::Enum(EnumInfo {
                mangled_name,
                name: ast.name.symbol(),
                ty,
                variants,
                is_external,
            }),
            ast.span,
        )?;

        Ok(ty)
    }

    fn generic_args_to_actual(&self, generic_args: &GenericArgs) -> anyhow::Result<GenericArgs> {
        let mut actual_types = Vec::new();

        for ty in generic_args.types.iter() {
            actual_types.push(type_to_actual(self, ty.clone(), generic_args.span)?);
        }

        assert_eq!(actual_types.len(), generic_args.types.len());

        Ok(GenericArgs {
            types: actual_types,
            span: generic_args.span,
        })
    }

    fn fn_types_to_actual(&self, fn_: &mut Fn) -> anyhow::Result<()> {
        let mut actual_params = VecDeque::new();

        for param in fn_.decl.params.v.iter() {
            actual_params.push_back(Param {
                ty: type_to_actual(self, param.ty.clone(), fn_.span)?,
                ..*param
            });
        }

        let actual_return_ty = match &fn_.decl.return_ty {
            Some(ty) => Some(type_to_actual(self, ty.clone(), fn_.span)?),
            None => None,
        };

        fn_.decl.params = Params {
            v: actual_params,
            ..fn_.decl.params
        };

        if let Some(ty) = actual_return_ty {
            fn_.decl.return_ty = Some(ty);
        }

        Ok(())
    }

    fn impl_types_to_actual(&self, impl_: &Impl) -> anyhow::Result<Impl> {
        let mut actual_items = Vec::new();

        for item in impl_.items.iter() {
            match &item.kind {
                TopLevelKind::Fn(fn_) => {
                    let mut fn_ = fn_.clone();
                    self.fn_types_to_actual(&mut fn_)?;
                    actual_items.push(TopLevel {
                        kind: TopLevelKind::Fn(fn_),
                        ..*item
                    });
                }
                _ => todo!(),
            }
        }

        Ok(Impl {
            items: actual_items.into(),
            ..impl_.clone()
        })
    }

    fn create_generic_type(&mut self, udt: &UserDefinedType) -> anyhow::Result<StructType<'ctx>> {
        let mangled_name = mangle_udt_name(
            self,
            &UserDefinedType {
                name: udt.name,
                generic_args: None, // Generic table's key doesn't include generic arg types.
            },
        );

        let generic_args = udt.generic_args.as_ref().unwrap();

        let symbol_kind = self.tcx.lookup_symbol(mangled_name, udt.name.span())?;

        let (impl_info, is_external) = {
            let borrowed_symbol_kind = symbol_kind.borrow();
            let generic_info = match &*borrowed_symbol_kind {
                SymbolTableValue::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            let generic_kind = &generic_info.kind;

            (
                match generic_kind {
                    GenericKind::Struct(info) => info.impl_info.clone(),
                    GenericKind::Enum(info) => info.impl_info.clone(),
                    _ => unimplemented!(),
                },
                generic_info.is_external.clone(),
            )
        };

        // Define methods
        if let Some(impl_info) = impl_info {
            let impl_ = impl_info.impl_.clone();
            let vis = impl_info.visibility;
            let span = impl_info.span;

            let mut new_impl = impl_.clone();
            // If you don't remove this, it will only be registered in the generic table, not defined.
            new_impl.generic_params = None;

            self.tcx.push_generic_arg_table(GenericArgTable::from((
                impl_.generic_params.as_ref().unwrap().clone(),
                generic_args.clone(),
            )));

            let generic_args_with_actual_types = self.generic_args_to_actual(generic_args)?;

            // To avoid infinite loops, check if already defined.
            if !impl_info
                .already_generated
                .contains(&generic_args_with_actual_types)
            {
                // Register as already generated.
                match *symbol_kind.borrow_mut() {
                    SymbolTableValue::Generic(ref mut generic_info) => match &mut generic_info.kind
                    {
                        GenericKind::Struct(info) => info
                            .impl_info
                            .as_mut()
                            .unwrap()
                            .already_generated
                            .insert(generic_args_with_actual_types.clone()),
                        GenericKind::Enum(info) => info
                            .impl_info
                            .as_mut()
                            .unwrap()
                            .already_generated
                            .insert(generic_args_with_actual_types.clone()),

                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                };

                new_impl = self.impl_types_to_actual(&new_impl)?;

                let bb_backup = self.builder.get_insert_block().unwrap();

                build_top_level_with_generic_args(
                    self,
                    if let Some(externals) = &is_external {
                        // External
                        TopLevel {
                            kind: TopLevelKind::ExternalImpl(ExternalImpl {
                                impl_: new_impl.clone(),
                                external_modules: externals.clone(),
                            }),
                            visibility: vis,
                            span,
                        }
                    } else {
                        // Internal
                        TopLevel {
                            kind: TopLevelKind::Impl(new_impl.clone()),
                            visibility: vis,
                            span,
                        }
                    },
                    impl_.generic_params.clone().unwrap(),
                    generic_args_with_actual_types,
                )?;

                self.builder.position_at_end(bb_backup);
            }

            self.tcx.pop_generic_arg_table();
        }

        let mangled_generic_name = mangle_udt_name(self, udt);

        let borrowed_symbol_kind = symbol_kind.borrow();
        let generic_kind = match &*borrowed_symbol_kind {
            SymbolTableValue::Generic(generic_info) => &generic_info.kind,
            _ => unreachable!(),
        };

        // Create an actual type from a generic type.
        match generic_kind {
            GenericKind::Struct(info) => self.create_generic_struct_type(
                mangled_generic_name,
                &info.ast,
                generic_args,
                is_external.clone(),
            ),
            GenericKind::Enum(info) => self.create_generic_enum_type(
                mangled_generic_name,
                &info.ast,
                generic_args,
                is_external.clone(),
            ),
            _ => unimplemented!(),
        }
    }

    fn conv_to_llvm_type(&mut self, ty: &Ty, span: Span) -> anyhow::Result<BasicTypeEnum<'ctx>> {
        let context = self.context();

        Ok(match ty.kind.as_ref() {
            TyKind::External(ety) => {
                let bkup = self
                    .modules_for_mangle
                    .change_for_external(vec![ety.module_name]);

                let ty = self.conv_to_llvm_type(&ety.ty, span)?;

                self.modules_for_mangle.change_for_internal(bkup);

                ty
            }

            TyKind::Fundamental(t) => t.as_llvm_type(self.context()),

            TyKind::UserDefined(udt) => {
                if udt.generic_args.is_some() {
                    return self.create_generic_type(udt).map(|ty| ty.into());
                }

                if let Some(generic_arg) = self.tcx.lookup_generic_arg(udt.name.symbol()) {
                    return self.conv_to_llvm_type(&generic_arg, span);
                }

                let mangled_name = mangle_udt_name(self, udt);

                match &*self.tcx.lookup_symbol(mangled_name, span)?.borrow() {
                    SymbolTableValue::Struct(sty) => sty.ty.into(),
                    SymbolTableValue::Enum(ety) => ety.ty.as_basic_type_enum(),
                    _ => todo!("Error"),
                }
            }

            TyKind::Generic(gty) => {
                let actual_ty = match self.tcx.lookup_generic_arg(gty.name.symbol()) {
                    Some(ty) => ty,
                    None => {
                        return Err(CodegenError::Undeclared {
                            name: gty.name.symbol(),
                            span,
                        }
                        .into());
                    }
                };

                self.conv_to_llvm_type(&actual_ty, span)?
            }

            TyKind::Reference(_) => self.context().ptr_type(AddressSpace::default()).into(),

            TyKind::Pointer(pty) => {
                // If the reference includes generic types, create a generic type.
                // But return a pointer type.
                if let TyKind::UserDefined(udt) = pty.get_base_type().kind.as_ref() {
                    if udt.generic_args.is_some() {
                        self.create_generic_type(udt)?;
                    }
                }

                self.context().ptr_type(AddressSpace::default()).into()
            }

            TyKind::Array((elem_ty, size)) => self
                .conv_to_llvm_type(elem_ty, span)?
                .array_type(*size)
                .into(),

            TyKind::Tuple(types) => {
                let mut llvm_types = Vec::new();
                for ty in types {
                    llvm_types.push(self.conv_to_llvm_type(ty, span)?);
                }
                context.struct_type(llvm_types.as_slice(), true).into()
            }

            TyKind::Unit => panic!("Cannot get LLVM type of unit type!"),
            TyKind::Never => panic!("Cannot get LLVM type of never type!"),
            TyKind::Inferred => panic!("Cannot get LLVM type of inferred type!"),
        })
    }

    fn verify_module(&self) -> anyhow::Result<()> {
        self.module.verify().map_err(|e| {
            self.module.print_to_stderr();
            CodegenError::LLVMError {
                what: e.to_string(),
            }
            .into()
        })
    }

    fn import_autoloads(&mut self) -> anyhow::Result<()> {
        let autoload_dir = kaede_autoload_dir();

        if !autoload_dir.exists() {
            panic!("Autoload directory not found!");
        }

        let autoload_libs = std::fs::read_dir(autoload_dir)?
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|e| e == "kd")) // Exclude non-source files
            .collect::<Vec<_>>();

        for lib in autoload_libs {
            let mut path_segments = Vec::new();

            for segment in lib.iter() {
                path_segments.push(Ident::new(
                    segment.to_string_lossy().to_string().into(),
                    Span::dummy(),
                ));
            }

            build_top_level(
                self,
                TopLevel {
                    kind: TopLevelKind::Import(Import {
                        module_path: Path {
                            segments: path_segments,
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    }),
                    visibility: Visibility::Private,
                    span: Span::dummy(),
                },
            )?;
        }

        Ok(())
    }

    /// If there was no user-defined main in the module, do nothing
    fn build_main_fn(&mut self) -> anyhow::Result<()> {
        let main_internal = match self.module.get_function("kdmain") {
            Some(fn_v) => fn_v,
            None => return Ok(()),
        };

        let main =
            self.module
                .add_function("main", self.context().i32_type().fn_type(&[], false), None);
        self.builder
            .position_at_end(self.context().append_basic_block(main, "entry"));

        let exit_status = self
            .builder
            .build_call(main_internal, &[], "")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.builder.build_return(Some(&exit_status))?;

        Ok(())
    }

    fn handle_lazy_defines(&mut self) -> anyhow::Result<()> {
        let bb_backup = self.builder.get_insert_block();

        let lazy_fns: Vec<_> = self.lazy_defines.drain(..).collect();

        for (top, generic_params, generic_args) in lazy_fns {
            build_top_level_with_generic_args(self, top, generic_params, generic_args)?;
        }

        if let Some(bb) = bb_backup {
            self.builder.position_at_end(bb);
        }

        Ok(())
    }

    // This function doesn't optimize modules.
    // Please execute 'opt' command to optimize the module.
    fn codegen(
        mut self,
        top_levels: Vec<TopLevel>,
        no_autoload: bool,
    ) -> anyhow::Result<Module<'ctx>> {
        self.gc_init()?;

        if !no_autoload {
            self.import_autoloads()?;
        }

        for top in top_levels {
            build_top_level(&mut self, top)?;
        }

        self.build_main_fn()?;

        self.handle_lazy_defines()?;

        self.verify_module()?;

        Ok(self.module)
    }
}
