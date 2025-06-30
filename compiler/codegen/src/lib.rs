use core::panic;

use error::CodegenError;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{AnyType, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

use kaede_ir::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    top::{FnDecl, LangLinkage, Param, TopLevel},
    ty::{
        make_fundamental_type, FundamentalTypeKind, Mutability, ReferenceType, Ty, TyKind,
        UserDefinedTypeKind,
    },
    CompileUnit,
};
use kaede_symbol::Symbol;
use tcx::{SymbolTable, TypeCtx};

pub mod error;
mod expr;
mod stmt;
mod tcx;
mod top;

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

pub struct CodeGenerator<'ctx> {
    cgcx: &'ctx CodegenCtx<'ctx>,
    tcx: TypeCtx<'ctx>,

    module: Module<'ctx>,
    builder: Builder<'ctx>,

    loop_break_bb_stk: Vec<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(cgcx: &'ctx CodegenCtx<'ctx>) -> anyhow::Result<Self> {
        let module = cgcx.context.create_module("main");

        let mut tcx = TypeCtx::default();
        tcx.push_symbol_table(SymbolTable::new()); // Top-level scope

        Ok(Self {
            cgcx,
            tcx,
            module,
            builder: cgcx.context.create_builder(),
            loop_break_bb_stk: vec![],
        })
    }

    fn context(&self) -> &'ctx Context {
        self.cgcx.context
    }

    pub fn get_size_in_bits(&self, type_: &dyn AnyType) -> u64 {
        self.cgcx.target_data.get_bit_size(type_)
    }

    pub fn create_gc_struct(
        &mut self,
        struct_llvm_ty: BasicTypeEnum<'ctx>,
        values: &[BasicValueEnum<'ctx>],
    ) -> anyhow::Result<PointerValue<'ctx>> {
        let mallocd = self.gc_malloc(struct_llvm_ty)?;

        for (index, init) in values.iter().enumerate() {
            let gep = unsafe {
                self.builder.build_in_bounds_gep(
                    struct_llvm_ty,
                    mallocd,
                    &[
                        self.context().i32_type().const_zero(),
                        self.context().i32_type().const_int(index as u64, false),
                    ],
                    "",
                )?
            };

            self.builder.build_store(gep, *init)?;
        }

        Ok(mallocd)
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(
        &mut self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> anyhow::Result<PointerValue<'ctx>> {
        let builder = self.context().create_builder();

        let entry = self.get_current_fn().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        Ok(builder.build_alloca(ty, name)?)
    }

    fn gc_init(&mut self) -> anyhow::Result<()> {
        // Declare GC_malloc in boehm-gc
        let return_ty = Ty {
            kind: TyKind::Reference(ReferenceType {
                refee_ty: make_fundamental_type(FundamentalTypeKind::I8, Mutability::Mut).into(),
            })
            .into(),
            mutability: Mutability::Mut,
        }
        .into();

        let params = vec![Param {
            name: Symbol::from("size".to_owned()),
            ty: make_fundamental_type(FundamentalTypeKind::U64, Mutability::Not).into(),
        }];

        let name = Symbol::from("GC_malloc".to_owned());

        self.declare_function(
            name,
            &FnDecl {
                lang_linkage: LangLinkage::Default,
                name: QualifiedSymbol::new(ModulePath::new(vec![]), name),
                is_var_args: false,
                return_ty: Some(return_ty),
                params,
            },
        )?;

        Ok(())
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

    fn conv_to_llvm_type(&mut self, ty: &Ty) -> BasicTypeEnum<'ctx> {
        let context = self.context();

        match ty.kind.as_ref() {
            TyKind::Fundamental(t) => t.as_llvm_type(self.context()),

            TyKind::UserDefined(udt) => {
                let qualified_symbol = match &udt.kind {
                    UserDefinedTypeKind::Struct(sty) => sty.name.clone(),
                    UserDefinedTypeKind::Enum(ety) => ety.name.clone(),
                };

                match self
                    .module
                    .get_struct_type(qualified_symbol.mangle().as_str())
                {
                    Some(sty) => sty.as_basic_type_enum(),
                    None => todo!("Error"),
                }
            }

            TyKind::Reference(_) | TyKind::Pointer(_) => {
                self.context().ptr_type(AddressSpace::default()).into()
            }

            TyKind::Array((elem_ty, size)) => {
                self.conv_to_llvm_type(elem_ty).array_type(*size).into()
            }

            TyKind::Tuple(types) => {
                let mut llvm_types = Vec::new();
                for ty in types {
                    llvm_types.push(self.conv_to_llvm_type(ty));
                }
                context.struct_type(llvm_types.as_slice(), true).into()
            }

            TyKind::Unit => panic!("Cannot get LLVM type of unit type!"),
            TyKind::Never => panic!("Cannot get LLVM type of never type!"),
        }
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

    // This function doesn't optimize modules.
    // Please execute 'opt' command to optimize the module.
    pub fn codegen(mut self, compile_unit: CompileUnit) -> anyhow::Result<Module<'ctx>> {
        self.gc_init()?;

        // Declare all functions
        // (This is necessary to avoid errors when generating generic functions)
        for top in compile_unit.top_levels.iter() {
            match top {
                TopLevel::Fn(fn_) => {
                    self.build_function(fn_.as_ref(), true)?;
                }
                TopLevel::Impl(impl_) => {
                    for method in impl_.methods.iter() {
                        self.build_function(method.as_ref(), true)?;
                    }
                }
                _ => {}
            }
        }

        for top in compile_unit.top_levels {
            self.build_top_level(top)?;
        }

        self.build_main_fn()?;

        self.verify_module()?;

        Ok(self.module)
    }
}
