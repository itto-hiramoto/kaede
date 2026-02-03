use std::{cell::RefCell, rc::Rc};

use inkwell::{
    module::Linkage,
    types::{BasicType, FunctionType, StructType},
    values::FunctionValue,
};

use kaede_common::rust_function_prefix;
use kaede_common::LangLinkage;
use kaede_ir::{
    top::{Enum, EnumVariant, Fn, FnDecl, Impl, Param, Struct, StructField, TopLevel},
    ty::{Ty, TyKind},
};
use kaede_symbol::Symbol;

use crate::{
    tcx::{SymbolTable, SymbolTableValue},
    CodeGenerator,
};

impl<'ctx> CodeGenerator<'ctx> {
    pub fn build_top_level(&mut self, tl: TopLevel) -> anyhow::Result<()> {
        match tl {
            TopLevel::Fn(node) => self.build_function(&node, false)?,

            TopLevel::Impl(node) => self.build_impl(&node)?,

            TopLevel::Struct(node) => self.build_struct(&node),

            TopLevel::Enum(node) => self.build_enum(&node),
        };
        Ok(())
    }

    fn build_enum(&mut self, node: &Enum) {
        self.create_enum_type(node, node.name.mangle());
    }

    fn build_struct(&mut self, node: &Struct) {
        self.create_struct_type(node.name.mangle(), &node.fields);
    }

    fn create_fn_type(
        &mut self,
        params: &[Rc<Ty>],
        return_ty: &Rc<Ty>,
        is_var_args: bool,
    ) -> anyhow::Result<FunctionType<'ctx>> {
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.conv_to_llvm_type(param).into());
        }

        Ok(if matches!(return_ty.kind.as_ref(), TyKind::Unit) {
            self.context()
                .void_type()
                .fn_type(param_types.as_slice(), is_var_args)
        } else {
            self.conv_to_llvm_type(return_ty)
                .fn_type(param_types.as_slice(), is_var_args)
        })
    }

    pub fn build_function(&mut self, node: &Fn, only_declare: bool) -> anyhow::Result<()> {
        let mangled_name = match node.decl.lang_linkage {
            // C functions are not mangled
            LangLinkage::C => node.decl.name.symbol(),

            LangLinkage::Rust => {
                format!("{}{}", rust_function_prefix(), node.decl.name.symbol()).into()
            }

            LangLinkage::Default => node.decl.name.mangle(),
        };

        self.build_fn(mangled_name, node, only_declare)
    }

    fn build_fn(
        &mut self,
        mangled_name: Symbol,
        node: &Fn,
        only_declare: bool,
    ) -> anyhow::Result<()> {
        let fn_value = self.declare_function(mangled_name, &node.decl)?;

        if only_declare {
            return Ok(());
        }

        // If the function is a declaration, the body is None.
        if let Some(body) = &node.body {
            let basic_block = self.context().append_basic_block(fn_value, "entry");
            self.builder.position_at_end(basic_block);

            self.fn_return_ty_stack.push(node.decl.return_ty.clone());

            // Allocate parameters
            let symbol_table = self.build_function_params(fn_value, &node.decl.params)?;

            // Push parameter table
            self.tcx.push_symbol_table(symbol_table);

            self.build_block(body)?;

            self.tcx.pop_symbol_table();
            self.fn_return_ty_stack.pop();

            if fn_value.get_type().get_return_type().is_none() && self.no_terminator() {
                // If return type is void and there is no termination, insert return
                self.builder.build_return(None)?;
            }
        }

        Ok(())
    }

    pub fn declare_function(
        &mut self,
        mangled_name: Symbol,
        decl: &FnDecl,
    ) -> anyhow::Result<FunctionValue<'ctx>> {
        if let Some(fn_value) = self.tcx.lookup_symbol(mangled_name) {
            // If the function is already declared, don't declare it again.
            return Ok(match &*fn_value.borrow() {
                SymbolTableValue::Function(fn_) => *fn_,
                _ => unreachable!(),
            });
        }

        let fn_type = self.create_fn_type(
            &decl.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>(),
            &decl.return_ty,
            decl.is_c_variadic,
        )?;

        let linkage = if decl.link_once {
            Linkage::LinkOnceODR
        } else {
            Linkage::External
        };

        let fn_value = self
            .module
            .add_function(mangled_name.as_str(), fn_type, Some(linkage));

        self.tcx
            .insert_symbol_to_root_scope(mangled_name, SymbolTableValue::Function(fn_value));

        Ok(fn_value)
    }

    fn build_function_params(
        &mut self,
        fn_value: FunctionValue<'ctx>,
        params: &[Param],
    ) -> anyhow::Result<SymbolTable<'ctx>> {
        let mut table = SymbolTable::new();

        for (idx, param) in params.iter().enumerate() {
            let llvm_param_ty = self.conv_to_llvm_type(&param.ty);

            let alloca = self
                .builder
                .build_alloca(llvm_param_ty, param.name.as_str())?;

            self.builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap())?;

            table.insert(
                param.name,
                Rc::new(RefCell::new(SymbolTableValue::Variable(alloca))),
            );
        }

        Ok(table)
    }

    fn create_struct_type(
        &mut self,
        mangled_name: Symbol,
        fields: &[StructField],
    ) -> StructType<'ctx> {
        let mut field_tys = Vec::new();
        for field in fields.iter() {
            field_tys.push(self.conv_to_llvm_type(&field.ty));
        }

        let ty = self.context().opaque_struct_type(mangled_name.as_str());

        ty.set_body(&field_tys, true);

        ty
    }

    /// Return None if type is not specified for all (like C's enum)
    /// The size is returned in bits
    fn get_largest_type_size_of_enum(&mut self, enum_items: &[EnumVariant]) -> Option<u64> {
        let mut largest = 0;

        for item in enum_items.iter() {
            if let Some(ty) = &item.ty {
                let llvm_ty = self.conv_to_llvm_type(ty);
                let size = self.get_size_in_bits(&llvm_ty);
                largest = std::cmp::max(size, largest);
            }
        }

        match largest {
            0 => None,
            _ => Some(largest),
        }
    }

    fn create_enum_type(&mut self, node: &Enum, mangled_name: Symbol) -> StructType<'ctx> {
        let largest_type_size = self.get_largest_type_size_of_enum(&node.variants);

        // If there is an item with a specified type
        // Specified: { i32, [i8; LARGEST_TYPE_SIZE_IN_BYTES] }
        // Not specified: { i32 }
        let ty = match largest_type_size {
            Some(size) => {
                let ty = self.context().opaque_struct_type(mangled_name.as_str());

                ty.set_body(
                    &[
                        self.context().i32_type().into(),
                        self.context()
                            .i8_type()
                            .array_type((size / 8) as u32)
                            .into(),
                    ],
                    true,
                );

                ty
            }

            None => {
                let ty = self.context().opaque_struct_type(mangled_name.as_str());

                ty.set_body(&[self.context().i32_type().into()], true);

                ty
            }
        };

        ty
    }

    fn build_impl(&mut self, node: &Impl) -> anyhow::Result<()> {
        for method in node.methods.iter() {
            self.build_function(method, false)?;
        }

        Ok(())
    }
}
