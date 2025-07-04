use std::rc::Rc;

use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use kaede_ir::stmt::{Assign, Block, Let, Stmt, TupleUnpack};
use kaede_ir::ty::{Ty, TyKind};
use kaede_symbol::Symbol;

use crate::expr::Value;
use crate::tcx::{SymbolTable, SymbolTableValue};
use crate::{get_loaded_pointer, CodeGenerator};

impl<'a, 'ctx> CodeGenerator<'ctx> {
    /// Generate statement code
    pub fn build_statement(&mut self, stmt: &Stmt) -> anyhow::Result<()> {
        match &stmt {
            Stmt::Expr(e) => {
                self.build_expr(e)?;
            }

            Stmt::Let(node) => self.build_let(node)?,

            Stmt::TupleUnpack(node) => self.build_tuple_unpacking(node)?,

            Stmt::Assign(node) => self.build_assign(node)?,
        }

        Ok(())
    }

    pub fn build_block(&mut self, block: &Block) -> anyhow::Result<()> {
        self.tcx.push_symbol_table(SymbolTable::new());

        for stmt in &block.body {
            self.build_statement(stmt)?;
        }

        if let Some(last_expr) = &block.last_expr {
            self.build_expr(last_expr)?;
        }

        self.tcx.pop_symbol_table();

        Ok(())
    }

    fn build_assign(&mut self, node: &Assign) -> anyhow::Result<()> {
        let assignee = self.build_expr(&node.assignee)?.unwrap();

        let ptr_to_left = match get_loaded_pointer(&assignee.as_instruction_value().unwrap()) {
            Some(p) => p,
            None => panic!("Invalid left of assignment"),
        };

        let value = self.build_expr(&node.value)?.unwrap();

        self.builder.build_store(ptr_to_left, value)?;

        Ok(())
    }

    fn build_let(&mut self, node: &Let) -> anyhow::Result<()> {
        let name = node.name;

        if let Some(init) = &node.init {
            let value = self.build_expr(init)?;

            let llvm_ty = self.conv_to_llvm_type(&node.ty);

            self.build_let_internal(name, llvm_ty, value)
        } else {
            todo!()
        }
    }

    pub fn build_let_internal(
        &mut self,
        name: Symbol,
        ty: BasicTypeEnum<'ctx>,
        value: Value<'ctx>,
    ) -> anyhow::Result<()> {
        let alloca = self.create_entry_block_alloca(name.as_str(), ty)?;

        self.tcx
            .insert_symbol_to_current_scope(name, SymbolTableValue::Variable(alloca));

        // Initialization
        self.builder.build_store(alloca, value.unwrap())?;

        Ok(())
    }

    fn build_tuple_unpacking(&mut self, node: &TupleUnpack) -> anyhow::Result<()> {
        let tuple = self.build_expr(&node.init)?.unwrap();

        for (index, name) in node.names.iter().enumerate() {
            if let Some(name) = name {
                self.unpack_tuple_field(node.init.ty.clone(), tuple, index as u32, *name)?;
            }
        }

        Ok(())
    }

    fn unpack_tuple_field(
        &mut self,
        tuple_ref_ty: Rc<Ty>,
        tuple: BasicValueEnum<'ctx>,
        index: u32,
        name: Symbol,
    ) -> anyhow::Result<()> {
        let tuple_ty = if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
            rty.get_base_type()
        } else {
            unreachable!();
        };

        let unpacked_value = self
            .build_indexing_common(
                tuple.into_pointer_value(),
                tuple_ref_ty.clone(),
                tuple_ty.clone(),
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_int(index as u64, false),
                ],
            )?
            .unwrap();

        self.build_let_internal(name, unpacked_value.get_type(), Some(unpacked_value))?;

        Ok(())
    }
}
