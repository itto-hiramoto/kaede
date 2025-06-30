use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use kaede_ir::stmt::{Assign, Block, Let, Stmt, TupleUnpack};
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
        todo!("{:?}", node)
    }

    // fn build_tuple_unpacking(&mut self, node: &TupleUnpack) -> anyhow::Result<()> {
    //     let tuple = self.build_expr(&node.init)?.unwrap();

    //     let tuple_ref_ty = tuple.get_type();

    //     let (tuple_len, tuple_mutability) =
    //         if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
    //             match rty.refee_ty.kind.as_ref() {
    //                 TyKind::Tuple(tuple_ty) => (tuple_ty.len(), tuple_ref_ty.mutability),

    //                 kind => {
    //                     return Err(CodegenError::MismatchedTypes {
    //                         types: (
    //                             create_inferred_tuple(node.names.len()).to_string(),
    //                             kind.to_string(),
    //                         ),
    //                         span: node.span,
    //                     }
    //                     .into());
    //                 }
    //             }
    //         } else {
    //             return Err(CodegenError::MismatchedTypes {
    //                 types: (
    //                     create_inferred_tuple(node.names.len()).to_string(),
    //                     tuple_ref_ty.kind.to_string(),
    //                 ),
    //                 span: node.span,
    //             }
    //             .into());
    //         };

    //     if node.names.len() != tuple_len {
    //         return Err(CodegenError::NumberOfTupleFieldsDoesNotMatch {
    //             lens: (node.names.len(), tuple_len),
    //             span: node.span,
    //         }
    //         .into());
    //     }

    //     // Unpacking
    //     for (index, name_and_mutability) in node.names.iter().enumerate() {
    //         let (name, mutability) = match name_and_mutability {
    //             Some(x) => (&x.0, x.1),

    //             // Ignore field
    //             None => continue,
    //         };

    //         if mutability.is_mut() && tuple_mutability.is_not() {
    //             todo!("Error")
    //         }

    //         self.unpack_one_tuple_field(&tuple, index as u32, name, mutability, node.span)?;
    //     }

    //     Ok(())
    // }

    // fn unpack_one_tuple_field(
    //     &mut self,
    //     tuple: &Value<'ctx>,
    //     index: u32,
    //     unpacked_name: &Ident,
    //     unpacked_mutability: Mutability,
    //     span: Span,
    // ) -> anyhow::Result<()> {
    //     assert!(matches!(
    //         tuple.get_type().kind.as_ref(),
    //         TyKind::Reference(_)
    //     ));

    //     let tuple_ref_ty = tuple.get_type();

    //     let tuple_ty = if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
    //         rty
    //     } else {
    //         unreachable!();
    //     };

    //     let unpacked_value = build_tuple_indexing(
    //         self.cucx,
    //         tuple.get_value().into_pointer_value(),
    //         index,
    //         &tuple_ty.refee_ty,
    //         span,
    //     )?;

    //     self.build_normal_let(
    //         unpacked_name,
    //         unpacked_mutability,
    //         Some(unpacked_value),
    //         Ty::new_inferred(unpacked_mutability).into(),
    //         span,
    //     )?;

    //     Ok(())
    // }
}
