use std::rc::Rc;

use crate::{
    tcx::{SymbolTable, SymbolTableValue},
    CodeGenerator,
};

use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
    IntPredicate,
};

use kaede_ir::{
    expr::{
        ArrayLiteral, Binary, BinaryKind, Cast, Else, EnumUnpack, EnumVariant, Expr, ExprKind,
        FieldAccess, FnCall, If, Indexing, Int, LogicalNot, Loop, StringLiteral, StructLiteral,
        TupleIndexing, TupleLiteral, Variable,
    },
    stmt::Block,
    ty::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind},
};

pub type Value<'ctx> = Option<BasicValueEnum<'ctx>>;

impl<'a, 'ctx> CodeGenerator<'ctx> {
    /// Generate expression code
    pub fn build_expr(&mut self, node: &Expr) -> anyhow::Result<Value<'ctx>> {
        Ok(match &node.kind {
            ExprKind::Block(block) => self.build_block_expr(block)?,

            ExprKind::Break => self.build_break()?,

            ExprKind::Return(value) => self.build_return(value)?,

            ExprKind::Loop(loop_) => self.build_loop(loop_)?,

            ExprKind::Int(int) => self.build_int(int)?,

            ExprKind::If(node) => self.build_if(node)?,

            ExprKind::StructLiteral(node) => self.struct_literal(node)?,

            ExprKind::StringLiteral(node) => self.build_string_literal(node)?,

            ExprKind::LogicalNot(node) => self.build_logical_not(node)?,

            ExprKind::Variable(node) => self.build_variable(node)?,

            ExprKind::BooleanLiteral(node) => self.build_boolean_literal(*node),

            ExprKind::FnCall(node) => self.build_fn_call(node)?,

            ExprKind::ArrayLiteral(node) => self.build_array_literal(node)?,

            ExprKind::TupleLiteral(node) => self.build_tuple_literal(node)?,

            ExprKind::Cast(node) => self.build_cast(node)?,

            ExprKind::Indexing(node) => self.build_array_indexing(node)?,

            ExprKind::Binary(node) => self.build_arithmetic_binary(node)?,

            ExprKind::FieldAccess(node) => self.build_field_access(node)?,

            ExprKind::TupleIndexing(node) => self.build_tuple_indexing(node)?,

            ExprKind::EnumVariant(node) => self.build_enum_variant(node)?,
        })
    }

    fn build_int(&self, int: &Int) -> anyhow::Result<Value<'ctx>> {
        use kaede_ir::expr::IntKind::*;

        match int.kind {
            I32(n) => Ok(Some(
                self.context().i32_type().const_int(n as u64, true).into(),
            )),

            U64(n) => Ok(Some(self.context().i64_type().const_int(n, false).into())),
        }
    }

    /// Unit value if the end of the block is not an expression
    fn build_block_expr(&mut self, block: &Block) -> anyhow::Result<Value<'ctx>> {
        if block.body.is_empty() && block.last_expr.is_none() {
            return Ok(None);
        }

        self.tcx.push_symbol_table(SymbolTable::new());

        for stmt in block.body.iter() {
            self.build_statement(stmt)?;
        }

        let value = if let Some(last_expr) = &block.last_expr {
            self.build_expr(last_expr)?
        } else {
            None
        };

        self.tcx.pop_symbol_table();

        Ok(value)
    }

    fn build_break(&self) -> anyhow::Result<Value<'ctx>> {
        match self.loop_break_bb_stk.last() {
            Some(bb) => {
                self.builder.build_unconditional_branch(*bb)?;
                Ok(None)
            }

            // Handled in semantic analysis
            None => unreachable!(),
        }
    }

    fn build_return(&mut self, value: &Option<Box<Expr>>) -> anyhow::Result<Value<'ctx>> {
        match &value {
            Some(val) => {
                let value = self.build_expr(val)?;
                self.builder.build_return(Some(&value.unwrap()))?
            }

            None => self.builder.build_return(None)?,
        };

        Ok(None)
    }

    fn build_loop(&mut self, node: &Loop) -> anyhow::Result<Value<'ctx>> {
        let parent = self.get_current_fn();

        let body_bb = self.context().append_basic_block(parent, "loopbody");

        let cont_bb = self.context().append_basic_block(parent, "loopcont");

        // Setup for break statement
        self.loop_break_bb_stk.push(cont_bb);

        // Build body block
        self.builder.build_unconditional_branch(body_bb)?;
        self.builder.position_at_end(body_bb);
        self.build_block(&node.body)?;

        self.loop_break_bb_stk.pop();

        // Loop!
        if self.no_terminator() {
            self.builder.build_unconditional_branch(body_bb)?;
        }

        self.builder.position_at_end(cont_bb);

        Ok(None)
    }

    fn build_if(&mut self, node: &If) -> anyhow::Result<Value<'ctx>> {
        let parent = self.get_current_fn();
        let zero_const = self.context().bool_type().const_zero();

        let cond = self.build_expr(&node.cond)?.unwrap().into_int_value();

        let cond = self
            .builder
            .build_int_compare(IntPredicate::NE, cond, zero_const, "ifcond")?;

        let then_bb = self.context().append_basic_block(parent, "then");
        let else_bb = self.context().append_basic_block(parent, "else");
        let cont_bb = self.context().append_basic_block(parent, "ifcont");

        self.builder
            .build_conditional_branch(cond, then_bb, else_bb)?;

        // Build then block
        self.builder.position_at_end(then_bb);

        // Enum unpack (optional)
        if let Some(enum_unpack) = &node.enum_unpack {
            // Push symbol table for enum unpack
            self.tcx.push_symbol_table(SymbolTable::new());
            self.build_enum_unpack(enum_unpack)?;
        }

        // Build then code
        let then_val = self.build_expr(&node.then)?;

        // Pop symbol table for enum unpack.
        if node.enum_unpack.is_some() {
            self.tcx.pop_symbol_table();
        }

        // Since there can be no more than one terminator per block
        if self.no_terminator() {
            self.builder.build_unconditional_branch(cont_bb)?;
        }

        let then_bb = self.builder.get_insert_block().unwrap();

        // Build else block
        self.builder.position_at_end(else_bb);

        // Build else code
        let else_val = match &node.else_ {
            Some(else_) => match else_.as_ref() {
                Else::If(if_) => self.build_if(if_)?,
                Else::Block(block) => self.build_expr(block)?,
            },

            _ => {
                if node.is_match {
                    // Build unreachable
                    self.builder.build_unreachable()?;
                }

                None
            }
        };

        let else_bb = self.builder.get_insert_block().unwrap();

        // Since there can be no more than one terminator per block
        if self.no_terminator() {
            self.builder.build_unconditional_branch(cont_bb)?;
        }

        self.builder.position_at_end(cont_bb);

        // Either then_val or else_val could be never
        let phi_ty = if then_val.is_none() {
            return Ok(else_val);
        } else if else_val.is_none() {
            return Ok(then_val);
        } else {
            node.then.ty.clone()
        };

        let llvm_phi_ty = self.conv_to_llvm_type(&phi_ty);
        let phi = self.builder.build_phi(llvm_phi_ty, "iftmp")?;

        phi.add_incoming(&[(&then_val.unwrap(), then_bb), (&else_val.unwrap(), else_bb)]);

        Ok(Some(phi.as_basic_value()))
    }

    fn build_enum_unpack(&mut self, enum_unpack: &EnumUnpack) -> anyhow::Result<()> {
        let variant_ty_llvm = self.conv_to_llvm_type(&enum_unpack.variant_ty);

        let pointee_ty = self.conv_to_llvm_type(&Ty {
            kind: TyKind::UserDefined(enum_unpack.enum_ty.clone()).into(),
            mutability: Mutability::Not,
        });

        let ptr = self
            .build_expr(&enum_unpack.enum_value)?
            .unwrap()
            .into_pointer_value();

        // { i32, [N x i8] }
        //        ^^^^^^^^
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                pointee_ty,
                ptr,
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_int(1_u64, false),
                ],
                "",
            )?
        };

        let variant_value = self.builder.build_load(variant_ty_llvm, gep, "")?;

        self.build_let_internal(enum_unpack.name, variant_ty_llvm, Some(variant_value))?;

        Ok(())
    }

    fn struct_literal(&mut self, node: &StructLiteral) -> anyhow::Result<Value<'ctx>> {
        let struct_llvm_ty = self
            .context()
            .get_struct_type(node.struct_info.name.mangle().as_str())
            .unwrap();

        let mut values = Vec::new();

        for value in node.values.iter() {
            let field_info = &node
                .struct_info
                .fields
                .iter()
                .find(|f| f.name == value.0)
                .unwrap();

            let value = self.build_expr(&value.1)?;

            // To sort by offset, store offset
            values.push((field_info.offset, value.unwrap()));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));
        let values: Vec<_> = values.iter().map(|e| e.1).collect();

        let value = self.create_gc_struct(struct_llvm_ty.as_basic_type_enum(), &values)?;

        Ok(Some(value.into()))
    }

    fn build_string_literal(&mut self, node: &StringLiteral) -> anyhow::Result<Value<'ctx>> {
        let global_s = self
            .builder
            .build_global_string_ptr(node.syb.as_str(), "str")?;

        let str_llvm_ty = self.conv_to_llvm_type(&Rc::new(make_fundamental_type(
            FundamentalTypeKind::Str,
            Mutability::Not,
        )));

        let p = self.create_gc_struct(
            str_llvm_ty,
            &[
                global_s.as_basic_value_enum(),
                self.context()
                    .i64_type()
                    .const_int(node.syb.as_str().len() as u64, false)
                    .into(),
            ],
        )?;

        Ok(Some(p.into()))
    }

    fn build_logical_not(&mut self, node: &LogicalNot) -> anyhow::Result<Value<'ctx>> {
        let operand = self.build_expr(&node.operand)?.unwrap();

        let zero = operand.get_type().const_zero();

        // Compared to zero, it would be equivalent to 'logical not'
        Ok(Some(
            self.builder
                .build_int_compare(
                    IntPredicate::EQ,
                    operand.into_int_value(),
                    zero.into_int_value(),
                    "",
                )?
                .into(),
        ))
    }

    fn build_variable(&mut self, node: &Variable) -> anyhow::Result<Value<'ctx>> {
        let symbol_kind = self.tcx.lookup_symbol(node.name);

        let ptr = match &*symbol_kind.borrow() {
            SymbolTableValue::Variable(var) => *var,
        };

        let llvm_ty = self.conv_to_llvm_type(&node.ty);

        Ok(Some(self.builder.build_load(llvm_ty, ptr, "")?))
    }

    fn build_boolean_literal(&self, value: bool) -> Value<'ctx> {
        Some(
            self.context()
                .bool_type()
                .const_int(value as u64, false)
                .into(),
        )
    }

    fn build_fn_call(&mut self, node: &FnCall) -> anyhow::Result<Value<'ctx>> {
        let args = {
            let mut args = Vec::new();

            for arg in node.args.0.iter() {
                args.push(self.build_expr(arg)?.unwrap().into());
            }

            args
        };

        let mangled_name = node.callee.decl.name.mangle();

        Ok(self
            .builder
            .build_call(
                self.module.get_function(mangled_name.as_str()).unwrap(),
                args.as_slice(),
                "",
            )?
            .try_as_basic_value()
            .left())
    }

    fn build_array_literal(&mut self, node: &ArrayLiteral) -> anyhow::Result<Value<'ctx>> {
        assert!(!node.elements.is_empty());

        let mut elems = Vec::new();

        // Compile elements
        for elem in node.elements.iter() {
            elems.push(self.build_expr(elem)?.unwrap());
        }

        let array_ty = Rc::new(Ty {
            kind: TyKind::Array((node.elements[0].ty.clone(), elems.len() as u32)).into(),
            mutability: Mutability::Not,
        });

        let array_llvm_ty = self.conv_to_llvm_type(&array_ty);

        let mallocd = self.gc_malloc(array_llvm_ty)?;

        for (idx, elem) in elems.iter().enumerate() {
            let gep = unsafe {
                self.builder.build_in_bounds_gep(
                    array_llvm_ty,
                    mallocd,
                    &[
                        self.context().i32_type().const_zero(),
                        self.context().i32_type().const_int(idx as u64, false),
                    ],
                    "",
                )?
            };

            self.builder.build_store(gep, *elem)?;
        }

        Ok(Some(mallocd.into()))
    }

    fn build_tuple_literal(&mut self, node: &TupleLiteral) -> anyhow::Result<Value<'ctx>> {
        let element_values = {
            let mut v = Vec::new();
            for e in node.elements.iter() {
                v.push(self.build_expr(e)?.unwrap());
            }
            v
        };

        let tuple_ty = Ty {
            kind: TyKind::Tuple(node.elements.iter().map(|e| e.ty.clone()).collect()).into(),
            mutability: Mutability::Not,
        };

        let tuple_llvm_ty = self.conv_to_llvm_type(&tuple_ty);

        let value = self.create_gc_struct(tuple_llvm_ty, &element_values)?;

        Ok(Some(value.into()))
    }

    fn build_cast(&mut self, node: &Cast) -> anyhow::Result<Value<'ctx>> {
        let value = self.build_expr(&node.operand)?.unwrap();

        let value_ty = node.operand.ty.clone();
        let target_ty = node.target_ty.clone();

        if value_ty.kind.is_int_or_bool() && target_ty.kind.is_int_or_bool() {
            self.build_int_cast(value, target_ty.clone())
        } else if matches!(
            value_ty.kind.as_ref(),
            TyKind::Reference(_) | TyKind::Pointer(_)
        ) && matches!(
            target_ty.kind.as_ref(),
            TyKind::Reference(_) | TyKind::Pointer(_)
        ) {
            self.build_ptr_cast(value, target_ty.clone())
        } else {
            todo!("Error");
        }
    }

    fn build_int_cast(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_ty: Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        assert!(value.is_int_value());
        assert!(target_ty.kind.is_int_or_bool());

        let target_llvm_ty = self.conv_to_llvm_type(&target_ty);

        Ok(Some(
            self.builder
                .build_int_cast_sign_flag(
                    value.into_int_value(),
                    target_llvm_ty.into_int_type(),
                    target_ty.kind.is_signed(),
                    "",
                )?
                .as_basic_value_enum(),
        ))
    }

    fn build_ptr_cast(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_ty: Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        let target_llvm_ty = self.conv_to_llvm_type(&target_ty);

        Ok(Some(
            self.builder
                .build_pointer_cast(
                    value.into_pointer_value(),
                    target_llvm_ty.into_pointer_type(),
                    "",
                )?
                .as_basic_value_enum(),
        ))
    }

    fn build_array_indexing(&mut self, node: &Indexing) -> anyhow::Result<Value<'ctx>> {
        // A raw array cannot be passed, but a pointer(reference) to an array
        let array_ref_value = self.build_expr(&node.operand)?.unwrap();

        let array_ty = {
            match node.operand.ty.kind.as_ref() {
                TyKind::Reference(rty) => {
                    if matches!(rty.get_base_type().kind.as_ref(), TyKind::Array(_)) {
                        rty.get_base_type().clone()
                    } else {
                        todo!("Error");
                    }
                }

                _ => unreachable!(),
            }
        };

        let array_llvm_ty = self.conv_to_llvm_type(&array_ty).into_array_type();

        let ptr_to_array = array_ref_value.into_pointer_value();

        let index = self.build_expr(&node.index)?.unwrap();

        // Calculate the address of the index-th element
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                array_llvm_ty,
                ptr_to_array,
                &[
                    self.context().i32_type().const_zero(),
                    index.into_int_value(),
                ],
                "",
            )?
        };

        Ok(Some(self.builder.build_load(
            array_llvm_ty.get_element_type(),
            gep,
            "",
        )?))
    }

    fn build_arithmetic_binary(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        use BinaryKind::*;

        let left_ty = node.lhs.ty.clone();
        let right_ty = node.rhs.ty.clone();

        let left = self.build_expr(&node.lhs)?.unwrap();
        let right = self.build_expr(&node.rhs)?.unwrap();

        // If operands are not int
        if !(left.is_int_value() && right.is_int_value()) {
            todo!("Error");
        }

        let left_int = left.into_int_value();
        let right_int = right.into_int_value();

        Ok(match node.kind {
            LogicalOr => self.build_logical_or(left_int, right_int)?,

            LogicalAnd => self.build_logical_and(left_int, right_int)?,

            Add => Some(self.builder.build_int_add(left_int, right_int, "")?.into()),

            Sub => Some(self.builder.build_int_sub(left_int, right_int, "")?.into()),

            Mul => Some(self.builder.build_int_mul(left_int, right_int, "")?.into()),

            Rem => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_signed_rem(left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_unsigned_rem(left_int, right_int, "")?
                            .into(),
                    )
                }
            }

            Div => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_signed_div(left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_unsigned_div(left_int, right_int, "")?
                            .into(),
                    )
                }
            }

            Eq => self.build_int_equal(left_int, right_int)?,

            Ne => Some(
                self.builder
                    .build_int_compare(IntPredicate::NE, left_int, right_int, "")?
                    .into(),
            ),

            Lt => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SLT, left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::ULT, left_int, right_int, "")?
                            .into(),
                    )
                }
            }

            Le => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SLE, left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::ULE, left_int, right_int, "")?
                            .into(),
                    )
                }
            }

            Gt => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SGT, left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::UGT, left_int, right_int, "")?
                            .into(),
                    )
                }
            }

            Ge => {
                if left_ty.kind.is_signed() || right_ty.kind.is_signed() {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::SGE, left_int, right_int, "")?
                            .into(),
                    )
                } else {
                    Some(
                        self.builder
                            .build_int_compare(IntPredicate::UGE, left_int, right_int, "")?
                            .into(),
                    )
                }
            }
        })
    }

    fn build_logical_or(
        &self,
        b1: IntValue<'ctx>,
        b2: IntValue<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        let bool_type = self.context().bool_type();

        let result = self.builder.build_or(b1, b2, "")?;

        let zero = bool_type.const_zero();
        let cmp = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "")?;
        let result = self
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "")?;

        Ok(Some(result))
    }

    fn build_logical_and(
        &self,
        b1: IntValue<'ctx>,
        b2: IntValue<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        let bool_type = self.context().bool_type();

        let result = self.builder.build_and(b1, b2, "")?;

        let zero = bool_type.const_zero();
        let cmp = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "")?;
        let result = self
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "")?;

        Ok(Some(result))
    }

    fn build_int_equal(
        &self,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        Ok(Some(
            self.builder
                .build_int_compare(IntPredicate::EQ, left, right, "")?
                .into(),
        ))
    }

    fn build_field_access(&mut self, node: &FieldAccess) -> anyhow::Result<Value<'ctx>> {
        let llvm_struct_ty = self
            .module
            .get_struct_type(node.struct_info.name.mangle().as_str())
            .unwrap();

        let struct_value = self.build_expr(&node.operand)?.unwrap();

        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                llvm_struct_ty,
                struct_value.into_pointer_value(),
                &[
                    self.context().i32_type().const_zero(),
                    self.context()
                        .i32_type()
                        .const_int(node.field_offset, false),
                ],
                "",
            )?
        };

        let field_ty = node.struct_info.fields[node.field_offset as usize]
            .ty
            .clone();

        let llvm_field_ty = self.conv_to_llvm_type(&field_ty);

        Ok(Some(self.builder.build_load(llvm_field_ty, gep, "")?))
    }

    fn build_tuple_indexing(&mut self, node: &TupleIndexing) -> anyhow::Result<Value<'ctx>> {
        return self.build_tuple_indexing_internal(node);
    }

    fn build_tuple_indexing_internal(
        &mut self,
        node: &TupleIndexing,
    ) -> anyhow::Result<Value<'ctx>> {
        let tuple = self.build_expr(&node.tuple)?.unwrap();

        self.build_indexing_common(
            tuple.into_pointer_value(),
            node.tuple.ty.clone(),
            node.element_ty.clone(),
            // Tuple is wrapped in a pointer (Generated from GC).
            // So, number of indexes is 2.
            &[
                self.context().i32_type().const_zero(),
                self.context()
                    .i32_type()
                    .const_int(node.index as u64, false),
            ],
        )
    }

    fn build_indexing_common(
        &mut self,
        ptr: PointerValue<'ctx>,
        ptr_ty: Rc<Ty>,
        element_ty: Rc<Ty>,
        ordered_indexes: &[IntValue<'ctx>],
    ) -> anyhow::Result<Value<'ctx>> {
        let pointee_ty = if let TyKind::Reference(rty) = ptr_ty.kind.as_ref() {
            rty.refee_ty.clone()
        } else {
            unreachable!()
        };

        let llvm_pointee_ty = self.conv_to_llvm_type(&pointee_ty);

        let llvm_element_ty = self.conv_to_llvm_type(&element_ty);

        let gep = unsafe {
            self.builder
                .build_in_bounds_gep(llvm_pointee_ty, ptr, ordered_indexes, "")?
        };

        Ok(Some(self.builder.build_load(llvm_element_ty, gep, "")?))
    }

    fn build_enum_variant(&mut self, node: &EnumVariant) -> anyhow::Result<Value<'ctx>> {
        let enum_llvm_ty = self
            .module
            .get_struct_type(node.enum_info.name.mangle().as_str())
            .unwrap();

        let offset_in_llvm = self
            .context()
            .i32_type()
            .const_int(node.variant_offset as u64, false)
            .into();

        let init_values = match &node.value {
            Some(v) => vec![offset_in_llvm, self.build_expr(v)?.unwrap()],
            None => vec![offset_in_llvm],
        };

        Ok(Some(
            self.create_gc_struct(enum_llvm_ty.as_basic_type_enum(), &init_values)?
                .into(),
        ))
    }
}
