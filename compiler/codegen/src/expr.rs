use std::{cell::RefCell, rc::Rc};

use crate::{
    error::CodegenError,
    tcx::{SymbolTable, SymbolTableValue},
    CodeGenerator,
};

use inkwell::{
    module::Linkage,
    types::{BasicType, StructType},
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
    IntPredicate,
};

use kaede_common::rust_function_prefix;
use kaede_ir::{
    expr::{
        ArrayLiteral, ArrayRepeat, Binary, BinaryKind, BuiltinFnCall, BuiltinFnCallKind,
        ByteStringLiteral, Cast, CharLiteral, Closure, Else, EnumUnpack, EnumVariant, Expr,
        ExprKind, FieldAccess, FnCall, FnPointer, If, Indexing, Int, LogicalNot, Loop, Slicing,
        StringLiteral, StructLiteral, TupleIndexing, TupleLiteral, Variable,
    },
    stmt::Block,
    ty::{
        make_fundamental_type, FundamentalType, FundamentalTypeKind, Mutability, PointerType, Ty,
        TyKind,
    },
};
use kaede_symbol::Symbol;

pub type Value<'ctx> = Option<BasicValueEnum<'ctx>>;

impl<'ctx> CodeGenerator<'ctx> {
    fn array_type_info(ty: &Rc<Ty>) -> Option<(Rc<Ty>, u32)> {
        match ty.kind.as_ref() {
            TyKind::Reference(rty) => match rty.get_base_type().kind.as_ref() {
                TyKind::Array((elem, len)) => Some((elem.clone(), *len)),
                _ => None,
            },
            TyKind::Array((elem, len)) => Some((elem.clone(), *len)),
            _ => None,
        }
    }

    fn slice_element_ty(ty: &Rc<Ty>) -> Option<Rc<Ty>> {
        match ty.kind.as_ref() {
            TyKind::Reference(rty) => match rty.get_base_type().kind.as_ref() {
                TyKind::Slice(elem) => Some(elem.clone()),
                _ => None,
            },
            TyKind::Slice(elem) => Some(elem.clone()),
            _ => None,
        }
    }

    fn build_array_to_slice_conversion(
        &mut self,
        array_ptr: PointerValue<'ctx>,
        array_len: u32,
        elem_ty: &Rc<Ty>,
    ) -> anyhow::Result<BasicValueEnum<'ctx>> {
        let elem_llvm_ty = self.conv_to_llvm_type(elem_ty);
        let array_llvm_ty = elem_llvm_ty.array_type(array_len);

        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                array_llvm_ty,
                array_ptr,
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_zero(),
                ],
                "",
            )?
        };

        let slice_ty = Rc::new(Ty {
            kind: TyKind::Slice(elem_ty.clone()).into(),
            mutability: Mutability::Not,
        });

        let slice_llvm_ty = self.conv_to_llvm_type(&slice_ty);
        let len_value = self.context().i64_type().const_int(array_len as u64, false);

        let slice_ptr = self.create_gc_struct(
            slice_llvm_ty,
            &[
                data_ptr.as_basic_value_enum(),
                len_value.as_basic_value_enum(),
            ],
        )?;

        Ok(slice_ptr.as_basic_value_enum())
    }

    pub(crate) fn coerce_value_to_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        value_ty: &Rc<Ty>,
        expected_ty: &Rc<Ty>,
    ) -> anyhow::Result<BasicValueEnum<'ctx>> {
        if let (Some((_, array_len)), Some(slice_elem)) = (
            Self::array_type_info(value_ty),
            Self::slice_element_ty(expected_ty),
        ) {
            let slice_val = self.build_array_to_slice_conversion(
                value.into_pointer_value(),
                array_len,
                &slice_elem,
            )?;
            return Ok(slice_val);
        }

        Ok(value)
    }

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
            ExprKind::ByteStringLiteral(node) => self.build_byte_string_literal(node)?,

            ExprKind::CharLiteral(node) => self.build_char_literal(node)?,

            ExprKind::LogicalNot(node) => self.build_logical_not(node)?,

            ExprKind::Variable(node) => self.build_variable(node)?,

            ExprKind::BooleanLiteral(node) => self.build_boolean_literal(*node),

            ExprKind::FnCall(node) => self.build_fn_call(node)?,

            ExprKind::FnPointer(fn_ptr) => self.build_fn_pointer(fn_ptr, &node.ty)?,

            ExprKind::ArrayLiteral(node) => self.build_array_literal(node)?,
            ExprKind::ArrayRepeat(node) => self.build_array_repeat(node)?,

            ExprKind::TupleLiteral(node) => self.build_tuple_literal(node)?,

            ExprKind::Cast(node) => self.build_cast(node)?,

            ExprKind::Indexing(node) => self.build_indexing(node)?,
            ExprKind::Slicing(node) => self.build_slicing(node)?,

            ExprKind::Binary(node) => self.build_arithmetic_binary(node)?,

            ExprKind::FieldAccess(node) => self.build_field_access(node)?,

            ExprKind::TupleIndexing(node) => self.build_tuple_indexing(node)?,

            ExprKind::EnumVariant(node) => self.build_enum_variant(node)?,

            ExprKind::BuiltinFnCall(node) => self.build_builtin_fn_call(node)?,

            ExprKind::Closure(closure) => self.build_closure(closure, &node.ty)?,
        })
    }

    fn build_builtin_fn_call(&mut self, node: &BuiltinFnCall) -> anyhow::Result<Value<'ctx>> {
        match node.kind {
            BuiltinFnCallKind::Unreachable => {
                self.builder.build_unreachable()?;
                Ok(None)
            }

            BuiltinFnCallKind::Str => {
                let p = self.build_expr(&node.args.0[0])?.unwrap();
                let len = self.build_expr(&node.args.0[1])?.unwrap();
                self.build_string_literal_internal(p, len.into_int_value())
            }

            BuiltinFnCallKind::PointerAdd => {
                let ptr = self
                    .build_expr(&node.args.0[0])?
                    .unwrap()
                    .into_pointer_value();
                let offset = self.build_expr(&node.args.0[1])?.unwrap().into_int_value();

                // Get the pointee type from the first argument's type
                let pointee_ty = if let TyKind::Pointer(pty) = node.args.0[0].ty.kind.as_ref() {
                    &pty.pointee_ty
                } else {
                    return Err(CodegenError::ExpectedPointerType.into());
                };

                let llvm_pointee_ty = self.conv_to_llvm_type(pointee_ty);

                let result_ptr = unsafe {
                    self.builder
                        .build_in_bounds_gep(llvm_pointee_ty, ptr, &[offset], "")?
                };

                Ok(Some(result_ptr.into()))
            }

            BuiltinFnCallKind::SizeOf => {
                let llvm_ty = self.conv_to_llvm_type(&node.args.0[0].ty);
                let size_val = llvm_ty.size_of().unwrap();
                let u64_ty = self.context().i64_type();

                let size_u64 = if size_val.get_type() == u64_ty {
                    size_val
                } else {
                    self.builder
                        .build_int_cast(size_val, u64_ty, "sizeof.cast")?
                };

                Ok(Some(size_u64.as_basic_value_enum()))
            }
        }
    }

    fn build_closure(&mut self, node: &Closure, ty: &Rc<Ty>) -> anyhow::Result<Value<'ctx>> {
        // TODO: Move this to semantic analysis phase

        let closure_ty = match ty.kind.as_ref() {
            TyKind::Closure(closure_ty) => closure_ty,
            _ => return Err(CodegenError::ExpectedClosureType.into()),
        };

        let (closure_struct_ty, closure_fn_ty, captures_tuple_ty) =
            self.closure_llvm_types(closure_ty);

        let saved_block = self.builder.get_insert_block().unwrap();

        let closure_name = self.fresh_closure_name();
        let closure_fn = self.module.add_function(
            closure_name.as_str(),
            closure_fn_ty,
            Some(Linkage::Internal),
        );

        let entry_bb = self.context().append_basic_block(closure_fn, "entry");
        self.builder.position_at_end(entry_bb);

        self.fn_return_ty_stack
            .push(Some(closure_ty.ret_ty.clone()));

        let mut symbol_table = SymbolTable::new();

        let captures_param = closure_fn.get_nth_param(0).unwrap().into_pointer_value();
        self.load_closure_captures(
            &mut symbol_table,
            captures_param,
            captures_tuple_ty,
            closure_ty,
            &node.captures,
        )?;

        for (idx, param) in node.params.iter().enumerate() {
            let llvm_param_ty = self.conv_to_llvm_type(&closure_ty.param_tys[idx]);

            let alloca = self.builder.build_alloca(llvm_param_ty, param.as_str())?;

            self.builder
                .build_store(alloca, closure_fn.get_nth_param(idx as u32 + 1).unwrap())?;

            symbol_table.insert(
                *param,
                Rc::new(RefCell::new(SymbolTableValue::Variable(alloca))),
            );
        }

        self.tcx.push_symbol_table(symbol_table);

        let body_val = self.build_expr(&node.body)?;

        if self.no_terminator() {
            match closure_ty.ret_ty.kind.as_ref() {
                TyKind::Unit => {
                    self.builder.build_return(None)?;
                }
                _ => {
                    if let Some(ret) = body_val {
                        self.builder.build_return(Some(&ret))?;
                    }
                }
            }
        }

        self.tcx.pop_symbol_table();

        self.fn_return_ty_stack.pop();

        self.builder.position_at_end(saved_block);

        let captures_tuple_ptr =
            self.build_closure_captures_tuple(&captures_tuple_ty, &node.captures)?;

        let closure_value = self.create_gc_struct(
            closure_struct_ty.as_basic_type_enum(),
            &[
                closure_fn
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum(),
                captures_tuple_ptr.into(),
            ],
        )?;

        Ok(Some(closure_value.into()))
    }

    fn load_closure_captures(
        &mut self,
        symbol_table: &mut SymbolTable<'ctx>,
        captures_ptr: PointerValue<'ctx>,
        captures_tuple_ty: StructType<'ctx>,
        closure_ty: &kaede_ir::ty::ClosureType,
        captures: &[Expr],
    ) -> anyhow::Result<()> {
        assert_eq!(closure_ty.captures.len(), captures.len());

        for (idx, capture) in captures.iter().enumerate() {
            let name = match &capture.kind {
                ExprKind::Variable(var) => var.name,
                _ => return Err(CodegenError::UnsupportedCaptureExpression.into()),
            };

            let capture_llvm_ty = self.conv_to_llvm_type(&closure_ty.captures[idx]);

            let gep = unsafe {
                self.builder.build_in_bounds_gep(
                    captures_tuple_ty,
                    captures_ptr,
                    &[
                        self.context().i32_type().const_zero(),
                        self.context().i32_type().const_int(idx as u64, false),
                    ],
                    "",
                )?
            };

            let loaded = self.builder.build_load(capture_llvm_ty, gep, "")?;
            let alloca = self.create_entry_block_alloca(name.as_str(), capture_llvm_ty)?;
            self.builder.build_store(alloca, loaded)?;

            symbol_table.insert(
                name,
                Rc::new(RefCell::new(SymbolTableValue::Variable(alloca))),
            );
        }

        Ok(())
    }

    fn build_closure_captures_tuple(
        &mut self,
        captures_tuple_ty: &StructType<'ctx>,
        captures: &[Expr],
    ) -> anyhow::Result<PointerValue<'ctx>> {
        let mut capture_values = Vec::with_capacity(captures.len());

        for capture in captures {
            let value = self
                .build_expr(capture)?
                .ok_or_else(|| anyhow::anyhow!("capture expression did not produce a value"))?;
            capture_values.push(value);
        }

        self.create_gc_struct(captures_tuple_ty.as_basic_type_enum(), &capture_values)
    }

    fn build_int(&self, int: &Int) -> anyhow::Result<Value<'ctx>> {
        use kaede_ir::expr::IntKind::*;

        match int.kind {
            I8(n) => Ok(Some(
                self.context().i8_type().const_int(n as u64, true).into(),
            )),

            U8(n) => Ok(Some(
                self.context().i8_type().const_int(n as u64, false).into(),
            )),

            I16(n) => Ok(Some(
                self.context().i16_type().const_int(n as u64, true).into(),
            )),

            U16(n) => Ok(Some(
                self.context().i16_type().const_int(n as u64, false).into(),
            )),

            I32(n) => Ok(Some(
                self.context().i32_type().const_int(n as u64, true).into(),
            )),

            U32(n) => Ok(Some(
                self.context().i32_type().const_int(n as u64, false).into(),
            )),

            I64(n) => Ok(Some(
                self.context().i64_type().const_int(n as u64, true).into(),
            )),

            U64(n) => Ok(Some(self.context().i64_type().const_int(n, false).into())),

            Infer(_) => Err(CodegenError::UnresolvedInferInt.into()),
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
                let mut value = self.build_expr(val)?.unwrap();

                if let Some(expected_ty) = self.fn_return_ty_stack.last().and_then(|ty| ty.clone())
                {
                    value = self.coerce_value_to_type(value, &val.ty, &expected_ty)?;
                }

                self.builder.build_return(Some(&value))?
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

        for (field_name, field_expr) in node.values.iter() {
            let field_info = &node
                .struct_info
                .fields
                .iter()
                .find(|f| f.name == *field_name)
                .unwrap();

            let value = self.build_expr(field_expr)?;
            let mut basic = value.unwrap();
            basic = self.coerce_value_to_type(basic, &field_expr.ty, &field_info.ty)?;

            // To sort by offset, store offset
            values.push((field_info.offset, basic));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));
        let values: Vec<_> = values.iter().map(|e| e.1).collect();

        let value = self.create_gc_struct(struct_llvm_ty.as_basic_type_enum(), &values)?;

        Ok(Some(value.into()))
    }

    fn build_byte_string_literal(
        &mut self,
        node: &ByteStringLiteral,
    ) -> anyhow::Result<Value<'ctx>> {
        let i8_ty = self.context().i8_type();
        let array_ty = i8_ty.array_type(node.bytes.len() as u32);

        let const_bytes = node
            .bytes
            .iter()
            .map(|b| i8_ty.const_int(*b as u64, false))
            .collect::<Vec<_>>();

        let array_val = i8_ty.const_array(&const_bytes);

        let global_name = self.fresh_literal_name("byte_str");
        let global = self.module.add_global(array_ty, None, global_name.as_str());
        global.set_initializer(&array_val);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);

        let elem_ty = Rc::new(make_fundamental_type(
            FundamentalTypeKind::U8,
            Mutability::Not,
        ));

        let slice_ptr = self.build_array_to_slice_conversion(
            global.as_pointer_value(),
            node.bytes.len() as u32,
            &elem_ty,
        )?;

        Ok(Some(slice_ptr))
    }

    fn build_string_literal(&mut self, node: &StringLiteral) -> anyhow::Result<Value<'ctx>> {
        let global_s = self
            .builder
            .build_global_string_ptr(node.syb.as_str(), "str")?;

        self.build_string_literal_internal(
            global_s.as_basic_value_enum(),
            self.context()
                .i64_type()
                .const_int(node.syb.as_str().len() as u64, false),
        )
    }

    fn build_string_literal_internal(
        &mut self,
        str: BasicValueEnum<'ctx>,
        len: IntValue<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        let str_llvm_ty = self.conv_to_llvm_type(&Rc::new(make_fundamental_type(
            FundamentalTypeKind::Str,
            Mutability::Not,
        )));

        let p = self.create_gc_struct(str_llvm_ty, &[str, len.as_basic_value_enum()])?;

        Ok(Some(p.into()))
    }

    fn build_char_literal(&mut self, node: &CharLiteral) -> anyhow::Result<Value<'ctx>> {
        let char_val = self.context().i8_type().const_int(node.ch as u64, false);
        Ok(Some(char_val.into()))
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
        let ptr = match &*self.tcx.lookup_symbol(node.name).unwrap().borrow() {
            SymbolTableValue::Variable(var) => *var,
            _ => unreachable!(),
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

            for (idx, arg) in node.args.0.iter().enumerate() {
                let mut value = self.build_expr(arg)?.unwrap();

                if let Some(param) = node.callee.params.get(idx) {
                    value = self.coerce_value_to_type(value, &arg.ty, &param.ty)?;
                }

                args.push(value.into());
            }

            args
        };

        let callee_result = self.callee_symbols(&node.callee).iter().find_map(|name| {
            self.tcx.lookup_symbol(*name).map(|symbol| {
                let value = symbol.borrow();
                match &*value {
                    SymbolTableValue::Function(fn_value) => (*name, Some(*fn_value)),
                    _ => (*name, None),
                }
            })
        });

        match callee_result {
            Some((_, Some(fn_value))) => Ok(self
                .builder
                .build_call(fn_value, args.as_slice(), "")?
                .try_as_basic_value()
                .left()),
            Some((mangled_name, None)) => self.build_closure_call(node, mangled_name, &args),
            None => Err(CodegenError::UnknownCallee {
                name: node.callee.name.symbol(),
            }
            .into()),
        }
    }

    fn build_fn_pointer(&mut self, node: &FnPointer, ty: &Rc<Ty>) -> anyhow::Result<Value<'ctx>> {
        let closure_ty = match ty.kind.as_ref() {
            TyKind::Closure(closure_ty) => closure_ty,
            _ => return Err(CodegenError::ExpectedClosureType.into()),
        };

        let fn_value = self
            .callee_symbols(&node.decl)
            .iter()
            .find_map(|name| match self.tcx.lookup_symbol(*name) {
                Some(value) => match &*value.borrow() {
                    SymbolTableValue::Function(fn_value) => Some(*fn_value),
                    _ => None,
                },
                None => None,
            })
            .ok_or(CodegenError::UnknownCallee {
                name: node.decl.name.symbol(),
            })?;

        assert!(closure_ty.captures.is_empty());
        let (closure_struct_ty, closure_fn_ty, _) = self.closure_llvm_types(closure_ty);

        let saved_block = self.builder.get_insert_block().unwrap();
        let wrapper_name = format!("fnptr_trampoline_{}", self.closure_counter);
        self.closure_counter += 1;
        let wrapper = self.module.add_function(
            wrapper_name.as_str(),
            closure_fn_ty,
            Some(Linkage::Internal),
        );
        let entry = self.context().append_basic_block(wrapper, "entry");
        self.builder.position_at_end(entry);

        let forwarded_args = wrapper
            .get_params()
            .iter()
            // Skip captures pointer (unused for plain functions)
            .skip(1)
            .map(|arg| (*arg).into())
            .collect::<Vec<_>>();

        let call = self
            .builder
            .build_call(fn_value, forwarded_args.as_slice(), "")?;
        if matches!(closure_ty.ret_ty.kind.as_ref(), TyKind::Unit) {
            self.builder.build_return(None)?;
        } else {
            let ret = call.try_as_basic_value().left().unwrap();
            self.builder.build_return(Some(&ret))?;
        }

        self.builder.position_at_end(saved_block);

        let fn_ptr = wrapper
            .as_global_value()
            .as_pointer_value()
            .as_basic_value_enum();

        let captures_ptr = self
            .context()
            .ptr_type(inkwell::AddressSpace::default())
            .const_null()
            .as_basic_value_enum();

        let value = self.create_gc_struct(
            closure_struct_ty.as_basic_type_enum(),
            &[fn_ptr, captures_ptr],
        )?;

        Ok(Some(value.into()))
    }

    fn callee_symbols(&self, callee: &kaede_ir::top::FnDecl) -> Vec<Symbol> {
        use kaede_common::LangLinkage;

        let mut names = match callee.lang_linkage {
            LangLinkage::Default => vec![callee.name.mangle()],
            LangLinkage::Rust => {
                vec![format!("{}{}", rust_function_prefix(), callee.name.symbol()).into()]
            }
            LangLinkage::C => vec![callee.name.symbol()],
        };

        let base_name = callee.name.symbol();
        if !names.contains(&base_name) {
            names.push(base_name);
        }

        names
    }

    fn build_closure_call(
        &mut self,
        node: &FnCall,
        mangled_name: Symbol,
        args: &[inkwell::values::BasicMetadataValueEnum<'ctx>],
    ) -> anyhow::Result<Value<'ctx>> {
        let callee_ptr = match self.tcx.lookup_symbol(mangled_name) {
            Some(value) => match &*value.borrow() {
                SymbolTableValue::Variable(var) => *var,
                _ => return Err(CodegenError::CalleeNotClosureValue { name: mangled_name }.into()),
            },

            None => {
                return Err(CodegenError::UnknownCallee {
                    name: node.callee.name.symbol(),
                }
                .into())
            }
        };

        let ptr_ty = self.context().ptr_type(inkwell::AddressSpace::default());
        let closure_struct_ty = self
            .context()
            .struct_type(&[ptr_ty.into(), ptr_ty.into()], true);

        let closure_ptr = self
            .builder
            .build_load(ptr_ty, callee_ptr, "")?
            .into_pointer_value();

        let fn_gep = unsafe {
            self.builder.build_in_bounds_gep(
                closure_struct_ty,
                closure_ptr,
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_zero(),
                ],
                "",
            )?
        };

        let captures_gep = unsafe {
            self.builder.build_in_bounds_gep(
                closure_struct_ty,
                closure_ptr,
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_int(1, false),
                ],
                "",
            )?
        };

        let fn_ptr = self.builder.build_load(ptr_ty, fn_gep, "")?;
        let captures_ptr = self.builder.build_load(ptr_ty, captures_gep, "")?;

        let mut call_param_types = Vec::with_capacity(node.callee.params.len() + 1);
        call_param_types.push(ptr_ty.into());
        for param in &node.callee.params {
            call_param_types.push(self.conv_to_llvm_type(&param.ty).into());
        }

        let closure_fn_ty = match &node.callee.return_ty {
            Some(ret) => self
                .conv_to_llvm_type(ret)
                .fn_type(call_param_types.as_slice(), false),
            None => self
                .context()
                .void_type()
                .fn_type(call_param_types.as_slice(), false),
        };

        #[allow(deprecated)]
        let fn_ptr_cast = self.builder.build_pointer_cast(
            fn_ptr.into_pointer_value(),
            closure_fn_ty.ptr_type(inkwell::AddressSpace::default()),
            "",
        )?;

        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(captures_ptr.into());
        call_args.extend_from_slice(args);

        Ok(self
            .builder
            .build_indirect_call(closure_fn_ty, fn_ptr_cast, call_args.as_slice(), "")?
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

    fn build_array_repeat(&mut self, node: &ArrayRepeat) -> anyhow::Result<Value<'ctx>> {
        let value = self.build_expr(&node.value)?.unwrap();

        let array_ty = Rc::new(Ty {
            kind: TyKind::Array((node.value.ty.clone(), node.count)).into(),
            mutability: Mutability::Not,
        });

        let array_llvm_ty = self.conv_to_llvm_type(&array_ty);

        let mallocd = self.gc_malloc(array_llvm_ty)?;

        for idx in 0..node.count {
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

            self.builder.build_store(gep, value)?;
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
            anyhow::bail!(
                "unsupported cast from `{}` to `{}`",
                value_ty.kind.to_string(),
                target_ty.kind.to_string()
            );
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

    // Build indexing for array and pointer and str
    fn build_indexing(&mut self, node: &Indexing) -> anyhow::Result<Value<'ctx>> {
        // A raw array cannot be passed, but a pointer(reference) to an array
        let ref_value = self.build_expr(&node.operand)?.unwrap();

        let refee_ty = match node.operand.ty.kind.as_ref() {
            TyKind::Reference(rty) => rty.get_base_type(),
            TyKind::Pointer(pty) => return self.build_pointer_indexing(node, ref_value, pty),
            _ => unreachable!(),
        };

        match refee_ty.kind.as_ref() {
            TyKind::Array(_) => {
                let array_llvm_ty = self.conv_to_llvm_type(&refee_ty).into_array_type();

                let ptr_to_array = ref_value.into_pointer_value();

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

                return Ok(Some(self.builder.build_load(
                    array_llvm_ty.get_element_type(),
                    gep,
                    "",
                )?));
            }
            TyKind::Slice(_) => {
                return self.build_slice_indexing(node, ref_value, refee_ty);
            }
            TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::Str,
                ..
            }) => {
                return self.build_string_indexing(node);
            }
            _ => {}
        }

        todo!("Error")
    }

    fn build_slicing(&mut self, node: &Slicing) -> anyhow::Result<Value<'ctx>> {
        let operand_value = self.build_expr(&node.operand)?.unwrap();
        let operand_ty = node.operand.ty.clone();

        let start_val = self.build_expr(&node.start)?.unwrap().into_int_value();
        let end_val = self.build_expr(&node.end)?.unwrap().into_int_value();

        let start_i64 = self
            .builder
            .build_int_cast(start_val, self.context().i64_type(), "")?;
        let end_i64 = self
            .builder
            .build_int_cast(end_val, self.context().i64_type(), "")?;

        let elem_llvm_ty = self.conv_to_llvm_type(&node.elem_ty);

        let data_ptr = match operand_ty.kind.as_ref() {
            TyKind::Reference(rty) => match rty.get_base_type().kind.as_ref() {
                TyKind::Array(_) => {
                    let array_llvm_ty = self
                        .conv_to_llvm_type(&rty.get_base_type())
                        .into_array_type();
                    let start_i32 =
                        self.builder
                            .build_int_cast(start_i64, self.context().i32_type(), "")?;

                    unsafe {
                        self.builder.build_in_bounds_gep(
                            array_llvm_ty,
                            operand_value.into_pointer_value(),
                            &[self.context().i32_type().const_zero(), start_i32],
                            "",
                        )?
                    }
                }
                TyKind::Slice(_) => {
                    let slice_llvm_ty = self
                        .conv_to_llvm_type(&rty.get_base_type())
                        .into_struct_type();

                    let data_gep = unsafe {
                        self.builder.build_in_bounds_gep(
                            slice_llvm_ty,
                            operand_value.into_pointer_value(),
                            &[
                                self.context().i32_type().const_zero(),
                                self.context().i32_type().const_zero(),
                            ],
                            "",
                        )?
                    };

                    let data_ptr = self.builder.build_load(
                        slice_llvm_ty.get_field_type_at_index(0).unwrap(),
                        data_gep,
                        "",
                    )?;

                    unsafe {
                        self.builder.build_in_bounds_gep(
                            elem_llvm_ty,
                            data_ptr.into_pointer_value(),
                            &[start_i64],
                            "",
                        )?
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let len_value = self
            .builder
            .build_int_sub(end_i64, start_i64, "")?
            .as_basic_value_enum();

        let slice_ty = Ty {
            kind: TyKind::Slice(node.elem_ty.clone()).into(),
            mutability: Mutability::Not,
        };
        let slice_llvm_ty = self.conv_to_llvm_type(&slice_ty);

        let slice_ptr =
            self.create_gc_struct(slice_llvm_ty, &[data_ptr.as_basic_value_enum(), len_value])?;

        Ok(Some(slice_ptr.as_basic_value_enum()))
    }

    fn build_pointer_indexing(
        &mut self,
        node: &Indexing,
        ptr_value: BasicValueEnum<'ctx>,
        pty: &PointerType,
    ) -> anyhow::Result<Value<'ctx>> {
        let pointee_llvm_ty = self.conv_to_llvm_type(&pty.pointee_ty);

        let index = self.build_expr(&node.index)?.unwrap();

        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                pointee_llvm_ty,
                ptr_value.into_pointer_value(),
                &[index.into_int_value()],
                "",
            )?
        };

        Ok(Some(self.builder.build_load(pointee_llvm_ty, gep, "")?))
    }

    fn build_slice_indexing(
        &mut self,
        node: &Indexing,
        slice_value: BasicValueEnum<'ctx>,
        slice_ty: Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        let slice_llvm_ty = self.conv_to_llvm_type(&slice_ty).into_struct_type();

        let data_gep = unsafe {
            self.builder.build_in_bounds_gep(
                slice_llvm_ty,
                slice_value.into_pointer_value(),
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_zero(),
                ],
                "",
            )?
        };

        let data_ptr = self.builder.build_load(
            slice_llvm_ty.get_field_type_at_index(0).unwrap(),
            data_gep,
            "",
        )?;

        let index = self.build_expr(&node.index)?.unwrap();
        let elem_ty = match slice_ty.kind.as_ref() {
            TyKind::Slice(elem_ty) => elem_ty.clone(),
            _ => unreachable!(),
        };
        let elem_llvm_ty = self.conv_to_llvm_type(&elem_ty);

        let elem_gep = unsafe {
            self.builder.build_in_bounds_gep(
                elem_llvm_ty,
                data_ptr.into_pointer_value(),
                &[index.into_int_value()],
                "",
            )?
        };

        Ok(Some(
            self.builder
                .build_load(elem_llvm_ty, elem_gep, "")?
                .as_basic_value_enum(),
        ))
    }

    fn build_string_indexing(&mut self, node: &Indexing) -> anyhow::Result<Value<'ctx>> {
        let string_ref_value = self.build_expr(&node.operand)?.unwrap();

        let string_llvm_ty = FundamentalType::create_llvm_str_type(self.context());

        let index = self.build_expr(&node.index)?.unwrap();

        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                string_llvm_ty,
                string_ref_value.into_pointer_value(),
                &[
                    self.context().i32_type().const_zero(),
                    self.context().i32_type().const_zero(),
                ],
                "",
            )?
        };

        let loaded = self.builder.build_load(
            string_llvm_ty
                .into_struct_type()
                .get_field_type_at_index(0)
                .unwrap(),
            gep,
            "",
        )?;

        let char_llvm_ty = self.conv_to_llvm_type(&make_fundamental_type(
            FundamentalTypeKind::Char,
            Mutability::Not,
        ));

        let char_gep = unsafe {
            self.builder.build_in_bounds_gep(
                char_llvm_ty,
                loaded.into_pointer_value(),
                &[index.into_int_value()],
                "",
            )?
        };

        Ok(Some(self.builder.build_load(char_llvm_ty, char_gep, "")?))
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
        self.build_tuple_indexing_internal(node)
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

    pub fn build_indexing_common(
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
