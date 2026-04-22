use std::{collections::HashMap, rc::Rc};

use kaede_ir::{self as ir, ty as ir_type};

pub(crate) struct GenericSubstituter<'a> {
    subst: &'a HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
}

impl<'a> GenericSubstituter<'a> {
    pub(crate) fn new(subst: &'a HashMap<ir_type::VarId, Rc<ir_type::Ty>>) -> Self {
        Self { subst }
    }

    fn apply_generic_instance(&self, generic_instance: &mut Option<ir_type::GenericInstanceInfo>) {
        if let Some(instance) = generic_instance {
            instance.args = instance.args.iter().map(|arg| self.apply_ty(arg)).collect();
        }
    }

    fn apply_ty(&self, ty: &Rc<ir_type::Ty>) -> Rc<ir_type::Ty> {
        ir_type::apply_type_var_bindings(ty, self.subst)
    }

    pub(crate) fn apply_fn_decl(&self, decl: &mut ir::top::FnDecl) {
        for param in &mut decl.params {
            param.ty = self.apply_ty(&param.ty);
            if let Some(default) = &mut param.default {
                self.apply_expr(Rc::make_mut(default));
            }
        }
        decl.return_ty = self.apply_ty(&decl.return_ty);
        self.apply_generic_instance(&mut decl.generic_instance);
    }

    pub(crate) fn apply_struct(&self, struct_: &mut ir::top::Struct) {
        for field in &mut struct_.fields {
            field.ty = self.apply_ty(&field.ty);
        }
        self.apply_generic_instance(&mut struct_.generic_instance);
    }

    pub(crate) fn apply_enum(&self, enum_: &mut ir::top::Enum) {
        for variant in &mut enum_.variants {
            if let Some(ty) = &variant.ty {
                variant.ty = Some(self.apply_ty(ty));
            }
        }
        self.apply_generic_instance(&mut enum_.generic_instance);
    }

    fn apply_stmt(&self, stmt: &mut ir::stmt::Stmt) {
        match stmt {
            ir::stmt::Stmt::Expr(expr) => self.apply_expr(Rc::make_mut(expr)),
            ir::stmt::Stmt::Let(let_stmt) => {
                let_stmt.ty = self.apply_ty(&let_stmt.ty);
                if let Some(init) = &mut let_stmt.init {
                    self.apply_expr(init);
                }
            }
            ir::stmt::Stmt::TupleUnpack(tuple_unpack) => {
                self.apply_expr(&mut tuple_unpack.init);
            }
            ir::stmt::Stmt::Assign(assign) => {
                self.apply_expr(&mut assign.assignee);
                self.apply_expr(&mut assign.value);
            }
        }
    }

    pub(crate) fn apply_block(&self, block: &mut ir::stmt::Block) {
        for stmt in &mut block.body {
            self.apply_stmt(stmt);
        }
        if let Some(last_expr) = &mut block.last_expr {
            self.apply_expr(last_expr);
        }
    }

    pub(crate) fn apply_expr(&self, expr: &mut ir::expr::Expr) {
        expr.ty = self.apply_ty(&expr.ty);

        match &mut expr.kind {
            ir::expr::ExprKind::Variable(var) => {
                var.ty = self.apply_ty(&var.ty);
            }
            ir::expr::ExprKind::ArrayLiteral(arr) => {
                for elem in &mut arr.elements {
                    self.apply_expr(elem);
                }
            }
            ir::expr::ExprKind::ArrayRepeat(rep) => {
                self.apply_expr(&mut rep.value);
            }
            ir::expr::ExprKind::TupleLiteral(tuple) => {
                for elem in &mut tuple.elements {
                    self.apply_expr(elem);
                }
            }
            ir::expr::ExprKind::StructLiteral(lit) => {
                for (_, value) in &mut lit.values {
                    self.apply_expr(value);
                }
                let mut struct_info = (*lit.struct_info).clone();
                self.apply_struct(&mut struct_info);
                lit.struct_info = Rc::new(struct_info);
            }
            ir::expr::ExprKind::Binary(bin) => {
                self.apply_expr(Rc::make_mut(&mut bin.lhs));
                self.apply_expr(Rc::make_mut(&mut bin.rhs));
            }
            ir::expr::ExprKind::Cast(cast) => {
                self.apply_expr(&mut cast.operand);
                cast.target_ty = self.apply_ty(&cast.target_ty);
            }
            ir::expr::ExprKind::FieldAccess(field) => {
                self.apply_expr(&mut field.operand);
                let mut struct_info = (*field.struct_info).clone();
                self.apply_struct(&mut struct_info);
                field.struct_info = Rc::new(struct_info);
            }
            ir::expr::ExprKind::UnresolvedFieldAccess(field) => {
                self.apply_expr(&mut field.operand);
            }
            ir::expr::ExprKind::TupleIndexing(tuple_idx) => {
                self.apply_expr(Rc::make_mut(&mut tuple_idx.tuple));
                tuple_idx.element_ty = self.apply_ty(&tuple_idx.element_ty);
            }
            ir::expr::ExprKind::EnumVariant(enum_var) => {
                if let Some(value) = &mut enum_var.value {
                    self.apply_expr(value);
                }
                let mut enum_info = (*enum_var.enum_info).clone();
                self.apply_enum(&mut enum_info);
                enum_var.enum_info = Rc::new(enum_info);
            }
            ir::expr::ExprKind::Indexing(idx) => {
                self.apply_expr(Rc::make_mut(&mut idx.operand));
                self.apply_expr(&mut idx.index);
            }
            ir::expr::ExprKind::Slicing(slicing) => {
                self.apply_expr(Rc::make_mut(&mut slicing.operand));
                self.apply_expr(&mut slicing.start);
                self.apply_expr(&mut slicing.end);
                slicing.elem_ty = self.apply_ty(&slicing.elem_ty);
            }
            ir::expr::ExprKind::LogicalNot(not) => {
                self.apply_expr(&mut not.operand);
            }
            ir::expr::ExprKind::BitNot(not) => {
                self.apply_expr(&mut not.operand);
            }
            ir::expr::ExprKind::FnCall(call) => {
                for arg in &mut call.args.0 {
                    self.apply_expr(arg);
                }
                let mut callee = (*call.callee).clone();
                self.apply_fn_decl(&mut callee);
                call.callee = Rc::new(callee);
            }
            ir::expr::ExprKind::GenericFnCall(call) => {
                for arg in &mut call.args.0 {
                    self.apply_expr(arg);
                }
                let mut callee = (*call.callee).clone();
                self.apply_fn_decl(&mut callee);
                call.callee = Rc::new(callee);
                call.generic_args = call
                    .generic_args
                    .iter()
                    .map(|arg| self.apply_ty(arg))
                    .collect();
            }
            ir::expr::ExprKind::Spawn(spawn) => {
                for arg in &mut spawn.args {
                    self.apply_expr(arg);
                }
                let mut callee = (*spawn.callee).clone();
                self.apply_fn_decl(&mut callee);
                spawn.callee = Rc::new(callee);
                spawn.arg_types = spawn.arg_types.iter().map(|ty| self.apply_ty(ty)).collect();
            }
            ir::expr::ExprKind::FnPointer(fn_ptr) => {
                let mut decl = (*fn_ptr.decl).clone();
                self.apply_fn_decl(&mut decl);
                fn_ptr.decl = Rc::new(decl);
            }
            ir::expr::ExprKind::Closure(closure) => {
                for capture in &mut closure.captures {
                    self.apply_expr(capture);
                }
                self.apply_expr(&mut closure.body);
            }
            ir::expr::ExprKind::Return(ret) => {
                if let Some(value) = ret {
                    self.apply_expr(value);
                }
            }
            ir::expr::ExprKind::If(if_expr) => {
                self.apply_expr(&mut if_expr.cond);
                self.apply_expr(&mut if_expr.then);
                if let Some(else_) = &mut if_expr.else_ {
                    match else_.as_mut() {
                        ir::expr::Else::If(if_) => {
                            let mut wrapped = ir::expr::Expr {
                                kind: ir::expr::ExprKind::If(if_.clone()),
                                ty: if_.then.ty.clone(),
                                span: if_.span,
                            };
                            self.apply_expr(&mut wrapped);
                            if let ir::expr::ExprKind::If(updated_if) = wrapped.kind {
                                *if_ = updated_if;
                            } else {
                                unreachable!()
                            }
                        }
                        ir::expr::Else::Block(block) => {
                            self.apply_expr(block);
                        }
                    }
                }
                if let Some(enum_unpack) = &mut if_expr.enum_unpack {
                    self.apply_expr(Rc::make_mut(&mut enum_unpack.enum_value));
                    if let Some(instance) = &enum_unpack.enum_ty.generic_instance {
                        enum_unpack.enum_ty = ir_type::UserDefinedType {
                            kind: enum_unpack.enum_ty.kind.clone(),
                            generic_instance: Some(ir_type::GenericInstanceInfo::new(
                                instance.origin.clone(),
                                instance.args.iter().map(|arg| self.apply_ty(arg)).collect(),
                            )),
                        };
                    }
                    enum_unpack.variant_ty = self.apply_ty(&enum_unpack.variant_ty);
                }
            }
            ir::expr::ExprKind::Loop(loop_expr) => {
                self.apply_block(&mut loop_expr.body);
            }
            ir::expr::ExprKind::Block(block) => {
                self.apply_block(block);
            }
            ir::expr::ExprKind::BuiltinFnCall(call) => {
                for arg in &mut call.args.0 {
                    self.apply_expr(arg);
                }
            }
            ir::expr::ExprKind::InterfaceBox(boxed) => {
                self.apply_expr(&mut boxed.value);
                let mut itable = (*boxed.itable).clone();
                itable.concrete_ty = self.apply_ty(&itable.concrete_ty);
                for method in &mut itable.methods {
                    let mut decl = (**method).clone();
                    self.apply_fn_decl(&mut decl);
                    *method = Rc::new(decl);
                }
                boxed.itable = Rc::new(itable);
            }
            ir::expr::ExprKind::InterfaceMethodCall(call) => {
                self.apply_expr(&mut call.receiver);
                for arg in &mut call.args.0 {
                    self.apply_expr(arg);
                }
                for param in &mut call.method.params {
                    param.ty = self.apply_ty(&param.ty);
                }
                call.method.return_ty = self.apply_ty(&call.method.return_ty);
            }
            ir::expr::ExprKind::Int(_)
            | ir::expr::ExprKind::Float(_)
            | ir::expr::ExprKind::StringLiteral(_)
            | ir::expr::ExprKind::ByteStringLiteral(_)
            | ir::expr::ExprKind::ByteLiteral(_)
            | ir::expr::ExprKind::CharLiteral(_)
            | ir::expr::ExprKind::BooleanLiteral(_)
            | ir::expr::ExprKind::Break => {}
        }
    }
}
