//! Mutable traversal of function bodies. A visitor overrides the hooks it cares about and calls
//! the matching `walk_*` function to continue into children; every type stored in the IR is
//! passed to [`MutVisitor::visit_ty`]. Within a node, child expressions are visited before the
//! types the node itself stores.
//!
//! Declarations shared with other nodes (callees, interface tables, and struct and enum
//! declarations embedded in expressions) are reached through their own hooks, so a visitor that
//! leaves them alone can override the hook instead of copying the declaration at every use.

use std::rc::Rc;

use crate::{
    expr::ITable,
    expr::{BuiltinFnCallKind, Else, Expr, ExprKind, If, SelectArm},
    stmt::{Block, Stmt},
    top::{Enum, FnDecl, InterfaceMethod, Struct},
    ty::{Mutability, Ty, TyKind, UserDefinedType, UserDefinedTypeKind},
};

pub trait MutVisitor {
    type Error;

    fn visit_ty(&mut self, _ty: &mut Rc<Ty>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<(), Self::Error> {
        walk_expr(self, expr)
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Error> {
        walk_stmt(self, stmt)
    }

    fn visit_block(&mut self, block: &mut Block) -> Result<(), Self::Error> {
        walk_block(self, block)
    }

    fn visit_fn_decl(&mut self, decl: &mut Rc<FnDecl>) -> Result<(), Self::Error> {
        walk_fn_decl(self, Rc::make_mut(decl))
    }

    fn visit_itable(&mut self, itable: &mut Rc<ITable>) -> Result<(), Self::Error> {
        let itable = Rc::make_mut(itable);
        self.visit_ty(&mut itable.concrete_ty)?;
        for method in &mut itable.methods {
            self.visit_fn_decl(method)?;
        }
        Ok(())
    }

    /// A struct or enum declaration embedded in an expression, visited as the type it declares
    /// so that it changes exactly as a type naming it would.
    fn visit_user_defined_type(&mut self, udt: &mut UserDefinedType) -> Result<(), Self::Error> {
        let mut ty = Rc::new(Ty {
            kind: TyKind::UserDefined(udt.clone()).into(),
            mutability: Mutability::Not,
        });
        self.visit_ty(&mut ty)?;
        if let TyKind::UserDefined(visited) = ty.kind.as_ref() {
            *udt = visited.clone();
        }
        Ok(())
    }
}

pub fn walk_block<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    block: &mut Block,
) -> Result<(), V::Error> {
    for stmt in &mut block.body {
        visitor.visit_stmt(stmt)?;
    }
    if let Some(last_expr) = &mut block.last_expr {
        visitor.visit_expr(last_expr)?;
    }
    Ok(())
}

pub fn walk_stmt<V: MutVisitor + ?Sized>(visitor: &mut V, stmt: &mut Stmt) -> Result<(), V::Error> {
    match stmt {
        Stmt::Expr(expr) => visitor.visit_expr(Rc::make_mut(expr)),
        Stmt::Let(let_) => {
            if let Some(init) = &mut let_.init {
                visitor.visit_expr(init)?;
            }
            visitor.visit_ty(&mut let_.ty)
        }
        Stmt::TupleUnpack(unpack) => visitor.visit_expr(&mut unpack.init),
        Stmt::Assign(assign) => {
            visitor.visit_expr(&mut assign.assignee)?;
            visitor.visit_expr(&mut assign.value)
        }
    }
}

pub fn walk_fn_decl<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    decl: &mut FnDecl,
) -> Result<(), V::Error> {
    for param in &mut decl.params {
        if let Some(default) = &mut param.default {
            visitor.visit_expr(Rc::make_mut(default))?;
        }
        visitor.visit_ty(&mut param.ty)?;
    }
    visitor.visit_ty(&mut decl.return_ty)?;
    if let Some(instance) = &mut decl.generic_instance {
        for arg in &mut instance.args {
            visitor.visit_ty(arg)?;
        }
    }
    Ok(())
}

pub fn walk_expr<V: MutVisitor + ?Sized>(visitor: &mut V, expr: &mut Expr) -> Result<(), V::Error> {
    match &mut expr.kind {
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::StringLiteral(_)
        | ExprKind::ByteStringLiteral(_)
        | ExprKind::ByteLiteral(_)
        | ExprKind::CharLiteral(_)
        | ExprKind::BooleanLiteral(_)
        | ExprKind::Break => {}
        ExprKind::StructLiteral(literal) => {
            for (_, value) in &mut literal.values {
                visitor.visit_expr(value)?;
            }
            visit_struct_info(visitor, &mut literal.struct_info)?;
        }
        ExprKind::ArrayLiteral(array) => {
            for element in &mut array.elements {
                visitor.visit_expr(element)?;
            }
        }
        ExprKind::ArrayRepeat(repeat) => visitor.visit_expr(&mut repeat.value)?,
        ExprKind::TupleLiteral(tuple) => {
            for element in &mut tuple.elements {
                visitor.visit_expr(element)?;
            }
        }
        ExprKind::Variable(variable) => visitor.visit_ty(&mut variable.ty)?,
        ExprKind::Binary(binary) => {
            visitor.visit_expr(Rc::make_mut(&mut binary.lhs))?;
            visitor.visit_expr(Rc::make_mut(&mut binary.rhs))?;
        }
        ExprKind::Cast(cast) => {
            visitor.visit_expr(&mut cast.operand)?;
            visitor.visit_ty(&mut cast.target_ty)?;
        }
        ExprKind::FieldAccess(access) => {
            visitor.visit_expr(&mut access.operand)?;
            visit_struct_info(visitor, &mut access.struct_info)?;
        }
        ExprKind::UnresolvedFieldAccess(access) => visitor.visit_expr(&mut access.operand)?,
        ExprKind::TupleIndexing(indexing) => {
            visitor.visit_expr(Rc::make_mut(&mut indexing.tuple))?;
            visitor.visit_ty(&mut indexing.element_ty)?;
        }
        ExprKind::EnumVariant(variant) => {
            if let Some(value) = &mut variant.value {
                visitor.visit_expr(value)?;
            }
            visit_enum_info(visitor, &mut variant.enum_info)?;
        }
        ExprKind::Indexing(indexing) => {
            visitor.visit_expr(Rc::make_mut(&mut indexing.operand))?;
            visitor.visit_expr(&mut indexing.index)?;
        }
        ExprKind::Slicing(slicing) => {
            visitor.visit_expr(Rc::make_mut(&mut slicing.operand))?;
            visitor.visit_expr(&mut slicing.start)?;
            visitor.visit_expr(&mut slicing.end)?;
            visitor.visit_ty(&mut slicing.elem_ty)?;
        }
        ExprKind::LogicalNot(not) => visitor.visit_expr(&mut not.operand)?,
        ExprKind::BitNot(not) => visitor.visit_expr(&mut not.operand)?,
        ExprKind::FnCall(call) => {
            for arg in &mut call.args.0 {
                visitor.visit_expr(arg)?;
            }
            visitor.visit_fn_decl(&mut call.callee)?;
        }
        ExprKind::GenericFnCall(call) => {
            for arg in &mut call.args.0 {
                visitor.visit_expr(arg)?;
            }
            visitor.visit_fn_decl(&mut call.callee)?;
            for arg in &mut call.generic_args {
                visitor.visit_ty(arg)?;
            }
        }
        ExprKind::Spawn(spawn) => {
            for arg in &mut spawn.args {
                visitor.visit_expr(arg)?;
            }
            visitor.visit_fn_decl(&mut spawn.callee)?;
            for arg_ty in &mut spawn.arg_types {
                visitor.visit_ty(arg_ty)?;
            }
        }
        ExprKind::FnPointer(pointer) => visitor.visit_fn_decl(&mut pointer.decl)?,
        ExprKind::Closure(closure) => {
            for capture in &mut closure.captures {
                visitor.visit_expr(capture)?;
            }
            visitor.visit_expr(&mut closure.body)?;
        }
        ExprKind::Return(value) => {
            if let Some(value) = value {
                visitor.visit_expr(value)?;
            }
        }
        ExprKind::If(if_) => walk_if(visitor, if_)?,
        ExprKind::Loop(loop_) => visitor.visit_block(&mut loop_.body)?,
        ExprKind::Block(block) => visitor.visit_block(block)?,
        ExprKind::BuiltinFnCall(call) => {
            if let BuiltinFnCallKind::TypeSize(ty) = &mut call.kind {
                visitor.visit_ty(ty)?;
            }
            for arg in &mut call.args.0 {
                visitor.visit_expr(arg)?;
            }
        }
        ExprKind::InterfaceBox(interface_box) => {
            visitor.visit_expr(&mut interface_box.value)?;
            visitor.visit_itable(&mut interface_box.itable)?;
        }
        ExprKind::InterfaceMethodCall(call) => {
            visitor.visit_expr(&mut call.receiver)?;
            walk_interface_method(visitor, &mut call.method)?;
            for arg in &mut call.args.0 {
                visitor.visit_expr(arg)?;
            }
        }
        ExprKind::Select(select) => {
            for arm in &mut select.arms {
                walk_select_arm(visitor, arm)?;
            }
            if let Some(default) = &mut select.default {
                visitor.visit_expr(default)?;
            }
        }
    }

    visitor.visit_ty(&mut expr.ty)
}

fn walk_if<V: MutVisitor + ?Sized>(visitor: &mut V, if_: &mut If) -> Result<(), V::Error> {
    visitor.visit_expr(&mut if_.cond)?;
    visitor.visit_expr(&mut if_.then)?;
    if let Some(else_) = &mut if_.else_ {
        match else_.as_mut() {
            Else::If(nested) => walk_if(visitor, nested)?,
            Else::Block(block) => visitor.visit_expr(block)?,
        }
    }
    if let Some(unpack) = &mut if_.enum_unpack {
        visitor.visit_expr(Rc::make_mut(&mut unpack.enum_value))?;
        visitor.visit_user_defined_type(&mut unpack.enum_ty)?;
        visitor.visit_ty(&mut unpack.variant_ty)?;
    }
    Ok(())
}

fn walk_interface_method<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    method: &mut InterfaceMethod,
) -> Result<(), V::Error> {
    for param in &mut method.params {
        visitor.visit_ty(&mut param.ty)?;
    }
    visitor.visit_ty(&mut method.return_ty)
}

fn walk_select_arm<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    arm: &mut SelectArm,
) -> Result<(), V::Error> {
    visitor.visit_expr(&mut arm.channel)?;
    if let Some(value) = &mut arm.value {
        visitor.visit_expr(value)?;
    }
    visitor.visit_expr(&mut arm.body)?;
    visitor.visit_ty(&mut arm.elem_ty)?;
    if let Some(option_ty) = &mut arm.option_ty {
        visitor.visit_ty(option_ty)?;
    }
    if let Some(option_enum) = &mut arm.option_enum_info {
        visit_enum_info(visitor, option_enum)?;
    }
    Ok(())
}

fn visit_struct_info<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    info: &mut Rc<Struct>,
) -> Result<(), V::Error> {
    let mut udt = UserDefinedType {
        kind: UserDefinedTypeKind::Struct(info.clone()),
        generic_instance: info.generic_instance.clone(),
    };
    visitor.visit_user_defined_type(&mut udt)?;
    if let UserDefinedTypeKind::Struct(visited) = udt.kind {
        *info = visited;
    }
    Ok(())
}

fn visit_enum_info<V: MutVisitor + ?Sized>(
    visitor: &mut V,
    info: &mut Rc<Enum>,
) -> Result<(), V::Error> {
    let mut udt = UserDefinedType {
        kind: UserDefinedTypeKind::Enum(info.clone()),
        generic_instance: info.generic_instance.clone(),
    };
    visitor.visit_user_defined_type(&mut udt)?;
    if let UserDefinedTypeKind::Enum(visited) = udt.kind {
        *info = visited;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use kaede_span::Span;
    use kaede_symbol::Symbol;

    use super::{walk_expr, MutVisitor};
    use crate::{
        expr::{EnumVariant, Expr, ExprKind, Int, IntKind},
        module_path::ModulePath,
        qualified_symbol::QualifiedSymbol,
        top::{Enum, EnumVariant as Variant},
        ty::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind},
    };

    /// Records each visited type, in visit order.
    #[derive(Default)]
    struct Recorder(Vec<String>);

    impl MutVisitor for Recorder {
        type Error = ();

        fn visit_ty(&mut self, ty: &mut Rc<Ty>) -> Result<(), ()> {
            self.0.push(match ty.kind.as_ref() {
                TyKind::UserDefined(udt) => udt.name().to_string(),
                kind => kind.to_string(),
            });
            Ok(())
        }
    }

    fn ty(kind: FundamentalTypeKind) -> Rc<Ty> {
        Rc::new(make_fundamental_type(kind, Mutability::Not))
    }

    #[test]
    fn visits_child_expressions_before_the_types_a_node_stores() {
        let value = Expr {
            kind: ExprKind::Int(Int {
                kind: IntKind::I8(1),
                span: Span::dummy(),
            }),
            ty: ty(FundamentalTypeKind::I8),
            span: Span::dummy(),
        };
        let enum_info = Rc::new(Enum {
            name: QualifiedSymbol::new(ModulePath::root(), Symbol::from("E".to_owned())),
            variants: vec![Variant {
                name: Symbol::from("V".to_owned()),
                ty: Some(ty(FundamentalTypeKind::I64)),
                offset: 0,
            }],
            generic_instance: None,
        });
        let mut expr = Expr {
            kind: ExprKind::EnumVariant(EnumVariant {
                enum_info,
                variant_offset: 0,
                value: Some(Box::new(value)),
                span: Span::dummy(),
            }),
            ty: ty(FundamentalTypeKind::Bool),
            span: Span::dummy(),
        };

        let mut recorder = Recorder::default();
        walk_expr(&mut recorder, &mut expr).unwrap();

        assert_eq!(recorder.0, ["i8", "E", "bool"]);
    }
}
