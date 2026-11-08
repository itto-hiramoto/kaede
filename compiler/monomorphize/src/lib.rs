use std::rc::Rc;

use anyhow::anyhow;
use kaede_ir::{
    expr::{BuiltinFnCallKind, Expr, ExprKind, FnCall, ITable},
    top::{FnDecl, TopLevel},
    ty::{contains_infer, UserDefinedType},
    visit::{walk_expr, MutVisitor},
    CompileUnit,
};

pub struct Monomorphizer;

impl Default for Monomorphizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use kaede_common::LangLinkage;
    use kaede_ir::{
        expr::{Args, Expr, ExprKind, GenericFnCall, ITable, InterfaceBox, InterfaceMethodCall},
        module_path::ModulePath,
        qualified_symbol::QualifiedSymbol,
        stmt::{Block, Stmt},
        top::{Fn, FnDecl, Impl, Interface, InterfaceMethod, TopLevel},
        ty::Ty,
        CompileUnit,
    };
    use kaede_span::Span;
    use kaede_symbol::Symbol;

    use super::Monomorphizer;

    fn unit_ty() -> Rc<Ty> {
        Rc::new(Ty::new_unit())
    }

    fn fn_decl(name: &str) -> Rc<FnDecl> {
        Rc::new(FnDecl {
            lang_linkage: LangLinkage::Default,
            link_once: false,
            name: QualifiedSymbol::new(ModulePath::root(), Symbol::from(name.to_owned())),
            params: vec![],
            is_c_variadic: false,
            return_ty: unit_ty(),
            generic_instance: None,
        })
    }

    fn generic_call_expr(name: &str) -> Expr {
        Expr {
            kind: ExprKind::GenericFnCall(GenericFnCall {
                callee: fn_decl(name),
                generic_args: vec![unit_ty()],
                args: Args(vec![], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        }
    }

    fn contains_generic_call(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::GenericFnCall(_) => true,
            ExprKind::FnCall(call) => call.args.0.iter().any(contains_generic_call),
            ExprKind::Spawn(spawn) => spawn.args.iter().any(contains_generic_call),
            ExprKind::ArrayLiteral(arr) => arr.elements.iter().any(contains_generic_call),
            ExprKind::ArrayRepeat(rep) => contains_generic_call(&rep.value),
            ExprKind::TupleLiteral(tuple) => tuple.elements.iter().any(contains_generic_call),
            ExprKind::Binary(bin) => {
                contains_generic_call(&bin.lhs) || contains_generic_call(&bin.rhs)
            }
            ExprKind::Cast(cast) => contains_generic_call(&cast.operand),
            ExprKind::FieldAccess(field) => contains_generic_call(&field.operand),
            ExprKind::UnresolvedFieldAccess(field) => contains_generic_call(&field.operand),
            ExprKind::TupleIndexing(tuple_idx) => contains_generic_call(&tuple_idx.tuple),
            ExprKind::EnumVariant(enum_var) => {
                enum_var.value.iter().any(|e| contains_generic_call(e))
            }
            ExprKind::Indexing(idx) => {
                contains_generic_call(&idx.operand) || contains_generic_call(&idx.index)
            }
            ExprKind::Slicing(slicing) => {
                contains_generic_call(&slicing.operand)
                    || contains_generic_call(&slicing.start)
                    || contains_generic_call(&slicing.end)
            }
            ExprKind::LogicalNot(not) => contains_generic_call(&not.operand),
            ExprKind::BitNot(not) => contains_generic_call(&not.operand),
            ExprKind::Closure(closure) => {
                closure.captures.iter().any(contains_generic_call)
                    || contains_generic_call(&closure.body)
            }
            ExprKind::Return(ret) => ret.as_ref().is_some_and(|e| contains_generic_call(e)),
            ExprKind::If(if_expr) => {
                contains_generic_call(&if_expr.cond)
                    || contains_generic_call(&if_expr.then)
                    || if_expr
                        .else_
                        .as_ref()
                        .is_some_and(|else_| match else_.as_ref() {
                            kaede_ir::expr::Else::If(inner) => contains_generic_call(&Expr {
                                kind: ExprKind::If(inner.clone()),
                                ty: unit_ty(),
                                span: Span::dummy(),
                            }),
                            kaede_ir::expr::Else::Block(block) => contains_generic_call(block),
                        })
                    || if_expr
                        .enum_unpack
                        .as_ref()
                        .is_some_and(|u| contains_generic_call(&u.enum_value))
            }
            ExprKind::Loop(loop_expr) => {
                loop_expr.body.body.iter().any(|stmt| match stmt {
                    Stmt::Expr(expr) => contains_generic_call(expr),
                    Stmt::Let(let_stmt) => {
                        let_stmt.init.as_ref().is_some_and(contains_generic_call)
                    }
                    Stmt::TupleUnpack(t) => contains_generic_call(&t.init),
                    Stmt::Assign(a) => {
                        contains_generic_call(&a.assignee) || contains_generic_call(&a.value)
                    }
                }) || loop_expr
                    .body
                    .last_expr
                    .as_ref()
                    .is_some_and(|e| contains_generic_call(e))
            }
            ExprKind::Block(block) => {
                block.body.iter().any(|stmt| match stmt {
                    Stmt::Expr(expr) => contains_generic_call(expr),
                    Stmt::Let(let_stmt) => {
                        let_stmt.init.as_ref().is_some_and(contains_generic_call)
                    }
                    Stmt::TupleUnpack(t) => contains_generic_call(&t.init),
                    Stmt::Assign(a) => {
                        contains_generic_call(&a.assignee) || contains_generic_call(&a.value)
                    }
                }) || block
                    .last_expr
                    .as_ref()
                    .is_some_and(|e| contains_generic_call(e))
            }
            ExprKind::BuiltinFnCall(call) => call.args.0.iter().any(contains_generic_call),
            ExprKind::StructLiteral(lit) => {
                lit.values.iter().any(|(_, v)| contains_generic_call(v))
            }
            ExprKind::InterfaceBox(boxed) => contains_generic_call(&boxed.value),
            ExprKind::InterfaceMethodCall(call) => {
                contains_generic_call(&call.receiver)
                    || call.args.0.iter().any(contains_generic_call)
            }
            ExprKind::Select(select) => {
                select.arms.iter().any(|arm| {
                    contains_generic_call(&arm.channel)
                        || arm.value.as_ref().is_some_and(|v| contains_generic_call(v))
                        || contains_generic_call(&arm.body)
                }) || select
                    .default
                    .as_ref()
                    .is_some_and(|d| contains_generic_call(d))
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::StringLiteral(_)
            | ExprKind::ByteStringLiteral(_)
            | ExprKind::ByteLiteral(_)
            | ExprKind::CharLiteral(_)
            | ExprKind::Variable(_)
            | ExprKind::BooleanLiteral(_)
            | ExprKind::FnPointer(_)
            | ExprKind::Break => false,
        }
    }

    #[test]
    fn rewrites_generic_call_in_function_body() {
        let main_fn = Rc::new(Fn {
            decl: (*fn_decl("main")).clone(),
            body: Some(Block {
                body: vec![],
                last_expr: Some(Box::new(generic_call_expr("foo_i32"))),
                span: Span::dummy(),
            }),
        });

        let mut unit = CompileUnit {
            top_levels: vec![TopLevel::Fn(main_fn)],
        };

        Monomorphizer::new().run(&mut unit).unwrap();

        let TopLevel::Fn(main_fn) = &unit.top_levels[0] else {
            panic!("expected top-level fn");
        };

        let body = main_fn.body.as_ref().expect("expected body");
        let expr = body.last_expr.as_ref().expect("expected last expr");
        assert!(matches!(expr.kind, ExprKind::FnCall(_)));
        assert!(!contains_generic_call(expr));
    }

    fn dummy_interface(name: &str) -> Rc<Interface> {
        Rc::new(Interface {
            name: QualifiedSymbol::new(ModulePath::root(), Symbol::from(name.to_owned())),
            methods: vec![InterfaceMethod {
                name: Symbol::from("m".to_owned()),
                self_: None,
                params: vec![],
                return_ty: unit_ty(),
            }],
        })
    }

    #[test]
    fn rewrites_generic_call_inside_interface_box() {
        let iface = dummy_interface("Iface");
        let itable = Rc::new(ITable {
            interface: iface.clone(),
            concrete_ty: unit_ty(),
            methods: vec![fn_decl("Impl::m")],
        });
        let boxed = Expr {
            kind: ExprKind::InterfaceBox(InterfaceBox {
                value: Box::new(generic_call_expr("inner_i32")),
                itable,
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        };

        let main_fn = Rc::new(Fn {
            decl: (*fn_decl("main")).clone(),
            body: Some(Block {
                body: vec![],
                last_expr: Some(Box::new(boxed)),
                span: Span::dummy(),
            }),
        });

        let mut unit = CompileUnit {
            top_levels: vec![TopLevel::Fn(main_fn)],
        };

        Monomorphizer::new().run(&mut unit).unwrap();

        let TopLevel::Fn(main_fn) = &unit.top_levels[0] else {
            panic!("expected top-level fn");
        };
        let body = main_fn.body.as_ref().expect("expected body");
        let expr = body.last_expr.as_ref().expect("expected last expr");
        assert!(!contains_generic_call(expr));
    }

    #[test]
    fn rewrites_generic_call_inside_interface_method_call() {
        let iface = dummy_interface("Iface");
        let receiver = Expr {
            kind: ExprKind::Variable(kaede_ir::expr::Variable {
                name: Symbol::from("r".to_owned()),
                ty: unit_ty(),
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        };
        let call = Expr {
            kind: ExprKind::InterfaceMethodCall(InterfaceMethodCall {
                receiver: Box::new(receiver),
                method_index: 0,
                method: iface.methods[0].clone(),
                args: Args(vec![generic_call_expr("arg_i32")], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        };

        let main_fn = Rc::new(Fn {
            decl: (*fn_decl("main")).clone(),
            body: Some(Block {
                body: vec![],
                last_expr: Some(Box::new(call)),
                span: Span::dummy(),
            }),
        });

        let mut unit = CompileUnit {
            top_levels: vec![TopLevel::Fn(main_fn)],
        };

        Monomorphizer::new().run(&mut unit).unwrap();

        let TopLevel::Fn(main_fn) = &unit.top_levels[0] else {
            panic!("expected top-level fn");
        };
        let body = main_fn.body.as_ref().expect("expected body");
        let expr = body.last_expr.as_ref().expect("expected last expr");
        assert!(!contains_generic_call(expr));
    }

    #[test]
    fn rewrites_generic_call_in_impl_method_body() {
        let method = Rc::new(Fn {
            decl: (*fn_decl("Type::m")).clone(),
            body: Some(Block {
                body: vec![Stmt::Expr(Rc::new(generic_call_expr("Type_i32::new")))],
                last_expr: None,
                span: Span::dummy(),
            }),
        });

        let mut unit = CompileUnit {
            top_levels: vec![TopLevel::Impl(Rc::new(Impl {
                methods: vec![method],
            }))],
        };

        Monomorphizer::new().run(&mut unit).unwrap();

        let TopLevel::Impl(impl_) = &unit.top_levels[0] else {
            panic!("expected top-level impl");
        };

        let body = impl_.methods[0]
            .body
            .as_ref()
            .expect("expected method body");
        let Stmt::Expr(expr) = &body.body[0] else {
            panic!("expected expr stmt");
        };
        assert!(matches!(expr.kind, ExprKind::FnCall(_)));
        assert!(!contains_generic_call(expr));
    }
}

impl Monomorphizer {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&mut self, compile_unit: &mut CompileUnit) -> anyhow::Result<()> {
        for top in &mut compile_unit.top_levels {
            match top {
                TopLevel::Fn(fn_) => {
                    if let Some(body) = &mut Rc::make_mut(fn_).body {
                        self.visit_block(body)?;
                    }
                }
                TopLevel::Impl(impl_) => {
                    for method in &mut Rc::make_mut(impl_).methods {
                        if let Some(body) = &mut Rc::make_mut(method).body {
                            self.visit_block(body)?;
                        }
                    }
                }
                TopLevel::Struct(_) | TopLevel::Enum(_) | TopLevel::Interface(_) => {}
            }
        }
        Ok(())
    }
}

/// Only function bodies are lowered; the declarations they reference are left as they are.
impl MutVisitor for Monomorphizer {
    type Error = anyhow::Error;

    fn visit_fn_decl(&mut self, _decl: &mut Rc<FnDecl>) -> anyhow::Result<()> {
        Ok(())
    }

    fn visit_itable(&mut self, _itable: &mut Rc<ITable>) -> anyhow::Result<()> {
        Ok(())
    }

    fn visit_user_defined_type(&mut self, _udt: &mut UserDefinedType) -> anyhow::Result<()> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> anyhow::Result<()> {
        walk_expr(self, expr)?;

        match &mut expr.kind {
            ExprKind::GenericFnCall(generic_call) => {
                if generic_call.generic_args.iter().any(contains_infer) {
                    return Err(anyhow!(
                        "unresolved generic arguments at monomorphize: {:?}",
                        generic_call.callee.name
                    ));
                }

                expr.kind = ExprKind::FnCall(FnCall {
                    callee: generic_call.callee.clone(),
                    args: generic_call.args.clone(),
                    span: generic_call.span,
                });
            }
            ExprKind::UnresolvedFieldAccess(field) => {
                return Err(anyhow!(
                    "unresolved field access reached monomorphize: {}",
                    field.field_name
                ));
            }
            ExprKind::BuiltinFnCall(call) => {
                if let BuiltinFnCallKind::TypeSize(ty) = &call.kind {
                    if contains_infer(ty) {
                        return Err(anyhow!(
                            "unresolved generic arguments at monomorphize: __type_size"
                        ));
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }
}
