use std::rc::Rc;

use anyhow::anyhow;
use kaede_ir::{
    expr::{Else, Expr, ExprKind, FnCall, If},
    stmt::{Block, Stmt},
    top::TopLevel,
    ty::contains_type_var,
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
        expr::{Args, Expr, ExprKind, GenericFnCall},
        module_path::ModulePath,
        qualified_symbol::QualifiedSymbol,
        stmt::{Block, Stmt},
        top::{Fn, FnDecl, Impl, TopLevel},
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
            name: QualifiedSymbol::new(ModulePath::new(vec![]), Symbol::from(name.to_owned())),
            params: vec![],
            is_c_variadic: false,
            return_ty: unit_ty(),
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
            ExprKind::Int(_)
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
            self.rewrite_top_level(top)?;
        }
        Ok(())
    }

    fn rewrite_top_level(&mut self, top: &mut TopLevel) -> anyhow::Result<()> {
        match top {
            TopLevel::Fn(fn_) => {
                let fn_ = Rc::get_mut(fn_).expect("TopLevel::Fn must be uniquely owned");
                if let Some(body) = &mut fn_.body {
                    self.rewrite_block(body)?;
                }
            }
            TopLevel::Impl(impl_) => {
                let impl_ = Rc::get_mut(impl_).expect("TopLevel::Impl must be uniquely owned");
                for method in &mut impl_.methods {
                    let method = Rc::get_mut(method).expect("Impl method must be uniquely owned");
                    if let Some(body) = &mut method.body {
                        self.rewrite_block(body)?;
                    }
                }
            }
            TopLevel::Struct(_) | TopLevel::Enum(_) => {}
        }
        Ok(())
    }

    fn rewrite_block(&mut self, block: &mut Block) -> anyhow::Result<()> {
        for stmt in &mut block.body {
            self.rewrite_stmt(stmt)?;
        }

        if let Some(last) = &mut block.last_expr {
            self.rewrite_expr(last)?;
        }
        Ok(())
    }

    fn rewrite_stmt(&mut self, stmt: &mut Stmt) -> anyhow::Result<()> {
        match stmt {
            Stmt::Expr(expr) => self.rewrite_expr(Rc::make_mut(expr))?,
            Stmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init {
                    self.rewrite_expr(init)?;
                }
            }
            Stmt::TupleUnpack(tuple_unpack) => self.rewrite_expr(&mut tuple_unpack.init)?,
            Stmt::Assign(assign) => {
                self.rewrite_expr(&mut assign.assignee)?;
                self.rewrite_expr(&mut assign.value)?;
            }
        }
        Ok(())
    }

    fn rewrite_if(&mut self, if_expr: &mut If) -> anyhow::Result<()> {
        self.rewrite_expr(&mut if_expr.cond)?;
        self.rewrite_expr(&mut if_expr.then)?;

        if let Some(else_) = &mut if_expr.else_ {
            match else_.as_mut() {
                Else::If(inner) => self.rewrite_if(inner)?,
                Else::Block(block) => self.rewrite_expr(block)?,
            }
        }

        if let Some(enum_unpack) = &mut if_expr.enum_unpack {
            self.rewrite_expr(Rc::make_mut(&mut enum_unpack.enum_value))?;
        }
        Ok(())
    }

    fn rewrite_expr(&mut self, expr: &mut Expr) -> anyhow::Result<()> {
        match &mut expr.kind {
            ExprKind::GenericFnCall(generic_call) => {
                for arg in &mut generic_call.args.0 {
                    self.rewrite_expr(arg)?;
                }

                if generic_call.generic_args.iter().any(contains_type_var) {
                    return Err(anyhow!(
                        "unresolved generic arguments at monomorphize: {:?}",
                        generic_call.callee.name
                    ));
                }

                let lowered = FnCall {
                    callee: generic_call.callee.clone(),
                    args: generic_call.args.clone(),
                    span: generic_call.span,
                };

                expr.kind = ExprKind::FnCall(lowered);
            }
            ExprKind::FnCall(call) => {
                for arg in &mut call.args.0 {
                    self.rewrite_expr(arg)?;
                }
            }
            ExprKind::Spawn(spawn) => {
                for arg in &mut spawn.args {
                    self.rewrite_expr(arg)?;
                }
            }
            ExprKind::ArrayLiteral(arr) => {
                for elem in &mut arr.elements {
                    self.rewrite_expr(elem)?;
                }
            }
            ExprKind::ArrayRepeat(rep) => self.rewrite_expr(&mut rep.value)?,
            ExprKind::TupleLiteral(tuple) => {
                for elem in &mut tuple.elements {
                    self.rewrite_expr(elem)?;
                }
            }
            ExprKind::Binary(bin) => {
                self.rewrite_expr(Rc::make_mut(&mut bin.lhs))?;
                self.rewrite_expr(Rc::make_mut(&mut bin.rhs))?;
            }
            ExprKind::Cast(cast) => self.rewrite_expr(&mut cast.operand)?,
            ExprKind::FieldAccess(field) => self.rewrite_expr(&mut field.operand)?,
            ExprKind::TupleIndexing(tuple_idx) => {
                self.rewrite_expr(Rc::make_mut(&mut tuple_idx.tuple))?;
            }
            ExprKind::EnumVariant(enum_var) => {
                if let Some(value) = &mut enum_var.value {
                    self.rewrite_expr(value)?;
                }
            }
            ExprKind::Indexing(idx) => {
                self.rewrite_expr(Rc::make_mut(&mut idx.operand))?;
                self.rewrite_expr(&mut idx.index)?;
            }
            ExprKind::Slicing(slicing) => {
                self.rewrite_expr(Rc::make_mut(&mut slicing.operand))?;
                self.rewrite_expr(&mut slicing.start)?;
                self.rewrite_expr(&mut slicing.end)?;
            }
            ExprKind::LogicalNot(not) => self.rewrite_expr(&mut not.operand)?,
            ExprKind::BitNot(not) => self.rewrite_expr(&mut not.operand)?,
            ExprKind::Closure(closure) => {
                for capture in &mut closure.captures {
                    self.rewrite_expr(capture)?;
                }
                self.rewrite_expr(&mut closure.body)?;
            }
            ExprKind::Return(ret) => {
                if let Some(value) = ret {
                    self.rewrite_expr(value)?;
                }
            }
            ExprKind::If(if_expr) => self.rewrite_if(if_expr)?,
            ExprKind::Loop(loop_expr) => self.rewrite_block(&mut loop_expr.body)?,
            ExprKind::Block(block) => self.rewrite_block(block)?,
            ExprKind::BuiltinFnCall(call) => {
                for arg in &mut call.args.0 {
                    self.rewrite_expr(arg)?;
                }
            }
            ExprKind::StructLiteral(lit) => {
                for (_, value) in &mut lit.values {
                    self.rewrite_expr(value)?;
                }
            }
            ExprKind::Int(_)
            | ExprKind::StringLiteral(_)
            | ExprKind::ByteStringLiteral(_)
            | ExprKind::ByteLiteral(_)
            | ExprKind::CharLiteral(_)
            | ExprKind::Variable(_)
            | ExprKind::BooleanLiteral(_)
            | ExprKind::FnPointer(_)
            | ExprKind::Break => {}
        }
        Ok(())
    }
}
