mod common;

use common::semantic_analyze;
use kaede_ir::{
    expr::{Expr, ExprKind},
    stmt::Stmt,
    top::TopLevel,
    ty::contains_type_var,
    CompileUnit,
};
use kaede_symbol::Symbol;

use crate::common::semantic_analyze_expect_error;

fn find_generic_callee_in_expr(expr: &Expr) -> Option<kaede_ir::qualified_symbol::QualifiedSymbol> {
    match &expr.kind {
        ExprKind::GenericFnCall(call) => Some(call.callee.name.clone()),
        ExprKind::FnCall(call) => {
            if call.callee.link_once {
                Some(call.callee.name.clone())
            } else {
                None
            }
        }
        ExprKind::Return(ret) => ret
            .as_ref()
            .and_then(|inner| find_generic_callee_in_expr(inner)),
        ExprKind::Block(block) => find_generic_callee_in_block(block),
        _ => None,
    }
}

fn find_generic_callee_in_block(
    block: &kaede_ir::stmt::Block,
) -> Option<kaede_ir::qualified_symbol::QualifiedSymbol> {
    for stmt in &block.body {
        match stmt {
            Stmt::Expr(expr) => {
                if let Some(name) = find_generic_callee_in_expr(expr) {
                    return Some(name);
                }
            }
            Stmt::Let(let_stmt) => {
                if let Some(init) = &let_stmt.init {
                    if let Some(name) = find_generic_callee_in_expr(init) {
                        return Some(name);
                    }
                }
            }
            Stmt::TupleUnpack(tuple_unpack) => {
                if let Some(name) = find_generic_callee_in_expr(&tuple_unpack.init) {
                    return Some(name);
                }
            }
            Stmt::Assign(assign) => {
                if let Some(name) = find_generic_callee_in_expr(&assign.assignee) {
                    return Some(name);
                }
                if let Some(name) = find_generic_callee_in_expr(&assign.value) {
                    return Some(name);
                }
            }
        }
    }

    block
        .last_expr
        .as_ref()
        .and_then(|expr| find_generic_callee_in_expr(expr))
}

fn main_generic_callee_name(ir: &CompileUnit) -> kaede_ir::qualified_symbol::QualifiedSymbol {
    let main_name = Symbol::from("kdmain".to_owned());
    let main_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_) if fn_.decl.name.symbol() == main_name => Some(fn_),
            _ => None,
        })
        .expect("expected main function");

    let body = main_fn.body.as_ref().expect("expected main body");
    find_generic_callee_in_block(body).expect("expected generated call in main body")
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    semantic_analyze("fn foo() {}")?;
    Ok(())
}

#[test]
fn empty_struct() -> anyhow::Result<()> {
    semantic_analyze("struct Foo {}")?;
    Ok(())
}

#[test]
fn empty_enum() -> anyhow::Result<()> {
    semantic_analyze("enum Foo {}")?;
    Ok(())
}

#[test]
fn function_with_return() -> anyhow::Result<()> {
    semantic_analyze("fn foo(): i32 { return 1 }")?;
    Ok(())
}

#[test]
fn simple_struct() -> anyhow::Result<()> {
    semantic_analyze("struct Foo { a: i32, b: i32 }")?;
    Ok(())
}

#[test]
fn simple_enum() -> anyhow::Result<()> {
    semantic_analyze("enum Foo { A, B }")?;
    Ok(())
}

#[test]
fn function_with_params() -> anyhow::Result<()> {
    semantic_analyze("fn foo(a: i32, b: i32): i32 { return a + b }")?;
    Ok(())
}

#[test]
fn function_with_generic_params() -> anyhow::Result<()> {
    semantic_analyze(
        "fn foo<T, U>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<i32, i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fn foo<T>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fn foo<T, U>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<123>(1, 2)
    }",
    )?;

    // Generic function arguments should be inferred from call arguments.
    semantic_analyze(
        "fn foo<T, U>(a: T, b: U): T {
        return a + b
    }
    fn f(): i32 {
        return foo(1, 2)
    }",
    )?;

    Ok(())
}

#[test]
fn generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        fn f() {
            let x = Foo<i32> { a: 1 }
        }
    ",
    )?;
    semantic_analyze(
        "enum Foo<T> { A, B(T) }
        fn f() {
            let x = Foo<i32>::B(1)
        }
    ",
    )?;
    semantic_analyze(
        "fn foo<T>(a: T): T { return a }
        fn f() {
            foo<i32>(1)
        }
    ",
    )?;
    semantic_analyze(
        "fn foo<T>(a: T): T { return a }
        fn f() {
            foo<i32>(1)
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_for_generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        impl<T> Foo<T> {
            fn f(self): T {
                return self.a
            }
        }
        fn f() {
            let foo = Foo<i32> { a: 1 }
            let x = foo.f()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        impl Foo {
            fn f(self): i32 {
                return self.a
            }
        }
        fn f() {
            let foo = Foo { a: 1 }
            let x = foo.f()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_with_static_method() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        impl Foo {
            fn new(n: i32): Foo {
                return Foo { a: n }
            }
            fn get_a(self): i32 {
                return self.a
            }
        }
        fn f(): i32 {
            let foo = Foo::new(1)
            return foo.get_a()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_with_static_method_for_generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        impl<T> Foo<T> {
            fn new(n: T): Foo<T> {
                return Foo<T> { a: n }
            }
            fn get_a(self): T {
                return self.a
            }
        }
        fn f(): i32 {
            let foo = Foo<i32>::new(1)
            return foo.get_a()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn extern_() -> anyhow::Result<()> {
    semantic_analyze(
        r#"extern "C" fn foo(): i32
        fn f() {
            foo()
        }
    "#,
    )?;
    Ok(())
}

#[test]
fn type_alias() -> anyhow::Result<()> {
    // Type alias resolved and used in function signature
    semantic_analyze(
        "type MyInt = i32
        fn foo(x: MyInt): i32 {
            return x
        }",
    )?;
    Ok(())
}

#[test]
fn type_alias_pub() -> anyhow::Result<()> {
    semantic_analyze(
        "pub type MyInt = i32
        fn foo(x: MyInt): i32 {
            return x
        }",
    )?;
    Ok(())
}

#[test]
fn generated_generic_function_has_concrete_types_after_inference() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fn __test_id<T>(x: T): T {
            return x
        }

        fn main(): i32 {
            let n: i32 = __test_id(1)
            return 58
        }
        "#,
    )?;

    let generated_name = main_generic_callee_name(&ir);
    let generated_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_) if fn_.decl.name == generated_name => Some(fn_),
            _ => None,
        })
        .expect("expected generated generic function");

    assert!(
        !contains_type_var(&generated_fn.decl.return_ty),
        "generated function return type should be concrete after inference"
    );

    Ok(())
}

#[test]
fn generated_generic_impl_method_has_concrete_types_after_inference() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        struct __TestBox<T> {
            value: T,
        }

        impl<T> __TestBox<T> {
            fn __test_get(self): T {
                return self.value
            }
        }

        fn main(): i32 {
            let b = __TestBox<i32> { value: 58 }
            return b.__test_get()
        }
        "#,
    )?;

    let generated_name = main_generic_callee_name(&ir);
    let generated_method = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Impl(impl_) => impl_
                .methods
                .iter()
                .find(|method| method.decl.name == generated_name),
            _ => None,
        })
        .expect("expected generated generic impl method");

    assert!(
        generated_method
            .decl
            .params
            .iter()
            .all(|param| !contains_type_var(&param.ty)),
        "generated method params should be concrete after inference"
    );
    assert!(
        !contains_type_var(&generated_method.decl.return_ty),
        "generated method return type should be concrete after inference"
    );

    Ok(())
}

#[test]
fn type_alias_struct() -> anyhow::Result<()> {
    semantic_analyze(
        "struct S { x: i32 }
         type AliasS = S
         fn foo(): S {
            return AliasS { x: 1 }
         }",
    )?;
    Ok(())
}
