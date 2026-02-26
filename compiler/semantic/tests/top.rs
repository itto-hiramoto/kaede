mod common;

use common::semantic_analyze;
use kaede_ir::{top::TopLevel, ty::contains_type_var};

use crate::common::semantic_analyze_expect_error;

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

    let generated_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_)
                if fn_.decl.link_once
                    && fn_
                        .decl
                        .name
                        .symbol()
                        .as_str()
                        .starts_with("__test_id") =>
            {
                Some(fn_)
            }
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
