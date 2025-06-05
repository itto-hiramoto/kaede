mod common;

use common::semantic_analyze;

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

    Ok(())
}
