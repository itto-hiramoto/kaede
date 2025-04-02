mod common;

use common::semantic_analyze;

#[test]
fn int() -> anyhow::Result<()> {
    semantic_analyze("fn f() { 1 }")?;
    Ok(())
}

#[test]
fn array_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { [1, 2, 3] }")?;
    Ok(())
}

#[test]
fn boolean_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { true }")?;
    semantic_analyze("fn f() { false }")?;
    Ok(())
}

#[test]
fn block() -> anyhow::Result<()> {
    semantic_analyze("fn f() { { 1 } }")?;
    Ok(())
}

#[test]
fn string_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { \"hello\" }")?;
    Ok(())
}

#[test]
fn arithmetic_binary() -> anyhow::Result<()> {
    semantic_analyze("fn f() { 1 + 2 }")?;
    semantic_analyze("fn f() { 1 - 2 }")?;
    semantic_analyze("fn f() { 1 * 2 }")?;
    semantic_analyze("fn f() { 1 / 2 }")?;
    semantic_analyze("fn f() { 1 % 2 }")?;
    semantic_analyze("fn f() { 1 < 2 }")?;
    semantic_analyze("fn f() { 1 <= 2 }")?;
    semantic_analyze("fn f() { 1 > 2 }")?;
    semantic_analyze("fn f() { 1 >= 2 }")?;
    semantic_analyze("fn f() { 1 == 2 }")?;
    semantic_analyze("fn f() { 1 != 2 }")?;
    Ok(())
}

#[test]
fn logical_binary() -> anyhow::Result<()> {
    semantic_analyze("fn f() { true && false }")?;
    semantic_analyze("fn f() { true || false }")?;
    Ok(())
}

#[test]
fn logical_not() -> anyhow::Result<()> {
    semantic_analyze("fn f() { !true }")?;
    Ok(())
}

#[test]
fn array_indexing() -> anyhow::Result<()> {
    semantic_analyze("fn f() { [0, 1, 2][1] }")?;
    Ok(())
}

#[test]
fn function_call() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(a: i32, b: i32): i32 { return a + b }\n
        fn g(): i32 { return f(48, 10) }
    ",
    )?;
    Ok(())
}

#[test]
fn if_() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(a: i32): i32 {
            return if a == 0 {
                1
            } else if a < 0 {
                -1
            } else {
                0
            }
        }",
    )?;
    Ok(())
}

#[test]
fn if_with_struct_literal() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: bool }
        fn f(a: i32): Foo {
            return if Foo { a: true }.a { 58 } else { 123 }
        }",
    )?;
    Ok(())
}
