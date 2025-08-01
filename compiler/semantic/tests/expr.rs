mod common;

use common::{semantic_analyze, semantic_analyze_expect_error};

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
            } else if a > 0 {
                -1
            } else {
                0
            }
        }",
    )?;

    // Test that `a < 0` is correctly parsed as a comparison, not generic syntax
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
fn tuple_literal() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(): (i32, bool) {
            return (42, true)
        }",
    )?;
    Ok(())
}

#[test]
fn tuple_indexing() -> anyhow::Result<()> {
    semantic_analyze(
        r#"fn f(): i32 {
            let t = (42, true, "hello, world")
            return t.0
        }"#,
    )?;
    Ok(())
}

#[test]
fn struct_literal() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32, b: bool }
        fn f(): Foo {
            return Foo { a: 42, b: true }
        }",
    )?;
    Ok(())
}

#[test]
fn struct_access() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        fn f(): i32 {
            let a = Foo { a: 58 }
            return a.a
        }",
    )?;
    Ok(())
}

#[test]
fn loop_and_break() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f() {
            loop {
                break
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_bool() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: bool): i32 {
            return match x {
                true => 1,
                false => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_int() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                1 => 2,
                _ => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_enum() -> anyhow::Result<()> {
    semantic_analyze(
        "enum Foo { A, B, C }
        fn f(x: Foo): i32 {
            return match x {
                Foo::A => 1,
                Foo::B => 2,
                Foo::C => 3,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_with_catch_all() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                _ => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_enum() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B, C }
        fn f(x: Foo): i32 {
            return match x {
                Foo::A => 1,
                Foo::B => 2,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_int() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                1 => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_bool() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(x: bool): i32 {
            return match x {
                true => 1,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn enum_variant() -> anyhow::Result<()> {
    semantic_analyze(
        "enum Foo { A, B(i32), C }
        fn f(): i32 {
            let x = Foo::B(1)
            return match x {
                Foo::A => 1,
                Foo::B(y) => y,
                Foo::C => 3,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_only_int_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(): i32 {
            let x = 42
            return match x {
                _ => 1
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_only_enum_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B }
        fn f(): i32 {
            let x = Foo::A
            return match x {
                _ => 1
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_undefined_function_int_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(): i32 {
            let x = 42
            return match x {
                1 => 10,
                2 => 20,
                _ => g()  // g() is undefined
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_undefined_function_enum_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B }
        fn f(): i32 {
            let x = Foo::A
            return match x {
                Foo::A => 10,
                _ => g()  // g() is undefined
            }
        }",
    )?;
    Ok(())
}
