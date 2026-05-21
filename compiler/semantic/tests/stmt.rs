mod common;

use common::semantic_analyze;

use crate::common::semantic_analyze_expect_error;

#[test]
fn simple_let() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() {
            let x = 1
        }
    ",
    )?;
    Ok(())
}

#[test]
fn let_with_mutability() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() {
            let mut x = 1
        }
    ",
    )?;
    Ok(())
}

#[test]
fn let_with_ty() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() {
            let x: i32 = 1
        }
    ",
    )?;
    Ok(())
}

#[test]
fn let_mismatched_types() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        r#"fun f() {
            let x: i32 = "hello, world"
        }
    "#,
    )?;
    Ok(())
}

#[test]
fn let_and_access() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() -> i32 {
            let x = 57
            return x + 1
        }
    ",
    )?;
    Ok(())
}

#[test]
fn local_const() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() -> i32 {
            const base: i32 = 48
            const result: i32 = base + 10
            return result
        }
    ",
    )?;
    Ok(())
}

#[test]
fn local_const_array_repeat_count() -> anyhow::Result<()> {
    semantic_analyze(
        "fun f() {
            const base: u32 = 2
            const len: u32 = base + 2
            let _ = [0; len]
        }
    ",
    )?;
    Ok(())
}

#[test]
fn local_const_rejects_runtime_initializer() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fun value() -> i32 {
            return 1
        }

        fun f() {
            const x: i32 = value()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn local_const_rejects_assignment() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fun f() {
            const x: i32 = 1
            x = 2
        }
    ",
    )?;
    Ok(())
}
