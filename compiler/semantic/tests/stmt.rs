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
