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
            const base = 48
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
            const base = 2
            const len = base + 2
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
            const x = value()
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

#[test]
fn top_level_const() -> anyhow::Result<()> {
    semantic_analyze(
        "export const BNODE_LEAF = 2

        fun main() -> i32 {
            return BNODE_LEAF
        }
    ",
    )?;
    Ok(())
}

#[test]
fn top_level_const_arithmetic() -> anyhow::Result<()> {
    semantic_analyze(
        "const BASE = 2
        const LEN = BASE + 2

        fun f() {
            let _ = [0; LEN]
        }
    ",
    )?;
    Ok(())
}

#[test]
fn top_level_const_rejects_runtime_initializer() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fun value() -> i32 {
            return 1
        }

        const X = value()
    ",
    )?;
    Ok(())
}

#[test]
fn top_level_const_rejects_assignment() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "const X = 1

        fun f() {
            X = 2
        }
    ",
    )?;
    Ok(())
}
