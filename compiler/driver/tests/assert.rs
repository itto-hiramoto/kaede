mod driver_test_support;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

use driver_test_support::{compile_project, run_program as test};

#[test]
fn std_assert_accepts_true_conditions() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.assert
use std.assert.assert

fun main() -> i32 {
    assert(true)
    std.assert.assert(1 + 1 == 2, "math broke")
    return 0
}"#,
    )
}

#[test]
fn std_assert_panics_with_custom_message() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import std.assert
use std.assert.assert

fun main() {
    assert(false, "custom assert message")
}"#,
    )?;

    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    Command::new(exe.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("panic: custom assert message"));

    Ok(())
}
