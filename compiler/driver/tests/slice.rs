mod driver_test_support;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

use driver_test_support::{compile_project, run_program as test};

#[test]
fn slice_copy_from_copies_exact_length() -> anyhow::Result<()> {
    test(
        0,
        r#"fun main() -> i32 {
    let mut bytes = [0 as u8; 5]
    bytes[1:4].copy_from(b"abc")
    if bytes[0] != 0 || bytes[1] != 'a' as u8 || bytes[2] != 'b' as u8 || bytes[3] != 'c' as u8 || bytes[4] != 0 {
        return 1
    }

    let mut nums = [0, 0, 0]
    let src = [4, 5, 6]
    nums[:].copy_from(src[:])
    if nums[0] != 4 || nums[1] != 5 || nums[2] != 6 {
        return 2
    }

    return 0
}"#,
    )
}

#[test]
fn slice_copy_from_panics_on_length_mismatch() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(
        r#"fun main() {
    let mut dst = [0 as u8; 2]
    dst[:].copy_from(b"abc")
}"#,
    )?;

    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    Command::new(exe.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "panic: [T].copy_from: length mismatch",
        ));

    Ok(())
}
