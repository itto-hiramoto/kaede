mod driver_test_support;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

use driver_test_support::{compile_project, run_program as test};

#[test]
fn std_bytes_primitive_endian_methods_round_trip() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.bytes

fun main() -> i32 {
    let mut buf = [0 as u8; 16]

    let small: u16 = 0x1234
    small.write_le_bytes(buf, 1)
    if buf[1] != 0x34 {
        return 1
    }
    if buf[2] != 0x12 {
        return 2
    }
    if u16::from_le_bytes(buf, 1) != 0x1234 {
        return 3
    }

    let wide: u64 = 0x0102030405060708
    wide.write_le_bytes(buf, 4)
    if buf[4] != 0x08 {
        return 4
    }
    if buf[11] != 0x01 {
        return 5
    }
    if u64::from_le_bytes(buf, 4) != 0x0102030405060708 {
        return 6
    }

    return 0
}"#,
    )
}

#[test]
fn std_bytes_range_checks_panic() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import std.bytes

fun main() -> i32 {
    let buf = [0 as u8; 1]
    u16::from_le_bytes(buf, 0)
    return 0
}"#,
    )?;

    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    Command::new(exe.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "panic: u16::from_le_bytes: buffer too small",
        ));

    Ok(())
}
