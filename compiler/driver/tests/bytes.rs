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
fn std_bytes_copy_fill_and_compare_cover_lengths() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.bytes

fun main() -> i32 {
    let mut dst = [0 as u8; 5]
    std.bytes.fill(dst, 9)
    if dst[0] != 9 || dst[4] != 9 {
        return 1
    }

    let copied = std.bytes.copy(dst[1:4], b"abc")
    if copied != 3 {
        return 2
    }
    if dst[0] != 9 || dst[1] != 'a' as u8 || dst[2] != 'b' as u8 || dst[3] != 'c' as u8 || dst[4] != 9 {
        return 3
    }

    let mut short = [0 as u8; 2]
    if std.bytes.copy(short, b"wxyz") != 2 {
        return 4
    }
    if short[0] != 'w' as u8 || short[1] != 'x' as u8 {
        return 5
    }

    if std.bytes.compare(b"abc", b"abc") != 0 {
        return 6
    }
    if std.bytes.compare(b"abc", b"abd") >= 0 {
        return 7
    }
    if std.bytes.compare(b"abd", b"abc") <= 0 {
        return 8
    }
    if std.bytes.compare(b"ab", b"abc") >= 0 {
        return 9
    }
    if std.bytes.compare(b"abc", b"ab") <= 0 {
        return 10
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
