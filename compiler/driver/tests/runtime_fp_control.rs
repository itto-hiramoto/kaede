#![cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]

mod driver_test_support;

use assert_fs::prelude::*;
use driver_test_support::run_binary;
use kaede_common::{
    kaede_gc_lib_path, kaede_lib_path, kaede_runtime_lib_path, kaede_runtime_linker_flags,
};
use std::process::Command;

#[cfg(target_arch = "x86_64")]
const FE_UPWARD: i32 = 0x800;
#[cfg(target_arch = "aarch64")]
const FE_UPWARD: i32 = 0x400000;
const FE_TONEAREST: i32 = 0;

/// Compile `program` to an object file and link it like the driver does, plus
/// libm for the `<fenv.h>` functions.
fn run_program_with_libm(expect: i32, program: &str) -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    let obj = tempdir.child("main.o");
    let exe = tempdir.child("a.out");
    main.write_str(program)?;

    let compile = Command::new(env!("CARGO_BIN_EXE_kaede"))
        .arg(main.path())
        .arg("-c")
        .arg("-o")
        .arg(obj.path())
        .arg("--root-dir")
        .arg(tempdir.path())
        .output()?;
    assert!(
        compile.status.success(),
        "kaede compile failed\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let link = Command::new("cc")
        .arg("-fPIE")
        .arg("-o")
        .arg(exe.path())
        .arg(obj.path())
        .args(kaede_runtime_linker_flags())
        .arg(kaede_lib_path())
        .arg(kaede_runtime_lib_path())
        .arg(kaede_gc_lib_path())
        .arg("-pthread")
        .arg("-lm")
        .output()?;
    assert!(
        link.status.success(),
        "link failed\nstderr:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    run_binary(expect, exe.path())
}

#[test]
fn rounding_mode_stays_with_its_task_across_yields() -> anyhow::Result<()> {
    let program = r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

extern "C" fun fesetround(mode: i32) -> i32
extern "C" fun fegetround() -> i32
extern "C" fun kaede_yield()

fun check_across_yields(mode: i32, ch: Channel<i32>) {
    let mut i = 0
    while i < 1000 {
        kaede_yield()
        if fegetround() != mode {
            ch.send(1)
            return
        }
        i = i + 1
    }
    ch.send(0)
}

fun set_upward(ch: Channel<i32>) {
    if fesetround(FE_UPWARD) != 0 {
        ch.send(2)
        return
    }
    check_across_yields(FE_UPWARD, ch)
}

fun keep_nearest(ch: Channel<i32>) {
    check_across_yields(FE_TONEAREST, ch)
}

fun main() -> i32 {
    let ch = Channel<i32>::with_capacity(8)
    let mut n = 0
    while n < 4 {
        spawn set_upward(ch)
        spawn keep_nearest(ch)
        n = n + 1
    }

    let mut failure = 0
    let mut received = 0
    while received < 8 {
        let code = match ch.recv() {
            Option::Some(v) => v,
            Option::None => return 3,
        }
        if code != 0 {
            failure = code
        }
        received = received + 1
    }

    if fegetround() != FE_TONEAREST {
        return 4
    }

    return failure
}"#
    .replace("FE_UPWARD", &FE_UPWARD.to_string())
    .replace("FE_TONEAREST", &FE_TONEAREST.to_string());

    run_program_with_libm(0, &program)
}
