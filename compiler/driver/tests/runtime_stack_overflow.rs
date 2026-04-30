use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::path::Path;
use std::process::{Command, Output};

fn compile(file_paths: &[&Path], root_dir: &Path, output_path: &Path) -> anyhow::Result<Output> {
    let mut args = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<String>>();

    args.push("-o".to_string());
    args.push(output_path.to_string_lossy().to_string());
    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    Ok(Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .output()?)
}

fn compile_program(
    program: &str,
) -> anyhow::Result<(assert_fs::TempDir, assert_fs::NamedTempFile)> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(program)?;

    let exe = assert_fs::NamedTempFile::new("overflow.out")?;
    let output = compile(&[main.path()], tempdir.path(), exe.path())?;

    assert!(
        output.status.success(),
        "kaede compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    Ok((tempdir, exe))
}

fn run_and_capture_stderr(exe_path: &Path) -> anyhow::Result<Output> {
    // Disable core dumps for the child so the test does not litter the
    // working directory or stall on a slow `core_pattern` handler.
    let mut cmd = Command::new(exe_path);
    cmd.env("RUST_BACKTRACE", "0");
    Ok(cmd.output()?)
}

fn assert_stack_overflow(output: &Output, expected_task_label: &str) {
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "expected non-zero exit, got {:?}\nstderr:\n{}",
        output.status,
        stderr
    );
    assert!(
        stderr.contains("kaede runtime: green thread stack overflow"),
        "stderr did not contain stack overflow diagnostic\nstderr:\n{}",
        stderr
    );
    assert!(
        stderr.contains(&format!("task: {}", expected_task_label)),
        "stderr did not identify task as `{}`\nstderr:\n{}",
        expected_task_label,
        stderr
    );
    assert!(
        stderr.contains("configured stack size:"),
        "stderr did not include configured stack size\nstderr:\n{}",
        stderr
    );
}

#[test]
fn main_task_stack_overflow_emits_diagnostic() -> anyhow::Result<()> {
    let (_tempdir, exe) = compile_program(
        r#"fun recurse(n: i32) -> i32 {
    let mut buf: [i32; 64] = [0; 64]
    buf[0] = n
    let r = recurse(n + 1)
    return r + buf[0]
}

fun main() -> i32 {
    return recurse(0)
}"#,
    )?;

    let output = run_and_capture_stderr(exe.path())?;
    assert_stack_overflow(&output, "main");
    Ok(())
}

#[test]
fn spawned_task_stack_overflow_emits_diagnostic() -> anyhow::Result<()> {
    let (_tempdir, exe) = compile_program(
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun recurse(n: i32) -> i32 {
    let mut buf: [i32; 64] = [0; 64]
    buf[0] = n
    let r = recurse(n + 1)
    return r + buf[0]
}

fun overflow_task(ch: Channel<i32>) {
    ch.send(recurse(0))
}

fun main() -> i32 {
    let ch = Channel<i32>::new()
    spawn overflow_task(ch)

    // Block the main task until the spawned task sends — which will never
    // happen, since it overflows its stack first and the runtime
    // terminates the whole process via the SIGSEGV diagnostic.
    match ch.recv() {
        Option::Some(v) => return v,
        Option::None => return 1,
    }
}"#,
    )?;

    let output = run_and_capture_stderr(exe.path())?;
    assert_stack_overflow(&output, "spawned");
    Ok(())
}
