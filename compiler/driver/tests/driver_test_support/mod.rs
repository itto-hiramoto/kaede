#![allow(dead_code)]
// Shared helpers for tests that compile and run .kd programs against the
// kaede driver. Each consuming integration test imports the subset it
// needs via `mod runtime_test_support;`.

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::path::Path;
use std::process::{Command, Output};

/// Invoke the kaede compiler. Asserts the compile succeeded and returns
/// the raw process output so callers can inspect stderr (e.g. for
/// compile-time warnings).
pub fn compile_to(
    file_paths: &[&Path],
    root_dir: &Path,
    output_path: &Path,
) -> anyhow::Result<Output> {
    let mut args: Vec<String> = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect();

    args.push("-o".to_string());
    args.push(output_path.to_string_lossy().to_string());
    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    let output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .output()?;

    assert!(
        output.status.success(),
        "kaede compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    Ok(output)
}

/// Compile into a fresh `NamedTempFile` and return both the tempfile and
/// the compiler's output.
pub fn compile_project(
    file_paths: &[&Path],
    root_dir: &Path,
) -> anyhow::Result<(assert_fs::NamedTempFile, Output)> {
    let exe = assert_fs::NamedTempFile::new("a.out")?;
    let output = compile_to(file_paths, root_dir, exe.path())?;
    Ok((exe, output))
}

/// Run a previously-compiled binary and assert that it exits with the
/// given code.
pub fn run_binary(expect: i32, exe_path: &Path) -> anyhow::Result<()> {
    Command::new(exe_path).assert().code(predicate::eq(expect));
    Ok(())
}

/// Convenience: write a single .kd source string into a fresh tempdir,
/// compile it, run it, and assert it exits with the given code.
pub fn run_program(expect: i32, program: &str) -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(program)?;
    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    run_binary(expect, exe.path())
}
