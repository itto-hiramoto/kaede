#![allow(dead_code)]
// Shared helpers for tests that compile and run .kd programs against the
// kaede driver. Each consuming integration test imports the subset it
// needs via `mod runtime_test_support;`.

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;
use std::process::{Command, Output};
use std::time::Duration;
use wait_timeout::ChildExt;

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

/// Wall-clock bound for a compiled test program. The blocking primitives —
/// channels, `select`, `WaitGroup` — fail by deadlocking rather than by
/// returning a wrong exit code, so an unbounded wait turns a lost wakeup into a
/// hung job instead of a failing test.
const RUN_TIMEOUT: Duration = Duration::from_secs(30);

/// Rewind and read back one of the capture files from [`run_binary`].
fn read_captured(file: &mut File) -> String {
    let mut buf = Vec::new();
    match file
        .seek(SeekFrom::Start(0))
        .and_then(|_| file.read_to_end(&mut buf))
    {
        Ok(_) => String::from_utf8_lossy(&buf).into_owned(),
        Err(err) => format!("<failed to read captured output: {err}>"),
    }
}

/// Run a previously-compiled binary and assert that it exits with the
/// given code within [`RUN_TIMEOUT`].
pub fn run_binary(expect: i32, exe_path: &Path) -> anyhow::Result<()> {
    // Capture through temp files rather than pipes: libtest cannot capture a
    // child's file descriptors, so inherited output would land unattributed in
    // the runner's output, and an undrained pipe would block a chatty program
    // and be misreported as a deadlock.
    let mut stdout_capture = tempfile::tempfile()?;
    let mut stderr_capture = tempfile::tempfile()?;

    let mut child = Command::new(exe_path)
        .stdout(stdout_capture.try_clone()?)
        .stderr(stderr_capture.try_clone()?)
        .spawn()?;

    let status = match child.wait_timeout(RUN_TIMEOUT)? {
        Some(status) => status,
        None => {
            // Ignore the errors here so the timeout is what gets reported.
            let _ = child.kill();
            let _ = child.wait();
            panic!(
                "`{}` did not exit within {RUN_TIMEOUT:?} and was killed; \
                 a blocking primitive most likely deadlocked\nstdout:\n{}\nstderr:\n{}",
                exe_path.display(),
                read_captured(&mut stdout_capture),
                read_captured(&mut stderr_capture)
            );
        }
    };

    assert_eq!(
        status.code(),
        Some(expect),
        "`{}` did not exit with {expect}: {status:?}\nstdout:\n{}\nstderr:\n{}",
        exe_path.display(),
        read_captured(&mut stdout_capture),
        read_captured(&mut stderr_capture)
    );

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
