use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use kaede_common::{
    kaede_gc_lib_path, kaede_lib_path, kaede_runtime_lib_path, kaede_runtime_library_dirs,
};
use std::{ffi::OsString, path::Path, process::Command};

/// Test program that allocates memory in a loop to test garbage collection
const TEST_PROGRAM: &str = r"struct Num {
    n: i32,
}

fn f(): ([i32; 3], (i32, i32, Num)) {
    let a = [1, 2, 3]
    let t = (48, 10, Num { n: 58 })
    return (a, t)
}

fn main(): i32 {
    let mut c = 0

    loop {
        let num = Num { n: 58 }

        if c == 1000 {
            return num.n
        }

        c = c + 1
    }

    return 123
}";

/// Helper function to set up the test environment and create the program file
fn setup_test_environment() -> anyhow::Result<(
    assert_fs::TempDir,
    impl AsRef<std::path::Path>,
    impl AsRef<std::path::Path>,
)> {
    let temp_dir = assert_fs::TempDir::new()?;
    let program_file = temp_dir.child("leak.kd");
    let object_file = temp_dir.child("leak.o");

    program_file.write_str(TEST_PROGRAM)?;

    Ok((temp_dir, program_file, object_file))
}

/// Helper function to compile the program with given compiler arguments
fn compile_program(
    program_file: &impl AsRef<std::path::Path>,
    object_file: &impl AsRef<std::path::Path>,
    temp_dir: &assert_fs::TempDir,
    extra_args: &[&str],
) -> anyhow::Result<()> {
    let object_file_path = object_file.as_ref().to_string_lossy();
    let temp_dir_path = temp_dir.path().to_string_lossy();
    let program_file_path = program_file.as_ref().to_string_lossy();

    let mut args = vec![
        "-O0",
        "-c",
        "-o",
        object_file_path.as_ref(),
        "--root-dir",
        temp_dir_path.as_ref(),
    ];
    args.extend_from_slice(extra_args);
    args.push(program_file_path.as_ref());

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .assert()
        .success();

    Ok(())
}

fn sanitizer_tests_enabled() -> bool {
    std::env::var_os("KAEDE_RUN_SANITIZER_TESTS").is_some()
}

fn sanitizer_rpath_flags() -> Vec<OsString> {
    if !cfg!(any(target_os = "linux", target_os = "macos")) {
        return Vec::new();
    }

    let mut flags = Vec::new();
    for lib_dir in kaede_runtime_library_dirs() {
        flags.push(OsString::from(format!("-Wl,-rpath,{}", lib_dir.display())));
    }
    flags
}

fn link_sanitized_executable(object_file: &Path, executable: &Path) -> anyhow::Result<()> {
    let kaede_gc_lib_path = kaede_gc_lib_path();
    let kaede_std_lib_path = kaede_lib_path();
    let kaede_runtime_lib_path = kaede_runtime_lib_path();

    let mut args = vec![
        // LeakSanitizer is integrated into AddressSanitizer on our supported targets.
        OsString::from("-fsanitize=address"),
        OsString::from("-fno-omit-frame-pointer"),
        OsString::from("-o"),
        executable.as_os_str().to_os_string(),
        object_file.as_os_str().to_os_string(),
        kaede_std_lib_path.into_os_string(),
        kaede_runtime_lib_path.into_os_string(),
        kaede_gc_lib_path.into_os_string(),
    ];
    args.extend(sanitizer_rpath_flags());
    args.push(OsString::from("-pthread"));

    let status = Command::new("cc").args(&args).status()?;
    if !status.success() {
        anyhow::bail!("Failed to emit executable file using `cc` with ASan");
    }

    Ok(())
}

fn run_sanitizer_leak_check(
    executable: &Path,
    expected_exit_code: i32,
    should_have_leaks: bool,
) -> anyhow::Result<()> {
    let output = Command::new(executable).output()?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    if should_have_leaks {
        if output.status.success() {
            anyhow::bail!("Expected LeakSanitizer to report leaks, but program succeeded");
        }
        if !stderr.contains("LeakSanitizer") && !stderr.contains("detected memory leaks") {
            anyhow::bail!("Expected LeakSanitizer output, got: {stderr}");
        }
        return Ok(());
    }

    if output.status.code() != Some(expected_exit_code) {
        anyhow::bail!(
            "Expected program to exit with {} without leaks, got status {:?}: {}",
            expected_exit_code,
            output.status.code(),
            stderr,
        );
    }
    if stderr.contains("LeakSanitizer") || stderr.contains("detected memory leaks") {
        anyhow::bail!("Unexpected LeakSanitizer output: {stderr}");
    }

    Ok(())
}

#[test]
fn leak_check_with_sanitizer() -> anyhow::Result<()> {
    if !sanitizer_tests_enabled() {
        return Ok(());
    }

    let (temp_dir, program_file, object_file) = setup_test_environment()?;
    let executable = temp_dir.child("leak");

    compile_program(&program_file, &object_file, &temp_dir, &[])?;
    link_sanitized_executable(object_file.as_ref(), executable.path())?;
    run_sanitizer_leak_check(executable.path(), 58, false)?;
    Ok(())
}

#[test]
fn leak_check_with_no_gc_should_fail_under_sanitizer() -> anyhow::Result<()> {
    if !sanitizer_tests_enabled() {
        return Ok(());
    }

    let (temp_dir, program_file, object_file) = setup_test_environment()?;
    let executable = temp_dir.child("leak");

    compile_program(&program_file, &object_file, &temp_dir, &["--no-gc"])?;
    link_sanitized_executable(object_file.as_ref(), executable.path())?;
    run_sanitizer_leak_check(executable.path(), 58, true)?;
    Ok(())
}
