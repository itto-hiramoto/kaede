use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

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
    let executable = temp_dir.child("leak");

    program_file.write_str(TEST_PROGRAM)?;

    Ok((temp_dir, program_file, executable))
}

/// Helper function to compile the program with given compiler arguments
fn compile_program(
    program_file: &impl AsRef<std::path::Path>,
    executable: &impl AsRef<std::path::Path>,
    temp_dir: &assert_fs::TempDir,
    extra_args: &[&str],
) -> anyhow::Result<()> {
    let executable_path = executable.as_ref().to_string_lossy();
    let temp_dir_path = temp_dir.path().to_string_lossy();
    let program_file_path = program_file.as_ref().to_string_lossy();

    let mut args = vec![
        "-O0",
        "-o",
        executable_path.as_ref(),
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

/// Helper function to run valgrind and check for leaks
fn run_valgrind_leak_check(executable: &impl AsRef<std::path::Path>, should_have_leaks: bool) {
    let executable_path = executable.as_ref().to_string_lossy();
    let assertion = Command::new("valgrind")
        .args(["--leak-check=full", executable_path.as_ref()])
        .assert()
        .code(predicate::eq(58));

    if should_have_leaks {
        // Expect memory leaks when GC is disabled
        assertion.stderr(
            predicate::str::contains("definitely lost")
                .and(predicate::str::contains("definitely lost: 0").not()),
        );
    } else {
        // Expect no memory leaks when GC is enabled
        assertion.stderr(
            // Valgrind results display changes depending on version
            predicate::str::contains("definitely lost: 0")
                .or(predicate::str::contains("definitely lost").count(0)),
        );
    }
}

#[test]
fn leak_check_with_valgrind() -> anyhow::Result<()> {
    let (temp_dir, program_file, executable) = setup_test_environment()?;
    compile_program(&program_file, &executable, &temp_dir, &[])?;
    run_valgrind_leak_check(&executable, false);
    Ok(())
}

#[test]
fn leak_check_with_no_gc_should_fail() -> anyhow::Result<()> {
    let (temp_dir, program_file, executable) = setup_test_environment()?;
    compile_program(&program_file, &executable, &temp_dir, &["--no-gc"])?;
    run_valgrind_leak_check(&executable, true);
    Ok(())
}
