use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

fn create_project(temp_dir: &assert_fs::TempDir, program: &str) -> anyhow::Result<()> {
    create_project_files(temp_dir, &[("main.kd", program)])
}

fn create_project_files(
    temp_dir: &assert_fs::TempDir,
    files: &[(&str, &str)],
) -> anyhow::Result<()> {
    let src_dir = temp_dir.child("src");
    src_dir.create_dir_all()?;
    for (path, program) in files {
        src_dir.child(path).write_str(program)?;
    }
    Ok(())
}

#[test]
fn run_fails_outside_project() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "This command expects a Kaede project",
        ));

    Ok(())
}

#[test]
fn run_executes_build_main_and_propagates_exit_code() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"fn main(): i32 {
    return 42
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(42));

    Ok(())
}

#[test]
fn run_executes_top_level_script_entry() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"let mut n = 40
n += 2
return n"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(42));

    Ok(())
}

#[test]
fn run_uses_unique_entry_candidate_even_if_not_main_kd() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project_files(
        &temp_dir,
        &[
            ("main.kd", "fn helper(): i32 { return 0 }"),
            (
                "app.kd",
                r#"let mut n = 40
n += 2
return n"#,
            ),
        ],
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(42));

    Ok(())
}

#[test]
fn run_fails_when_entry_candidate_is_ambiguous() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project_files(
        &temp_dir,
        &[
            ("main.kd", "fn main(): i32 { return 0 }"),
            ("app.kd", "return 42"),
        ],
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Entry unit is ambiguous"));

    Ok(())
}

#[test]
fn run_forwards_args_after_double_dash() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"extern "C" fn strcmp(s1: *i8, s2: *i8): i32

fn main(args: Vector<str>): i32 {
    if args.len() < 2 {
        return 125
    }

    let s = args.at(1).unwrap()

    if strcmp(s.as_ptr(), "kaede".as_ptr()) == 0 {
        return 123
    }

    return 124
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .args(["run", "--", "kaede"])
        .assert()
        .code(predicate::eq(123));

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .args(["run", "--", "hello"])
        .assert()
        .code(predicate::eq(124));

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(125));

    Ok(())
}
