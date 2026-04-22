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
    temp_dir.child("Kaede.toml").write_str(
        "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[build]\nsrc = \"src\"\nout = \"build/main\"\n",
    )?;
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
        .stderr(predicate::str::contains("Kaede.toml"));

    Ok(())
}

#[test]
fn run_executes_build_main_and_propagates_exit_code() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"fun main() -> i32 {
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
            ("main.kd", "fun helper() -> i32 { return 0 }"),
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
            ("main.kd", "fun main() -> i32 { return 0 }"),
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
        r#"extern "C" fun strcmp(s1: *u8, s2: *u8) -> i32

fun main(args: Vector<str>) -> i32 {
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

#[test]
fn run_executes_result_try_program() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"import std.result
use std.result.Result

fun inner() -> Result<i32, str> {
    return Result::Ok(42)
}

fun outer() -> Result<i32, str> {
    value := inner()?;
    return Result::Ok(value)
}

fun main() -> i32 {
    return match outer() {
        Result::Ok(value) => value,
        Result::Err(_) => 1,
    }
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
fn run_executes_option_try_program() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    create_project(
        &temp_dir,
        r#"import std.option
use std.option.Option

fun inner() -> Option<i32> {
    return Option::Some(42)
}

fun outer() -> Option<i32> {
    value := inner()?;
    return Option::Some(value)
}

fun main() -> i32 {
    return match outer() {
        Option::Some(value) => value,
        Option::None => 1,
    }
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(42));

    Ok(())
}
