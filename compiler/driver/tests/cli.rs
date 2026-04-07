use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::{fs, process::Command};

#[test]
fn failed_to_open_file() -> anyhow::Result<()> {
    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .arg("test/file/doesnt/exist")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to open file"));

    Ok(())
}

#[test]
fn no_input_files() -> anyhow::Result<()> {
    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .assert()
        .failure()
        .stderr(predicate::str::contains("No input files"));

    Ok(())
}

#[test]
fn new_creates_hello_world_main() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let project_name = "hello_kaede";
    let project_dir = temp_dir.child(project_name);
    let main = project_dir.child("src/main.kd");

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .args(["new", project_name])
        .assert()
        .success();

    main.assert(predicate::path::is_file());
    assert_eq!(
        fs::read_to_string(main.path())?,
        r#"fn main() {
    println("hello, world!")
}"#,
    );

    Ok(())
}

#[test]
fn direct_compile_uses_unique_entry_candidate_even_if_not_main_kd() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let lib = temp_dir.child("lib.kd");
    let app = temp_dir.child("app.kd");
    let exe = temp_dir.child("a.out");

    lib.write_str("fn helper(): i32 { return 0 }")?;
    app.write_str("return 42")?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            lib.path().to_str().unwrap(),
            app.path().to_str().unwrap(),
            "-o",
            exe.path().to_str().unwrap(),
            "--root-dir",
            temp_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    Command::new(exe.path()).assert().code(predicate::eq(42));

    Ok(())
}

#[test]
fn direct_compile_fails_when_entry_candidate_is_ambiguous() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let app1 = temp_dir.child("app1.kd");
    let app2 = temp_dir.child("app2.kd");
    let exe = temp_dir.child("a.out");

    app1.write_str("return 1")?;
    app2.write_str("fn main(): i32 { return 2 }")?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            app1.path().to_str().unwrap(),
            app2.path().to_str().unwrap(),
            "-o",
            exe.path().to_str().unwrap(),
            "--root-dir",
            temp_dir.path().to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Entry unit is ambiguous"));

    Ok(())
}

#[test]
fn direct_compile_rejects_try_outside_result_function() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let app = temp_dir.child("app.kd");
    let exe = temp_dir.child("a.out");

    app.write_str(
        r#"import std.result
use std.result.Result

fn fail(): i32 {
    value := Result::Ok(1)?;
    return value
}

fn main(): i32 {
    return fail()
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            app.path().to_str().unwrap(),
            "-o",
            exe.path().to_str().unwrap(),
            "--root-dir",
            temp_dir.path().to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "`?` can only be used in functions returning `Result<T, E>`",
        ));

    Ok(())
}
