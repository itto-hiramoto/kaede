use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

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
