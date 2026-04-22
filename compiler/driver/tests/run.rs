use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::fs;
use std::process::Command;

fn create_project(temp_dir: &assert_fs::TempDir, program: &str) -> anyhow::Result<()> {
    create_project_files(temp_dir, &[("main.kd", program)])
}

fn create_project_files(
    temp_dir: &assert_fs::TempDir,
    files: &[(&str, &str)],
) -> anyhow::Result<()> {
    temp_dir.child("Kaede.toml").write_str(
        "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[build]\nsrc = \"src\"\nout = \"build/test\"\n",
    )?;
    let src_dir = temp_dir.child("src");
    src_dir.create_dir_all()?;
    for (path, program) in files {
        src_dir.child(path).write_str(program)?;
    }
    Ok(())
}

#[test]
fn build_writes_binary_to_configured_out_path() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    create_project(
        &temp_dir,
        r#"fun main() -> i32 {
    return 0
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("build")
        .assert()
        .success();

    // Fixture's manifest sets `out = "build/test"`; confirm the binary
    // lands there and not at the legacy hardcoded `build/main`.
    temp_dir
        .child("build")
        .child("test")
        .assert(predicate::path::is_file());
    temp_dir
        .child("build")
        .child("main")
        .assert(predicate::path::missing());

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

#[test]
fn build_rejects_rust_import_when_rust_section_is_missing() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    // Manifest omits `[rust]`, so Rust interop is not enabled even though
    // a `rust/` directory is present on disk.
    create_project(
        &temp_dir,
        "import rust::anything\n\nfun main() -> i32 { return 0 }",
    )?;

    let rust_dir = temp_dir.child("rust");
    rust_dir.child("src").create_dir_all()?;
    fs::write(
        rust_dir.child("Cargo.toml").path(),
        "[package]\nname = \"anything\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    )?;
    fs::write(rust_dir.child("src/lib.rs").path(), "pub fn f() {}\n")?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("build")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Add a `[rust]` section to Kaede.toml",
        ));

    Ok(())
}

#[test]
fn run_resolves_rust_import_from_configured_rust_path() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;

    // Manifest points `rust.path` at a non-default directory (`vendor`)
    // to exercise the `rust.path` plumbing end-to-end.
    temp_dir.child("Kaede.toml").write_str(
        r#"[package]
name = "custom_rust_path"
version = "0.1.0"

[build]
src = "src"
out = "build/custom_rust_path"

[rust]
path = "vendor"
"#,
    )?;

    let src_dir = temp_dir.child("src");
    src_dir.create_dir_all()?;
    src_dir.child("main.kd").write_str(
        r#"import rust::custom_rust_path

fun main() -> i32 {
    return rust::custom_rust_path::answer()
}"#,
    )?;

    let vendor = temp_dir.child("vendor");
    vendor.child("src").create_dir_all()?;
    fs::write(
        vendor.child("Cargo.toml").path(),
        "[package]\nname = \"custom_rust_path\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    )?;
    fs::write(
        vendor.child("src/lib.rs").path(),
        "pub fn answer() -> i32 { 42 }\n",
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .current_dir(temp_dir.path())
        .arg("run")
        .assert()
        .code(predicate::eq(42));

    Ok(())
}
