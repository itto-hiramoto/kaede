use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn compiled_binary_initializes_runtime_before_running_main() -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let main = temp_dir.child("main.kd");
    let exe = temp_dir.child("a.out");

    main.write_str(
        r#"fun main() -> i32 {
    return 0
}"#,
    )?;

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            main.path().to_str().unwrap(),
            "-o",
            exe.path().to_str().unwrap(),
            "--root-dir",
            temp_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    Command::new(exe.path()).assert().code(predicate::eq(0));

    Ok(())
}
