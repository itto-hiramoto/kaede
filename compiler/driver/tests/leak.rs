use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn leak_check_with_valgrind() -> anyhow::Result<()> {
    // Create a temporary directory for all files
    let temp_dir = assert_fs::TempDir::new()?;

    let program_file = temp_dir.child("leak.kd");
    program_file.write_str(
        r"struct Num {
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
        }",
    )?;

    // Compile to executable directly
    let executable = temp_dir.child("leak");
    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            "-O0",
            "-o",
            &executable.path().to_string_lossy(),
            "--root-dir",
            &temp_dir.path().to_string_lossy(),
            &program_file.path().to_string_lossy(),
        ])
        .assert()
        .success();

    // Leak check
    Command::new("valgrind")
        .args(["--leak-check=full", &executable.path().to_string_lossy()])
        .assert()
        .code(predicate::eq(58))
        .stderr(
            // Valgrind results display changes depending on version
            predicate::str::contains("definitely lost: 0")
                .or(predicate::str::contains("definitely lost").count(0)),
        );

    Ok(())
}
