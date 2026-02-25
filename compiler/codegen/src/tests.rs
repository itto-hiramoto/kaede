//! These tests compile and execute binaries and rely on GC/runtime state.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

use inkwell::context::Context;
use kaede_common::{kaede_gc_lib_path, kaede_lib_path, kaede_runtime_lib_path};
use kaede_parse::Parser;
use kaede_semantic::{SemanticAnalyzer, SemanticError};
use tempfile::tempdir;

use crate::{CodeGenerator, CodegenCtx};

/// Helper function to extract SemanticError from anyhow::Error for tests
fn extract_semantic_error(err: anyhow::Error) -> SemanticError {
    err.downcast::<SemanticError>()
        .expect("Expected SemanticError in test")
}

/// Return exit status
fn exec(program: &str) -> anyhow::Result<i32> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context, false).unwrap();

    let file = PathBuf::from("test").into();

    let ast = Parser::new(program, file).run().unwrap();
    let ir = SemanticAnalyzer::new_for_single_file_test().analyze(ast, false, false)?;

    let module = CodeGenerator::new(&cgcx).unwrap().codegen(ir).unwrap();
    if let Some(main_fn) = module.get_function("main") {
        unsafe {
            main_fn.delete();
        }
    }

    let temp_dir = tempdir()?;
    let bitcode_path = temp_dir.path().join("test.bc");
    let obj_path = temp_dir.path().join("test.o");
    let harness_path = temp_dir.path().join("test_main.c");
    let exe_path = temp_dir.path().join("test_exec");

    module.write_bitcode_to_path(&bitcode_path);

    let status = Command::new("llc")
        .args([
            "-filetype=obj",
            "-relocation-model=pic",
            "-o",
            obj_path.to_str().unwrap(),
            bitcode_path.to_str().unwrap(),
        ])
        .status()?;
    if !status.success() {
        anyhow::bail!("Failed to emit object file using 'llc'");
    }

    let harness_src = r#"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef void (*TaskFn)(void *);

extern int32_t kdmain(void);

void kaede_runtime_init(void);
void kaede_spawn_main(TaskFn fn, void *arg, size_t arg_size);
int kaede_runtime_run(void);
void kaede_runtime_shutdown(void);
void kaede_runtime_set_exit_code(int code);

static void kaede_test_main(void *arg) {
    (void)arg;
    int32_t code = kdmain();
    kaede_runtime_set_exit_code((int)code);
}

int main(void) {
    kaede_runtime_init();
    kaede_spawn_main(kaede_test_main, NULL, 0);
    int code = kaede_runtime_run();
    kaede_runtime_shutdown();
    fprintf(stderr, "%d\n", code);
    return 0;
}
"#;

    fs::write(&harness_path, harness_src)?;

    let kaede_gc_lib_path = kaede_gc_lib_path();
    let kaede_std_lib_path = kaede_lib_path();
    let kaede_runtime_lib_path = kaede_runtime_lib_path();

    let status = Command::new("cc")
        .args([
            "-fPIE",
            "-o",
            exe_path.to_str().unwrap(),
            harness_path.to_str().unwrap(),
            obj_path.to_str().unwrap(),
            kaede_std_lib_path.to_str().unwrap(),
            kaede_runtime_lib_path.to_str().unwrap(),
            kaede_gc_lib_path.to_str().unwrap(),
            "-pthread",
        ])
        .status()?;
    if !status.success() {
        anyhow::bail!("Failed to emit executable file using 'cc'");
    }

    let output = Command::new(&exe_path).output()?;
    if !output.status.success() {
        anyhow::bail!("Executable terminated by signal");
    }

    let stderr = String::from_utf8_lossy(&output.stderr);
    let code = stderr
        .trim()
        .parse::<i32>()
        .map_err(|_| anyhow::anyhow!("Failed to parse test output"))?;
    Ok(code)
}

/// Execute a program and return stderr, expecting the program to abort
fn exec_expect_abort(program: &str) -> anyhow::Result<String> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context, false).unwrap();

    let file = PathBuf::from("test").into();

    let ast = Parser::new(program, file).run().unwrap();
    let ir = SemanticAnalyzer::new_for_single_file_test().analyze(ast, false, false)?;

    let module = CodeGenerator::new(&cgcx).unwrap().codegen(ir).unwrap();
    if let Some(main_fn) = module.get_function("main") {
        unsafe {
            main_fn.delete();
        }
    }

    let temp_dir = tempdir()?;
    let bitcode_path = temp_dir.path().join("test.bc");
    let obj_path = temp_dir.path().join("test.o");
    let harness_path = temp_dir.path().join("test_main.c");
    let exe_path = temp_dir.path().join("test_exec");

    module.write_bitcode_to_path(&bitcode_path);

    let status = Command::new("llc")
        .args([
            "-filetype=obj",
            "-relocation-model=pic",
            "-o",
            obj_path.to_str().unwrap(),
            bitcode_path.to_str().unwrap(),
        ])
        .status()?;
    if !status.success() {
        anyhow::bail!("Failed to emit object file using 'llc'");
    }

    let harness_src = r#"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef void (*TaskFn)(void *);

extern int32_t kdmain(void);

void kaede_runtime_init(void);
void kaede_spawn_main(TaskFn fn, void *arg, size_t arg_size);
int kaede_runtime_run(void);
void kaede_runtime_shutdown(void);
void kaede_runtime_set_exit_code(int code);

static void kaede_test_main(void *arg) {
    (void)arg;
    int32_t code = kdmain();
    kaede_runtime_set_exit_code((int)code);
}

int main(void) {
    kaede_runtime_init();
    kaede_spawn_main(kaede_test_main, NULL, 0);
    int code = kaede_runtime_run();
    kaede_runtime_shutdown();
    fprintf(stderr, "%d\n", code);
    return 0;
}
"#;

    fs::write(&harness_path, harness_src)?;

    let kaede_gc_lib_path = kaede_gc_lib_path();
    let kaede_std_lib_path = kaede_lib_path();
    let kaede_runtime_lib_path = kaede_runtime_lib_path();

    let status = Command::new("cc")
        .args([
            "-fPIE",
            "-o",
            exe_path.to_str().unwrap(),
            harness_path.to_str().unwrap(),
            obj_path.to_str().unwrap(),
            kaede_std_lib_path.to_str().unwrap(),
            kaede_runtime_lib_path.to_str().unwrap(),
            kaede_gc_lib_path.to_str().unwrap(),
            "-pthread",
        ])
        .status()?;
    if !status.success() {
        anyhow::bail!("Failed to emit executable file using 'cc'");
    }

    let output = Command::new(&exe_path).output()?;
    // We expect the program to abort, so don't check status.success()
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    Ok(stderr)
}

#[test]
fn add() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 48 + 10 }")?, 58);

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 68 - 10 }")?, 58);

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 48 * 10 }")?, 480);

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 580 / 10 }")?, 58);

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 48 + 10 * 2 }")?, 68);

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return 48 + 20 / 2 }")?, 58);

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return (48 -10/ 2) + 58 * 2 }")?, 159);

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return +(-(-58)) }")?, 58);

    Ok(())
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    let program = r"fn f() {
    }

    fn main(): i32 {
        f()
        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn return_() -> anyhow::Result<()> {
    assert_eq!(exec("fn main(): i32 { return (48*2 +10 * 2) / 2}")?, 58);

    Ok(())
}

#[test]
fn empty_return() -> anyhow::Result<()> {
    let program = r"fn f() {
        return
    }

    fn main(): i32 {
        f()
        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn let_statement() -> anyhow::Result<()> {
    // Type inference
    let program = r"fn main(): i32 {
        let x = 48
        let y = 10
        return x + y
    }";

    assert_eq!(exec(program)?, 58);

    // Mutable, Type inference
    let program = r"fn main(): i32 {
        let mut x = 58
        return x
    }";

    assert_eq!(exec(program)?, 58);

    // Specified type
    let program = r"fn main(): i32 {
        let x: i32 = 58
        return x
    }";

    assert_eq!(exec(program)?, 58);

    // Mutable, Specified type
    let program = r"fn main(): i32 {
        let mut x: i32 = 58
        return x
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn call_function() -> anyhow::Result<()> {
    let program = r"fn f1(): i32 {
        return 48
    }

    fn f2(): i32 {
        return 10
    }

    fn main(): i32 {
        return f1() + f2()
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn function_parameters() -> anyhow::Result<()> {
    let program = r"fn f(n: i32): i32 {
        return n
    }

    fn main(): i32 {
        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn function_call_with_one_argument() -> anyhow::Result<()> {
    let program = r"fn f(n: i32): i32 {
        return n
    }

    fn main(): i32 {
        return f(58)
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn function_call_with_multi_args() -> anyhow::Result<()> {
    let program = r"fn f(x: i32, y: i32): i32 {
        return x + y
    }

    fn main(): i32 {
        return f(48, 10)
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn function_call_with_keyword_args() -> anyhow::Result<()> {
    let program = r"fn f(x: i32, y: i32, z: i32): i32 {
        return x * y + z
    }

    fn main(): i32 {
        // Reordered keyword arguments should map correctly.
        return f(z=8, x=50, y=0)
    }";

    assert_eq!(exec(program)?, 8);

    Ok(())
}

#[test]
fn simple_if() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 58 == 58 {
            return 58
        }

        return 123
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_else() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 48 == 10 {
            return 48
        } else if
        58 == 58 {
            return 58
        } else
        {
            return 10
        }

        return 123
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn equality_operation() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 4810 == 4810 {
            return 58
        }

        return 123
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn loop_() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut n = 0

        loop {
            if n == 58 {
                break
            }

            n = n + 1
        }

        return n
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn while_() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut n = 0

        while n < 10 {
            n = n + 1
        }

        return n
    }";

    assert_eq!(exec(program)?, 10);

    Ok(())
}

#[test]
fn break_() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        loop {
            break
        }

        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn break_outside_of_loop() {
    let program = r"fn main(): i32 {
        break
    }";

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::BreakOutsideOfLoop { .. }
    ));
}

#[test]
fn simple_assignment() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut n = 0

        n = 58

        return n
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable() {
    let program = r"fn main(): i32 {
        let n = 58
        n = 4810
        return n
    }";

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignTwiceToImutable { .. }
    ));
}

#[test]
fn string_literal() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let s1 = "hello, world"
        let s2 = "こんにちは"

        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn builtin_format() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let s = __format("a{}b{}", "x", "yz")
        return s.len() as i32
    }"#;

    assert_eq!(exec(program)?, 5);
    Ok(())
}

#[test]
fn builtin_format_requires_literal_template() {
    let program = r#"fn main(): i32 {
        let t = "{}"
        let _ = __format(t, "x")
        return 0
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::FormatTemplateMustBeStringLiteral { .. }
    ));
}

#[test]
fn builtin_format_requires_matching_placeholder_count() {
    let program = r#"fn main(): i32 {
        let _ = __format("{} {}", "x")
        return 0
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::FormatPlaceholderCountMismatch { .. }
    ));
}

#[test]
fn format_is_undeclared() {
    let program = r#"fn main(): i32 {
        let _ = format("{}", "x")
        return 0
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::Undeclared { name, .. } if name.as_str() == "format"
    ));
}

#[test]
fn interpolated_string() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let name = "xy"
        let s = $"a{name}b"
        return s.len() as i32
    }"#;

    assert_eq!(exec(program)?, 4);
    Ok(())
}

#[test]
fn byte_string_literal() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let bs = b"hi"
        return (bs[0] as i32) + (bs[1] as i32)
    }"#;

    assert_eq!(exec(program)?, 209);

    Ok(())
}

#[test]
fn byte_string_literal_with_escapes() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let bs = b"a\n\\\""
        return (bs[0] as i32) + (bs[1] as i32) + (bs[2] as i32) + (bs[3] as i32)
    }"#;

    assert_eq!(exec(program)?, 233); // 'a' (97) + '\n' (10) + '\\' (92) + '"' (34)
    Ok(())
}

#[test]
fn byte_literal() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let b = b'A'
        return b as i32
    }"#;

    assert_eq!(exec(program)?, 65); // 'A' is 65
    Ok(())
}

#[test]
fn byte_literal_escape_sequences() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let newline = b'\n'
        let tab = b'\t'
        let backslash = b'\\'
        let quote = b'\''
        return (newline as i32) + (tab as i32) + (backslash as i32) + (quote as i32)
    }"#;

    assert_eq!(exec(program)?, 10 + 9 + 92 + 39); // '\n' (10) + '\t' (9) + '\\' (92) + '\'' (39)
    Ok(())
}

#[test]
fn byte_literal_arithmetic() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let a = b'a'
        let z = b'z'
        return (z - a) as i32
    }"#;

    assert_eq!(exec(program)?, 25); // 'z' (122) - 'a' (97) = 25
    Ok(())
}

#[test]
fn string_type() -> anyhow::Result<()> {
    let program = r#"fn f(s: str): str {
        return s
    }

    fn main(): i32 {
        let s: str = f("Yohaio")
        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn define_struct() -> anyhow::Result<()> {
    let program = r"struct A {
        a: i32,
        b: bool,
    }

    struct B { a: i32, b: bool }

    fn main(): i32 {
        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn struct_field_access() -> anyhow::Result<()> {
    let program = r"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn main(): i32 {
        let person = Person { is_male: false, stature: 48, age: 10, is_female: true }
        return person.age + person.stature
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn true_() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let b = true

        if b {
            return 58
        }

        return 123
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn false_() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let b = false

        if b {
            return 123
        }

        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn has_no_fields() {
    let program = r"fn main(): i32 {
        4810.shino
    }";

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::HasNoFields { .. }
    ));
}

#[test]
fn less_than() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 48 < 48 {
            return 123
        } else if 48 < 10 {
            return 124
        } else if 10 < 48 {
            return 58
        }

        return 125
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn greater_than() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 10 > 10 {
            return 123
        } else if 10 > 48 {
            return 124
        } else if 48 > 10 {
            return 58
        }

        return 125
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn less_than_or_equal() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 48 <= 48 {
            return 58
        } else {
            return 123
        }

        return 86
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn greater_than_or_equal() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 10 >= 10 {
            return 58
        } else {
            return 123
        }

        return 86
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn not_equal_to() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if 48 != 48 {
            return 123
        } else if 48 != 10 {
            return 58
        }

        return 124
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn logical_not() -> anyhow::Result<()> {
    let program = r"fn f(fl: bool): bool {
        return !fl
    }

    fn main(): i32 {
        if !(48 != 10) {
            return 123
        }

        let fls = false;

        if f(fls) {
            return 58
        }

        return 124
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn remainder() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        return 123 % 7
    }";

    assert_eq!(exec(program)?, 4);

    Ok(())
}

#[test]
fn bit_and_or_xor() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let a = 5 & 3
            let b = 5 | 2
            let c = 5 ^ 1
            return a + b + c
        }
    "#;

    assert_eq!(exec(program)?, 12);
    Ok(())
}

#[test]
fn bit_shift() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let a = 3 << 2
            let b = 16 >> 2
            return a + b
        }
    "#;

    assert_eq!(exec(program)?, 16);
    Ok(())
}

#[test]
fn bit_not() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            return ~0
        }
    "#;

    assert_eq!(exec(program)?, -1);
    Ok(())
}

#[test]
fn bit_operator_precedence() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let a = 1 + 2 << 3
            let b = 8 >> 1 & 3
            let c = 1 | 2 ^ 3 & 4
            return a + b + c
        }
    "#;

    assert_eq!(exec(program)?, 27);
    Ok(())
}

#[test]
fn logical_or() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if false || false {
            return 123
        }
        if false || true {
            if true || false {
                if true || true {
                    if 48 < 10 || 48 != 10 {
                        return 58
                    }
                }
            }
        }
        return 124
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn logical_and() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        if false && true {
            return 123
        }
        if true && false {
            return 124
        }
        if false && false {
            return 125
        }
        if true && true {
            if 48 > 10 && 48 != 10 {
                return 58
            }
        }
        return 126
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn array_literal() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let a = [48, 10]
        return 58
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn array_repeat() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let a = [42; 3]
        return a[0] + a[1] + a[2]
    }";

    assert_eq!(exec(program)?, 126);

    Ok(())
}

#[test]
fn array_repeat_value_evaluated_once() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut x = 0
        let arr = [{ x = x + 1; x }; 4]
        return x * 10 + arr[3]
    }";

    assert_eq!(exec(program)?, 11);

    Ok(())
}

#[test]
fn array_repeat_with_type_annotation() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let arr: [i32; 2] = [0; 2]
        return arr[0] + arr[1]
    }";

    assert_eq!(exec(program)?, 0);

    Ok(())
}

#[test]
fn array_indexing() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let a = [48, 10]
        return a[0] + [4][0] + a[1]
    }";

    assert_eq!(exec(program)?, 62);

    Ok(())
}

#[test]
fn array_type() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let a: [i32; 2] = [48, 10]
        let n: [i32; 1] = [4]
        return a[0] + a[1] + n[0]
    }";

    assert_eq!(exec(program)?, 62);

    Ok(())
}

#[test]
fn array_as_argument() -> anyhow::Result<()> {
    let program = r"fn add(a: [i32; 2]): i32 {
        return a[0] + a[1]
    }

    fn main(): i32 {
        let a = [48, 10]

        return add(a)
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn array_as_mutable_argument() -> anyhow::Result<()> {
    let program = r"fn modify(a: mut [i32; 2]) {
        a[0] = 48
        a[1] = 10
    }

    fn add(a: [i32; 2]): i32 {
        return a[0] + a[1]
    }

    fn main(): i32 {
        let mut a = [12, 34]

        modify(a)

        return add(a)
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn immutable_array_as_mutable_argument() {
    let program = r"fn modify(a: mut [i32; 2]) {
        a[0] = 48
        a[1] = 10
    }

    fn add(a: [i32; 2]): i32 {
        return a[0] + a[1]
    }

    fn main(): i32 {
        let a = [123, 124]

        // ERROR!
        modify(a)

        return add(a)
    }";

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignImmutableToMutable { .. }
    ));
}

#[test]
fn assign_to_array_elements() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut a = [123]

        a[0] = 58

        return a[0]
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn tuple_indexing() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let tup = (58, true, "hello")

        if tup.1 {
            return tup.0
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn tuple_unpacking() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let tup = (48, true, "hello", 10)

        let (n1, f, _, n2) = tup

        if f {
            return n1 + n2
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    let program = r#"fn main(): i32 {
        let (n1, f, _, n2) = (48, true, "hello", 10)

        if f {
            return n1 + n2
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn tuple_as_argument() -> anyhow::Result<()> {
    let program = r"fn tup1(a: (i32, bool)): i32 {
        if a.1 {
            return a.0
        }

        return 123
    }

    fn tup2(a: (i32, bool)): i32 {
        let (n, f) = a

        if f {
            return n
        }

        return 124
    }

    fn main(): i32 {
        let tup = (58, true)

        return tup1(tup) + tup2(tup)
    }";

    assert_eq!(exec(program)?, 116);

    Ok(())
}

#[test]
fn tuple_as_mutable_argument() -> anyhow::Result<()> {
    let program = r"fn modify(tup: mut (i32, bool)) {
        tup.0 = 58
        tup.1 = true
    }

    fn main(): i32 {
        let mut tup = (123, false)

        modify(tup)

        if tup.1 {
            return tup.0
        }

        return 123
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn tuples_require_access_by_index() {
    let program = r"fn main(): i32 {
        let tup = (58, true)
        tup.llvm
    }";

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::TupleRequireAccessByIndex { .. }
    ));
}

#[test]
fn tuple_in_tuple() -> anyhow::Result<()> {
    let program = r"fn main(): i32 {
        let mut tuptup = ((48, true), (0, true))

        let mut io = tuptup.1
        io.0 = 10

        return tuptup.0.0 + tuptup.1.0
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_struct_field() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn main(): i32 {
        let mut person = Person { is_male: false, stature: 48, age: 0, is_female: true }
        person.age = 10
        return person.age + person.stature
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_struct_field() {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn main(): i32 {
        let person = Person { is_male: false, stature: 48, age: 0, is_female: true }
        person.age = 10
        return person.age + person.stature
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignTwiceToImutable { .. }
    ));
}

#[test]
fn assign_to_tuple_field() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let mut t = (58, false)
        t.1 = true

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_tuple_field() {
    let program = r#"fn main(): i32 {
        let t = (58, false)
        t.1 = true

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignTwiceToImutable { .. }
    ));
}

#[test]
fn comments() -> anyhow::Result<()> {
    let program = r"// hello, world
    /* hello, world */
    /*
    hello, world
    world, hello
     */
    fn main(): i32 {
        // hello, world
        /* hello, world */
        /*
        hello, world
        world, hello
         */

        let n /* Number */ = 58 // Let

        return /* My name is John Smith */ n // Return

        // hello, world
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_1() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let x = if true {
            58
        } else {
            123
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_2() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let x = if true {
            58
        } else {
            return 123
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_3() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let x = if false {
            123
        } else {
            return 58
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_4() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 4810

        let x = if false {
            123
        } else if n == 4810 {
            58
        } else {
            return 123
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_1() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        return if true {
            58
        } else {
            123
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_2() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        return if false {
            123
        } else {
            return 58
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_3() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        return 122 + if true {
            return 58
        } else {
            1
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn nested_if_expr_1() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 4810

        let x = if n == 4810 {
            if false {
                123
            } else if true {
                58
            } else {
                return 124
            }
        } else if true {
            125
        } else {
            return 126
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn nested_if_expr_2() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 4810

        let x = if false {
            123
        } else if true {
            if false {
                124
            } else if n != 4810 {
                125
            } else {
                return 58
            }
        } else {
            return 126
        }

        return x
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn copy_struct() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn main(): i32 {
        let mut p1 = Person {
            is_male: false,
            stature: 48,
            age: 0,
            is_female: true,
        }

        let mut p2 = p1

        p2.age = 10

        return p1.age + p1.stature
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn copy_array() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let mut ar = [128, 256, 512]

        let mut arr = ar

        arr[0] = 58

        return ar[0]
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn copy_tuple() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let mut tup = (false, 123)

        let mut tupp = tup

        tupp.0 = true
        tupp.1 = 58

        if tup.0 {
            return tup.1
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn struct_as_mutable_argument() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn f(p: mut Person) {
        p.age = 10
    }

    fn main(): i32 {
        let mut p1 = Person {
            is_male: false,
            stature: 48,
            age: 0,
            is_female: true,
        }

        f(p1)

        return p1.age + p1.stature
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn struct_in_struct() -> anyhow::Result<()> {
    let program = r"struct Age {
       n: i32,
    }

    struct Person {
        age: Age,
    }

    fn main(): i32 {
        let mut p = Person {
            age: Age { n: 0 },
        }

        let mut age = p.age
        age.n = 58

        return p.age.n
    }";

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn copy_scalar_type_data() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 58

        let mut m = n
        m = 123

        return n
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_to_mutable() {
    // Array
    let program = r#"fn main(): i32 {
        let ar = [128, 256, 512]

        let mut arr = ar

        return ar[0]
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignImmutableToMutable { .. }
    ));

    // Tuple
    let program = r#"fn main(): i32 {
        let tup = (58, true)

        let mut t = tup

        return tup.0
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignImmutableToMutable { .. }
    ));

    // Struct
    let program = r#"struct Number {
        n: i32,
    }

    fn main(): i32 {
        let n = Number { n: 58 }

        let mut m = n

        return n.n
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignImmutableToMutable { .. }
    ));
}

#[test]
fn return_struct() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    fn f(): Person {
        return Person { age: 58 }
    }

    fn main(): i32 {
        let s = f()

        return s.age
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn struct_literal_field_init_shorthand() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
    }

    fn main(): i32 {
        let age = 10
        let stature = 48
        let person = Person { age, stature }
        return person.age + person.stature
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn return_tuple() -> anyhow::Result<()> {
    let program = r#"fn f(): (i32, bool) {
        return (58, true)
    }

    fn main(): i32 {
        let t = f()

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn return_array() -> anyhow::Result<()> {
    let program = r#"fn f(): [i32; 2] {
        return [48, 10]
    }

    fn main(): i32 {
        let ar = f()

        return ar[0] + ar[1]
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn array_to_slice_param_codegen() -> anyhow::Result<()> {
    let program = r#"fn sum(s: [i32]): i32 {
        let mut sum = 0
        let mut i = 0

        loop {
            if i == s.len() {
                break
            }
            sum += s[i]
            i += 1
        }

        return sum
    }

    fn main(): i32 {
        let ar1: [i32; 2] = [1, 2]
        let ar2 = [10, 20, 30]

        return sum(ar1) + sum(ar2)
    }"#;

    assert_eq!(exec(program)?, 63);

    Ok(())
}

#[test]
fn array_to_slice_return_codegen() -> anyhow::Result<()> {
    let program = r#"fn make(): [i32] {
        return [48, 10]
    }

    fn main(): i32 {
        let s = make()

        return s[0] + s[1]
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn slice_len_method_codegen() -> anyhow::Result<()> {
    let program = r#"fn len_of(s: [i32]): u64 {
        return s.len()
    }

    fn main(): i32 {
        let ar = [5, 6, 7]
        return len_of(ar) as i32
    }"#;

    assert_eq!(exec(program)?, 3);

    Ok(())
}

#[test]
fn slice_as_ptr_method_codegen() -> anyhow::Result<()> {
    let program = r#"fn first_plus_len(s: [i32]): i32 {
        let ptr = s.as_ptr()
        return ptr[0] + s.len() as i32
    }

    fn main(): i32 {
        let ar = [10, 20]
        return first_plus_len(ar)
    }"#;

    assert_eq!(exec(program)?, 12);

    Ok(())
}

#[test]
fn slice_full_range_codegen() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let ar = [1, 2, 3, 4]
        let s = ar[:]
        return s.len() as i32 + s[0] + s[3]
    }"#;

    assert_eq!(exec(program)?, 9); // 4(len) + 1 + 4

    Ok(())
}

#[test]
fn slice_range_codegen() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let ar = [5, 6, 7, 8, 9]
        let s = ar[1:4] // 6,7,8
        return s.len() as i32 + s[0] + s[2]
    }"#;

    assert_eq!(exec(program)?, 17); // 3 + 6 + 8

    Ok(())
}

#[test]
fn slice_trailing_and_nested_codegen() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let ar = [10, 20, 30, 40]
        let mid = ar[1:4] // 20,30,40
        let tail = mid[1:] // 30,40
        return tail[0] + tail.len() as i32
    }"#;

    assert_eq!(exec(program)?, 32); // 30 + 2

    Ok(())
}

#[test]
fn slice_empty_codegen() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let ar = [1, 2, 3]
        let empty = ar[2:2]
        return empty.len() as i32
    }"#;

    assert_eq!(exec(program)?, 0);

    Ok(())
}

#[test]
fn if_in_loop() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    fn get_age(p: Person): i32 {
        return p.age
    }

    fn main(): i32 {
        let mut p = Person { age: 0 }

        loop {
            if get_age(p) == 58 {
                break
            } else {
                p.age = p.age + 1
            }
        }

        return get_age(p)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn simple_method() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        fn get_age(self): i32 {
            return self.age
        }
    }

    fn main(): i32 {
        let mut p = Person { age: 123 }

        p.age = 58

        return p.get_age()
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn mutable_method() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        fn get_age(self): i32 {
            return self.age
        }

        fn change_age_to(mut self, new_age: i32) {
            self.age = new_age
        }
    }

    fn main(): i32 {
        let mut p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn call_mutable_methods_from_immutable() {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        fn get_age(self): i32 {
            return self.age
        }

        fn change_age_to(mut self, new_age: i32) {
            self.age = new_age
        }
    }

    fn main(): i32 {
        let p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotCallMutableMethodOnImmutableValue { .. }
    ));
}

#[test]
fn modify_fields_in_immutable_methods() {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        fn get_age(self): i32 {
            return self.age
        }

        fn change_age_to(self, new_age: i32) {
            self.age = new_age
        }
    }

    fn main(): i32 {
        let mut p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::CannotAssignTwiceToImutable { .. }
    ));
}

#[test]
fn static_method() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        fn new(age: i32): Person {
            return Person { age: age }
        }

        fn get_age(self): i32 {
            return self.age
        }
    }

    fn main(): i32 {
        let p = Person::new(58)

        return p.get_age()
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn create_simple_enum() -> anyhow::Result<()> {
    let program = r#"enum Simple {
        A,
        B,
        C,
    }

    fn main(): i32 {
        let a = Simple::A
        let b = Simple::B
        let c = Simple::C

        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn create_tagged_enum() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn main(): i32 {
        let e1 = E::A
        let e2 = E::B(Fruits { apple: 48, ichigo: 10 })
        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_stmt_on_enum() -> anyhow::Result<()> {
    let program = r#"enum E {
        A,
        B,
    }

    fn f(): i32 {
        let e = E::B

        match e {
            E::A => return 123,
            E::B => return 58,
        }

        return 124
    }

    fn main(): i32 {
        let e = E::A

        match e {
            E::B => return 125,
            E::A => return f(),
        }

        return 126
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_enum() -> anyhow::Result<()> {
    let program = r#"enum E {
        A,
        B,
    }

    fn main(): i32 {
        let e = E::A

        return match e {
            E::A => 58,
            E::B => return 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_enum_with_wildcard() -> anyhow::Result<()> {
    let program = r#"enum E {
        A,
        B,
    }

    fn main(): i32 {
        let e = E::A

        return match e {
            E::B => return 123,
            _ => 58,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_tagged_enum() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn main(): i32 {
        let e = E::B(Fruits { apple: 48, ichigo: 10 })

        return match e {
            E::A => 123,
            E::B(fr) => fr.apple + fr.ichigo,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_tagged_enum_to_discard_value() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn main(): i32 {
        let e = E::B(Fruits { apple: 48, ichigo: 10 })

        return match e {
            E::A => 123,
            E::B(_) => 58,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_tagged_enum_with_wildcard() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn main(): i32 {
        let e = E::B(Fruits { apple: 48, ichigo: 10 })

        return match e {
            E::A => 123,
            _ => 58,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_int_without_wildcard() {
    let program = r#"fn main(): i32 {
        let n = 25

        return match n {
            48 => 10,
            10 => 48,
            25 => 58,
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MatchNotExhaustive { .. }
    ));
}

#[test]
fn match_on_int_with_wildcard() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 58

        return match n {
            48 => 10,
            10 => 48,
            58 => 48 + 10,
            _ => 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_bool_without_true() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let flag = false

        return match flag {
            false => 58,
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MatchNotExhaustive { .. }
    ));

    Ok(())
}

#[test]
fn match_on_bool_without_false() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let flag = true

        return match flag {
            true => 58,
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MatchNotExhaustive { .. }
    ));

    Ok(())
}

#[test]
fn match_on_bool() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let flag = true

        return match flag {
            true => 58,
            false => 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_on_bool_with_wildcard() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let flag = true

        return match flag {
            false => 123,
            _ => 58,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn non_exhaustive_patterns() {
    let program = r#"enum E {
        A,
        B,
        C,
    }

    fn main(): i32 {
        let e = E::A

        return match e {
            E::A => 58,
            E::B => return 123,
            // There is no pattern covering E::C!
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MatchNotExhaustive { .. }
    ));
}

#[test]
fn duplicate_pattern() {
    let program = r#"enum E {
        A,
        B,
    }

    fn main(): i32 {
        let e = E::A

        return match e {
            E::A => 58,
            E::A => 58,
            E::B => return 123,
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::DuplicatePattern { .. }
    ));
}

#[test]
fn enum_as_argument() -> anyhow::Result<()> {
    let program = r#"enum E {
        A,
        B,
        C,
    }

    fn f(e: E): i32 {
        return match e {
            E::A => 123,
            E::B => 124,
            E::C => 58,
        }
    }

    fn main(): i32 {
        return f(E::C)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn tagged_enum_as_argument() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn sum_fruits(e: E): i32 {
        return match e {
            E::A => 116,
            E::B(fruits) => fruits.apple + fruits.ichigo,
        }
    }

    fn main(): i32 {
        let e1 = E::A
        let e2 = E::B(Fruits { apple: 48, ichigo: 10 })
        return sum_fruits(e1) - sum_fruits(e2)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_with_single_parameter() -> anyhow::Result<()> {
    let program = r#"fn add_10<T>(n: T):T {
        return n + 10
    }

    fn main(): i32 {
        return add_10<i32>(48)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_with_multiple_parameters() -> anyhow::Result<()> {
    let program = r#"fn add_or_mul<T1, T2, SwitchT>(n1: T1, n2: T2, switcher: SwitchT): T1 {
        return if switcher {
            n1 + n2
        } else {
            n1 * n2
        }
    }

    fn main(): i32 {
        return add_or_mul<i32, i32, bool>(48, 10, true)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_param_inference() -> anyhow::Result<()> {
    let program = r#"fn add_or_mul<T1, T2, SwitchT>(n1: T1, n2: T2, switcher: SwitchT): T1 {
        return if switcher {
            n1 + n2
        } else {
            n1 * n2
        }
    }

    fn main(): i32 {
        return add_or_mul(48, 10, true)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_with_struct_type() -> anyhow::Result<()> {
    let program = r#"struct Sample {
        n: i32,
    }

    fn get_n<S>(o: S): i32 {
        return o.n
    }

    fn main(): i32 {
        let s = Sample { n: 58 }
        return get_n<Sample>(s)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_with_tuple_type() -> anyhow::Result<()> {
    let program = r#"fn get_third<T>(t: T): i32 {
        return t.2
    }

    fn main(): i32 {
        let tup = (48, 10, 58)
        return get_third<(i32, i32, i32)>(tup)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_function_with_array_type() -> anyhow::Result<()> {
    let program = r#"fn get_third<T>(a: T): i32 {
        return a[2]
    }

    fn main(): i32 {
        let a = [48, 10, 58]
        return get_third<[i32; 3]>(a)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_struct_with_single_parameter() -> anyhow::Result<()> {
    let program = r#"struct A<T> {
        value: T,
    }
    fn main(): i32 {
        let a = A<i32> { value: 58 }

        return a.value
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_struct_as_function_argument() -> anyhow::Result<()> {
    let program = r#"struct Sample<T> {
        n: T,
    }

    fn get_n(s: Sample<i32>): i32 {
        return s.n
    }

    fn main(): i32 {
        let s = Sample<i32> { n: 58 }
        return get_n(s)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_struct_with_multiple_parameters() -> anyhow::Result<()> {
    let program = r#"struct Fruits<T1, T2> {
        apple: T1,
        ichigo: T2,
    }
    struct A<T> {
        value: T,
    }
    struct Number {
        value: i32,
    }
    fn main(): i32 {
        let fr1 = Fruits<i32, i32> { apple: 48, ichigo: 10 }
        let fr2 = Fruits<A<i32>, Number> { apple: A<i32> { value: 48 }, ichigo: Number { value: 10 } }

        let fr1_sum = fr1.apple + fr1.ichigo
        let fr2_sum = fr2.apple.value + fr2.ichigo.value

        if fr1_sum == fr2_sum {
            return fr2_sum
        }

        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn block() -> anyhow::Result<()> {
    let program = r#"fn f(): i32 {
        return 58
    }
    fn main(): i32 {
        let n = {
            let a = 48
            let b = 10

            if f() != (a + b) {
                return 123
            }

            a + b
        } * 2

        return n / 2
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_with_block() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let n = 58

        return match n {
            58 => {
                let mut a = 29
                a = a * 2
                a
            },
            _ => 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn match_unpack_unit_variant() {
    let program = r#"enum E {
        A,
        B,
    }

    fn main(): i32 {
        let e = E::A

        return match e {
            E::A(n) => 123,
            E::B => 124,
        }
    }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::UnitVariantCannotUnpack { .. }
    ));
}

#[test]
fn c_ffi() -> anyhow::Result<()> {
    let program = r#"extern "C" fn abs(n: i32): i32

    fn main(): i32 {
        return abs(-48) + abs(10)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn c_ffi_with_ptr_and_vararg() -> anyhow::Result<()> {
    let program = r#"extern "C" fn printf(format: *i8, ...): i32

    fn main(): i32 {
        let format = "%s%d\n"
        let n = printf(format.as_ptr(), "hello, ".as_ptr(), 58)
        return 48 + n
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn method_for_i32() -> anyhow::Result<()> {
    let program = r#"impl i32 {
        fn twice(self): i32 {
            return self * 2
        }

        fn twice_then_add(self, n: i32): i32 {
            return self.twice() + n
        }
    }

    fn main(): i32 {
        let n = 14.twice_then_add(1)
        return n.twice()
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn cast_integer() -> anyhow::Result<()> {
    let program = r#"fn f(): i64 {
        let n = 58
        return n as i64
    }

    fn main(): i32 {
        return f() as i32
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn all_integer_types_arithmetic_and_casts() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let i8v: i8 = 1
        let u8v: u8 = 2
        let i16v: i16 = 3
        let u16v: u16 = 4
        let i32v: i32 = 5
        let u32v: u32 = 6
        let i64v: i64 = 7
        let u64v: u64 = 30

        let total = (i8v as i64) +
            (u8v as i64) +
            (i16v as i64) +
            (u16v as i64) +
            (i32v as i64) +
            (u32v as i64) +
            i64v +
            (u64v as i64)

        return total as i32
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn cast_str_to_pointer() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let s = "hello, world" as *str as *i8
        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn single_variant_enum() -> anyhow::Result<()> {
    let program = r#"struct C {
        apple: i32,
    }

    enum A {
        B(C),
    }

    fn main(): i32 {
        let a = A::B(C { apple: 58 })

        return match a {
            A::B(c) => c.apple,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_enum_with_single_parameter() -> anyhow::Result<()> {
    let program = r#"enum Opt<T> {
        Some(T),
        None,
    }

    fn main(): i32 {
        let a = Opt<i32>::Some(58)

        return match a {
            Opt::Some(n) => n,
            Opt::None => 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_enum_as_function_argument() -> anyhow::Result<()> {
    let program = r#"enum Opt<T> {
        Some(T),
        None,
    }

    fn get(opt: Opt<i32>): i32 {
        return match opt {
            Opt::Some(n) => n,
            Opt::None => 123,
        }
    }

    fn main(): i32 {
        let opt = Opt<i32>::Some(58)
        return get(opt)
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_enum_with_multiple_parameters() -> anyhow::Result<()> {
    let program = r#"enum Res<T1, T2> {
        Ok(T1),
        Err(T2),
    }

    fn main(): i32 {
        let res1 = Res<i32, bool>::Err(false)
        let res2 = Res<i32, bool>::Ok(58)

        match res1 {
            Res::Ok(_) => return 123,
            Res::Err(f) => {
                if f {
                    return 123
                }
            },
        }

        return match res2 {
            Res::Ok(n) => n,
            Res::Err(_) => 123,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn omit_comma_at_end_of_struct_fields() -> anyhow::Result<()> {
    let program = r#"struct S {
        n: i32,
        m: i32
    }

    fn main(): i32 {
        let a = S { n: 48, m: 10 }
        return a.n + a.m
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn omit_comma_at_end_of_enum_variants() -> anyhow::Result<()> {
    let program = r#"enum A {
        B,
        C(i32)
    }

    fn main(): i32 {
        let a = A::C(58)

        return match a {
            A::B => 123,
            A::C(n) => 58,
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn omit_comma_at_end_of_match_arms() -> anyhow::Result<()> {
    let program = r#"enum A {
        B,
        C(i32),
    }

    fn main(): i32 {
        let a = A::C(58)

        return match a {
            A::B => 123,
            A::C(n) => 58
        }
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn less_than_with_variables() -> anyhow::Result<()> {
    let program = r#"fn main(): i32 {
        let a = 48
        let b = 10

        // This is a parser test.
        // Below is not a<b> but a < b
        if a < b {
            return 123
        }

        return 58
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn static_method_with_no_args() -> anyhow::Result<()> {
    let program = r#"struct A {
        size: i32,
    }
    impl A {
        fn new(): A {
            return A { size: 58 }
        }
        fn get_size(self): i32 {
            return self.size
        }
    }
    fn main(): i32 {
        let a = A::new()
        return a.get_size()
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn generic_method() -> anyhow::Result<()> {
    let program = r#"struct A<T> {
        value: T,
    }
    impl<T> A<T> {
        fn new(value: T): A<T> {
            return A<T> { value: value }
        }
        fn get_value(self): T {
            return self.value
        }
        fn is_equal(self, other: A<T>): bool {
            return self.value == other.value
        }
    }
    fn main(): i32 {
        let a = A<i32>::new(58)
        if a.is_equal(A<i32>::new(58)) {
            return a.get_value()
        }
        return 123
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn complex_generic_struct_method() -> anyhow::Result<()> {
    let program = r#"pub enum MyOption<T> {
        Some(T),
        None,
    }

    struct MyList<T> {
        value: MyOption<T>,
        next: MyOption<MyList<T>>,
    }

    impl<T> MyList<T> {
        fn new(): mut MyList<T> {
            return MyList<T> { value: MyOption<T>::None, next: MyOption<MyList<T>>::None }
        }

        fn len(self): u32 {
            return match self.next {
                MyOption::Some(ne) => (1 as u32) + ne.len(),
                MyOption::None => {
                    match self.value {
                        MyOption::Some(_) => 1,
                        MyOption::None => 0
                    }
                }
            }
        }

        fn push(mut self, elem: T) {
            match self.next {
                MyOption::Some(ne) => {
                    ne.push(elem)
                },

                MyOption::None => {
                    match self.value {
                        MyOption::Some(_) => {
                            let tmp = MyList<T> { value: MyOption<T>::Some(elem), next: MyOption<MyList<T>>::None }
                            self.next = MyOption<MyList<T>>::Some(tmp)
                        },
                        MyOption::None => {
                            // For the first push
                            self.value = MyOption<T>::Some(elem)
                        }
                    }
                }
            }
        }

        fn at(self, idx: u32): MyOption<T> {
            return match self.next {
                MyOption::Some(ne) => {
                    if idx == (0 as u32) {
                        self.value
                    } else {
                        ne.at(idx - (1 as u32))
                    }
                },

                MyOption::None => {
                    self.value
                }
            }
        }
    }

    fn main(): i32 {
        let mut v = MyList<i32>::new()
        v.push(48)
        v.push(10)

        let mut i = 0 as u32
        let mut len = v.len()

        let mut sum = 0

        loop {
            if i == len {
                break
            }

            sum = sum + match v.at(i) {
                MyOption::Some(tmp) => tmp,
                MyOption::None => break,
            }

            i = i + (1 as u32)
        }

        return sum
    }"#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn recursive_function() -> anyhow::Result<()> {
    let program = r#"
        fn f(n: i32): i32 {
            if n == 10 {
                return n
            }

            return f(n + 1)
        }

        fn main(): i32 {
            return f(0)
        }
    "#;

    assert_eq!(exec(program)?, 10);

    Ok(())
}

#[test]
fn prelude() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut v = Vector<i32>::new()
            v.push(58)
            return match v.at(0) {
                Option::Some(n) => n,
                Option::None => 123,
            }
        }
    "#;

    assert_eq!(exec(program)?, 58);

    Ok(())
}

#[test]
fn hashmap_basic_crud() -> anyhow::Result<()> {
    let program = r#"
        import std.collections

        use std.collections.hash_str

        fn eq_str(a: str, b: str): bool {
            return a == b
        }

        fn main(): i32 {
            let mut m = HashMap<str, i32>::new(hash_str, eq_str)
            if !m.is_empty() {
                return 1
            }
            if m.len() != 0 {
                return 2
            }

            let prev1 = m.insert("alpha", 10)
            if match prev1 { Option::Some(_) => true, Option::None => false } {
                return 3
            }

            let got = match m.get("alpha") {
                Option::Some(v) => v,
                Option::None => return 4,
            }
            if got != 10 {
                return 5
            }
            if !m.contains("alpha") {
                return 6
            }

            let removed = match m.remove("alpha") {
                Option::Some(v) => v,
                Option::None => return 7,
            }
            if removed != 10 {
                return 8
            }
            if m.contains("alpha") {
                return 9
            }
            if match m.get("alpha") { Option::Some(_) => true, Option::None => false } {
                return 10
            }
            if match m.remove("alpha") { Option::Some(_) => true, Option::None => false } {
                return 11
            }

            return 58
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn hashmap_overwrite_returns_old_value() -> anyhow::Result<()> {
    let program = r#"
        import std.collections

        use std.collections.hash_str

        fn eq_str(a: str, b: str): bool {
            return a == b
        }

        fn main(): i32 {
            let mut m = HashMap<str, i32>::new(hash_str, eq_str)

            let r1 = m.insert("k", 11)
            if match r1 { Option::Some(_) => true, Option::None => false } {
                return 1
            }

            let r2 = m.insert("k", 22)
            let old = match r2 {
                Option::Some(v) => v,
                Option::None => return 2,
            }
            if old != 11 {
                return 3
            }
            if m.len() != 1 {
                return 4
            }

            return match m.get("k") {
                Option::Some(v) => v,
                Option::None => 5,
            }
        }
    "#;

    assert_eq!(exec(program)?, 22);
    Ok(())
}

#[test]
fn hashmap_collision_and_rehash() -> anyhow::Result<()> {
    let program = r#"
        import std.collections

        fn hash_i32_const(n: i32): u64 {
            if n == -2147483648 {
                return 1
            }
            return 1
        }

        fn eq_i32(a: i32, b: i32): bool {
            return a == b
        }

        fn main(): i32 {
            let mut m = HashMap<i32, i32>::with_capacity(8, hash_i32_const, eq_i32)

            let mut i = 0
            while i < 20 {
                m.insert(i, i + 100)
                i += 1
            }

            let mut sum = 0
            let mut j = 0
            while j < 20 {
                let v = match m.get(j) {
                    Option::Some(x) => x,
                    Option::None => return 1,
                }
                sum += v
                j += 1
            }

            // 100 + ... + 119 = 2190
            if sum != 2190 {
                return 2
            }

            let r = match m.remove(5) {
                Option::Some(x) => x,
                Option::None => return 3,
            }
            if r != 105 {
                return 4
            }
            if m.contains(5) {
                return 5
            }

            return 58
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn string_indexing() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let s = "Hello, World!"
            return s[0] as i32
        }
    "#;

    assert_eq!(exec(program)?, 72);

    Ok(())
}

#[test]
fn char_type() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let c1 = 'a'
            let c2 = "abc"[0]
            if c1 == c2 {
                return 123
            }
            return 256
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn match_with_catch_all_and_non_catch_all() {
    let program = r#"
        fn main(): i32 {
            let n = 123
            return match n {
                _ => 124
            }
        }
    "#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MatchMustHaveNonCatchAllArm { .. }
    ));
}

#[test]
fn match_with_catch_all() -> anyhow::Result<()> {
    let program = r#"
        enum E {
            A,
            B,
            C,
        }

        fn f(): i32 {
            return 123
        }

        fn main(): i32 {
            let e = E::C
            return match e {
                E::A => 124,
                E::B => 125,
                _ => {
                    return f()
                }
            }
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn recursive_field_type_on_struct() -> anyhow::Result<()> {
    let program = r#"
        struct A {
            a: Option<A>,
        }

        fn main(): i32 {
            let a = A { a: Option<A>::None }
            return 123
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn recursive_field_type_on_enum() -> anyhow::Result<()> {
    let program = r#"
        enum E {
            A,
            B,
            C(Option<E>),
        }

        fn main(): i32 {
            let e = E::C(Option<E>::None)
            return 123
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn not_wrapped_option() {
    let program = r#"
        struct Test {
            opt: Option<i32>,
        }

        fn f(): i32 {
            let test = Test {
                opt: 123,
            }
            return 123
        }"#;

    assert!(matches!(
        extract_semantic_error(exec(program).unwrap_err()),
        SemanticError::MismatchedTypes { .. }
    ));
}

#[test]
fn shadowing_in_match_arms() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let n = Option<i32>::Some(123)
            return match n {
                Option::Some(n) => n,
                Option::None => 1,
            }
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn bidirectional_type_inference() -> anyhow::Result<()> {
    let program = r#"
        fn f(n: u64): i32 {
            return n as i32
        }

        fn main(): i32 {
            let n = 123
            return f(n) // n is inferred as u64
        }
    "#;

    assert_eq!(exec(program)?, 123);

    Ok(())
}

#[test]
fn add_assign() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut n: i32 = 10
            n += 20
            return n
        }
    "#;

    assert_eq!(exec(program)?, 30);
    Ok(())
}

#[test]
fn sub_assign() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut n: i32 = 100
            n -= 45
            return n
        }
    "#;

    assert_eq!(exec(program)?, 55);
    Ok(())
}

#[test]
fn mul_assign() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut n: i32 = 29
            n *= 3
            return n
        }
    "#;

    assert_eq!(exec(program)?, 87);
    Ok(())
}

#[test]
fn div_assign() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut n: i32 = 116
            n /= 4
            return n
        }
    "#;

    assert_eq!(exec(program)?, 29);
    Ok(())
}

#[test]
fn rem_assign() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut n: i32 = 117
            n %= 4
            return n
        }
    "#;

    assert_eq!(exec(program)?, 1);
    Ok(())
}

#[test]
fn closure_without_params_captures_values() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let a = 48
            let b = 10

            let add = || a + b

            return add()
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_with_param_and_captured_copy() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let mut base = 50
            let add = |n| base + n
            base = 0

            return add(8)
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_with_multiple_params() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let add = |a, b| a + b
            return add(48, 10)
        }
    "#;
    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_with_captured_udt() -> anyhow::Result<()> {
    let program = r#"
        struct A {
            n: i32,
        }

        fn main(): i32 {
            let mut a = A { n: 48 }
            let f = |b| a.n + b
            a.n = 10
            return f(10)
        }
    "#;
    assert_eq!(exec(program)?, 20);
    Ok(())
}

#[test]
fn closure_with_captured_udt_method() -> anyhow::Result<()> {
    let program = r#"
        struct A {
            n: i32,
        }

        impl A {
            fn add(self, b: i32): i32 {
                return self.n + b
            }
        }

        fn main(): i32 {
            let a = A { n: 123 }
            let f = |b| a.add(b)
            return f(10)
        }
    "#;
    assert_eq!(exec(program)?, 133);
    Ok(())
}

#[test]
fn closure_with_block_body() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let a = 48
            let b = 10
            let add = || {
                a + b
            }
            return add()
        }
    "#;
    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_with_explicit_return_value() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let add = |a, b| {
                return a + b
            }
            return add(48, 10)
        }
    "#;
    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn unit_closure_with_empty_return() -> anyhow::Result<()> {
    let program = r#"
        fn call(f: fn()): i32 {
            f()
            return 58
        }

        fn main(): i32 {
            return call(|| {
                return
            })
        }
    "#;
    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_type_as_function_param() -> anyhow::Result<()> {
    let program = r#"
        fn apply(f: fn(i32) -> i32, x: i32): i32 {
            return f(x)
        }

        fn main(): i32 {
            return apply(|n| n + 10, 48)
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_type_as_variable() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            let f: fn(i32) -> i32 = |x| x + 1
            return f(57)
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_type_as_function_return_type() -> anyhow::Result<()> {
    let program = r#"
        fn f(): fn(i32) -> i32 {
            return |x| x + 1
        }

        fn main(): i32 {
            let g = f()
            return g(48)
        }
    "#;
    assert_eq!(exec(program)?, 49);
    Ok(())
}

#[test]
fn closure_type_with_generic_param() -> anyhow::Result<()> {
    let program = r#"
        fn apply<T>(f: fn(T) -> T, x: T): T {
            return f(x)
        }

        fn main(): i32 {
            return apply<i32>(|n| n + 10, 48)
        }
    "#;
    assert_eq!(exec(program)?, 58);
    Ok(())
}

#[test]
fn closure_immediate_call() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            return (|n| n + 1)(10)
        }
    "#;
    assert_eq!(exec(program)?, 11);
    Ok(())
}

#[test]
fn closure_immediate_call_with_multiple_params() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            return (|x, y| x + y)(15, 25)
        }
    "#;
    assert_eq!(exec(program)?, 40);
    Ok(())
}

#[test]
fn closure_immediate_call_nested() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            return (|x| (|y| x + y)(20))(30)
        }
    "#;
    assert_eq!(exec(program)?, 50);
    Ok(())
}

#[test]
fn closure_as_return_value_immediate_call() -> anyhow::Result<()> {
    let program = r#"
        fn f(): fn(i32) -> i32 {
            return |x| x + 1
        }

        fn main(): i32 {
            return f()(20)
        }
    "#;
    assert_eq!(exec(program)?, 21);
    Ok(())
}

#[test]
fn function_value_assigned_and_called() -> anyhow::Result<()> {
    let program = r#"
        fn add(x: i32, y: i32): i32 {
            return x + y
        }

        fn main(): i32 {
            let f = add
            return f(2, 3)
        }
    "#;
    assert_eq!(exec(program)?, 5);
    Ok(())
}

#[test]
fn function_value_passed_as_argument() -> anyhow::Result<()> {
    let program = r#"
        fn apply(f: fn(i32, i32) -> i32, x: i32, y: i32): i32 {
            return f(x, y)
        }

        fn mul(x: i32, y: i32): i32 {
            return x * y
        }

        fn main(): i32 {
            return apply(mul, 6, 7)
        }
    "#;
    assert_eq!(exec(program)?, 42);
    Ok(())
}

#[test]
fn function_and_closure_share_fn_type() -> anyhow::Result<()> {
    let program = r#"
        fn apply_twice(f: fn(i32) -> i32, x: i32): i32 {
            return f(f(x))
        }

        fn inc(n: i32): i32 {
            return n + 1
        }

        fn main(): i32 {
            let closure: fn(i32) -> i32 = |n| n + 2
            return apply_twice(inc, 10) + apply_twice(closure, 0)
        }
    "#;
    assert_eq!(exec(program)?, 16);
    Ok(())
}

#[test]
fn spawn_mutex_waitgroup() -> anyhow::Result<()> {
    let program = r#"
        import std.sync
        import std.collections

        use std.sync.Mutex
        use std.sync.WaitGroup
        use std.collections.Vector

        fn worker(id: i32, lock: mut Mutex, wg: mut WaitGroup, xs: mut Vector<i32>) {
            lock.lock()
            xs.push(id)
            lock.unlock()
            wg.done()
        }

        fn main(): i32 {
            let mut lock = Mutex::new()
            let mut wg = WaitGroup::new()
            let mut xs = Vector<i32>::new()

            let mut i = 0
            loop {
                if i >= 10 {
                    break
                }
                i += 1
                wg.add(1)
                spawn worker(i, lock, wg, xs)
            }

            wg.wait()
            return xs.len() as i32
        }
    "#;

    assert_eq!(exec(program)?, 10);
    Ok(())
}

#[test]
fn panic_basic() -> anyhow::Result<()> {
    let program = r#"
        fn main(): i32 {
            panic("Something went wrong")
            return 0
        }
    "#;

    let stderr = exec_expect_abort(program)?;
    assert!(stderr.contains("panic: Something went wrong"));
    assert!(stderr.contains("(at test:"));
    Ok(())
}

#[test]
fn panic_never_type_in_if() -> anyhow::Result<()> {
    // Test that panic returns Never type and works in if expressions
    let program = r#"
        fn main(): i32 {
            let x = if false {
                panic("unreachable")
            } else {
                42
            }
            return x
        }
    "#;

    assert_eq!(exec(program)?, 42);
    Ok(())
}

#[test]
fn panic_never_type_in_match() -> anyhow::Result<()> {
    // Test that panic returns Never type and works in match expressions
    let program = r#"
        enum Result {
            Ok(i32),
            Err,
        }

        fn main(): i32 {
            let r = Result::Ok(58)
            let x = match r {
                Result::Ok(v) => v,
                Result::Err => panic("error occurred"),
            }
            return x
        }
    "#;

    assert_eq!(exec(program)?, 58);
    Ok(())
}
