//! Command line argument tests based on test.kd
//! Testing the strcmp functionality and command line argument handling
//!
//! This test suite provides comprehensive coverage for command line argument processing:
//!
//! ## Test Coverage:
//! - **Exact match**: Tests when first argument exactly matches "kaede" (returns 123)
//! - **Different arguments**: Tests various non-matching arguments (returns 124)
//! - **Edge cases**: Empty strings, case sensitivity, partial matches, longer strings
//! - **Multiple arguments**: Tests behavior with multiple command line arguments
//! - **No arguments**: Tests behavior when no arguments are provided (returns 125)
//!
//! ## Exit Codes:
//! - **123**: First argument matches "kaede" exactly
//! - **124**: First argument doesn't match "kaede" (or any other mismatch)
//! - **125**: Insufficient arguments (less than 2 total arguments)
//!
//! ## Technical Details:
//! - Uses extern C strcmp function for string comparison
//! - Demonstrates Kaede's *i8 pointer type for C string interop
//! - Shows Vector<str> usage for command line arguments
//! - Includes safe argument count validation before accessing arguments
//! - Tests proper type casting (u32 for Vector::len(), *i8 for str::as_ptr())
//!
//! Based on the original test.kd, enhanced with proper argument validation to prevent
//! runtime panics when insufficient arguments are provided.

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

/// Test program that checks command line arguments with safe validation
const TEST_PROGRAM: &str = r#"extern "C" fn strcmp(s1: *i8, s2: *i8): i32

fn main(args: Vector<str>): i32 {
    let mut i = 0

    // Check if we have enough arguments (program name + at least 1 argument)
    if args.len() < 2 {
        return 125  // Special exit code for insufficient arguments
    }

    let s = args.at(1).unwrap()

    if strcmp(s.as_ptr(), "kaede".as_ptr()) == 0 {
        return 123
    }

    return 124
}"#;

/// Helper function to compile and run the test program with given arguments
fn compile_and_run_with_args(args: &[&str], expected_exit_code: i32) -> anyhow::Result<()> {
    let temp_dir = assert_fs::TempDir::new()?;
    let program_file = temp_dir.child("test_cli.kd");
    let executable = temp_dir.child("test_cli");

    program_file.write_str(TEST_PROGRAM)?;

    // Compile the program
    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            "-O0",
            "-o",
            executable.path().to_str().unwrap(),
            "--root-dir",
            temp_dir.path().to_str().unwrap(),
            program_file.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    // Run the program with provided arguments
    Command::new(executable.path())
        .args(args)
        .assert()
        .code(predicate::eq(expected_exit_code));

    Ok(())
}

#[test]
fn test_exact_match_kaede() -> anyhow::Result<()> {
    // Test when the first argument exactly matches "kaede"
    compile_and_run_with_args(&["kaede"], 123)
}

#[test]
fn test_different_argument() -> anyhow::Result<()> {
    // Test when the first argument doesn't match "kaede"
    compile_and_run_with_args(&["hello"], 124)
}

#[test]
fn test_empty_string_argument() -> anyhow::Result<()> {
    // Test with empty string argument
    compile_and_run_with_args(&[""], 124)
}

#[test]
fn test_case_sensitivity() -> anyhow::Result<()> {
    // Test that the comparison is case-sensitive
    compile_and_run_with_args(&["KAEDE"], 124)
}

#[test]
fn test_partial_match() -> anyhow::Result<()> {
    // Test partial matches don't work
    compile_and_run_with_args(&["kae"], 124)
}

#[test]
fn test_longer_string() -> anyhow::Result<()> {
    // Test longer strings don't match
    compile_and_run_with_args(&["kaede123"], 124)
}

#[test]
fn test_multiple_arguments() -> anyhow::Result<()> {
    // Test with multiple arguments - only first one should be checked
    compile_and_run_with_args(&["kaede", "extra", "args"], 123)
}

#[test]
fn test_multiple_arguments_wrong_first() -> anyhow::Result<()> {
    // Test with multiple arguments where first doesn't match
    compile_and_run_with_args(&["wrong", "kaede"], 124)
}

#[test]
fn test_special_characters() -> anyhow::Result<()> {
    // Test with special characters
    compile_and_run_with_args(&["ka@ede"], 124)
}

#[test]
fn test_numeric_argument() -> anyhow::Result<()> {
    // Test with numeric argument
    compile_and_run_with_args(&["123"], 124)
}

#[test]
fn test_no_arguments_provided() -> anyhow::Result<()> {
    // Test with no arguments - should return 125 (insufficient arguments)
    compile_and_run_with_args(&[], 125)
}
