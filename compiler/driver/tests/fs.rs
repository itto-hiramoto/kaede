mod driver_test_support;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::fs;
use std::process::Command;

use driver_test_support::compile_project;

fn compile_and_run_in_tempdir(program: &str) -> anyhow::Result<assert_fs::TempDir> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(program)?;

    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    Command::new(exe.path())
        .current_dir(tempdir.path())
        .assert()
        .success();

    Ok(tempdir)
}

#[test]
fn std_fs_open_options_cover_write_read_append_and_exclusive_create() -> anyhow::Result<()> {
    let tempdir = compile_and_run_in_tempdir(
        r#"import std.collections
import std.fs
import std.io
import std.result
import std.string

use std.fs.OpenOptions
use std.result.Result
use std.string.bytes_eq

fun main() -> i32 {
    let mode = 420

    let file = match std.fs.open_file("basic.txt", OpenOptions::write_create_truncate(mode)) {
        Result::Ok(value) => value,
        Result::Err(_) => return 1,
    }

    file.write(b"abc").unwrap()
    file.sync_all().unwrap()
    file.close().unwrap()

    let append = match std.fs.open_file("basic.txt", OpenOptions::append_create(mode)) {
        Result::Ok(value) => value,
        Result::Err(_) => return 2,
    }

    append.write(b"de").unwrap()
    append.sync_data().unwrap()
    append.close().unwrap()

    let read = match std.fs.open_file("basic.txt", OpenOptions::read_only()) {
        Result::Ok(value) => value,
        Result::Err(_) => return 3,
    }

    let bytes = match std.io.read_all(read, 100) {
        Result::Ok(value) => value,
        Result::Err(_) => return 4,
    }
    read.close().unwrap()

    if !bytes_eq(bytes.as_slice(), b"abcde") {
        return 5
    }

    let exclusive = std.fs.open_file("basic.txt", OpenOptions {
        read: false,
        write: true,
        create: false,
        create_new: true,
        truncate: false,
        append: false,
        mode,
    })
    if exclusive.is_ok() {
        return 6
    }

    return 0
}
"#,
    )?;

    tempdir
        .child("basic.txt")
        .assert(predicate::path::is_file());
    assert_eq!(fs::read(tempdir.path().join("basic.txt"))?, b"abcde");

    Ok(())
}

#[test]
fn std_fs_dir_operations_support_temp_rename_sync_and_remove() -> anyhow::Result<()> {
    let tempdir = compile_and_run_in_tempdir(
        r#"import std.collections
import std.fs
import std.io
import std.result
import std.string

use std.fs.OpenOptions
use std.result.Result
use std.string.bytes_eq

fun main() -> i32 {
    let dir = match std.fs.open_dir(".") {
        Result::Ok(value) => value,
        Result::Err(_) => return 1,
    }

    let tmp = match dir.create_temp("rename-*.tmp") {
        Result::Ok(value) => value,
        Result::Err(_) => return 2,
    }

    tmp.write(b"renamed").unwrap()
    tmp.sync_all().unwrap()
    tmp.close().unwrap()

    dir.rename(tmp.name(), "renamed.txt").unwrap()
    dir.sync().unwrap()

    let read = match dir.open_file("renamed.txt", OpenOptions::read_only()) {
        Result::Ok(value) => value,
        Result::Err(_) => return 3,
    }

    let bytes = match std.io.read_all(read, 100) {
        Result::Ok(value) => value,
        Result::Err(_) => return 4,
    }
    read.close().unwrap()

    if !bytes_eq(bytes.as_slice(), b"renamed") {
        return 5
    }

    dir.remove_file("renamed.txt").unwrap()
    dir.close().unwrap()

    if std.fs.open_file("renamed.txt", OpenOptions::read_only()).is_ok() {
        return 6
    }

    return 0
}
"#,
    )?;

    tempdir
        .child("renamed.txt")
        .assert(predicate::path::missing());
    let leftovers = fs::read_dir(tempdir.path())?
        .filter_map(Result::ok)
        .filter(|entry| entry.file_name().to_string_lossy().starts_with("rename-"))
        .count();
    assert_eq!(leftovers, 0);

    Ok(())
}

#[test]
fn std_fs_write_atomic_replaces_or_preserves_existing_file() -> anyhow::Result<()> {
    let tempdir = compile_and_run_in_tempdir(
        r#"import std.collections
import std.fs
import std.io
import std.result
import std.string

use std.fs.AtomicWriteOptions
use std.fs.OpenOptions
use std.result.Result
use std.string.bytes_eq

fun read_bytes(path: str) -> std.collections.Vector<u8> {
    let file = std.fs.open_file(path, OpenOptions::read_only()).unwrap()
    let bytes = std.io.read_all(file, 100).unwrap()
    file.close().unwrap()
    return bytes
}

fun main() -> i32 {
    let options = AtomicWriteOptions::durable_replace(420)

    std.fs.write_atomic("atomic.txt", b"first", options).unwrap()
    if !bytes_eq(read_bytes("atomic.txt").as_slice(), b"first") {
        return 1
    }

    std.fs.write_atomic("atomic.txt", b"second", options).unwrap()
    if !bytes_eq(read_bytes("atomic.txt").as_slice(), b"second") {
        return 2
    }

    let no_replace = std.fs.write_atomic("atomic.txt", b"third", AtomicWriteOptions {
        mode: 420,
        sync_file: true,
        sync_dir: true,
        replace: false,
    })
    if no_replace.is_ok() {
        return 3
    }

    if !bytes_eq(read_bytes("atomic.txt").as_slice(), b"second") {
        return 4
    }

    return 0
}
"#,
    )?;

    assert_eq!(fs::read(tempdir.path().join("atomic.txt"))?, b"second");
    let leftovers = fs::read_dir(tempdir.path())?
        .filter_map(Result::ok)
        .filter(|entry| entry.file_name().to_string_lossy().contains(".tmp."))
        .count();
    assert_eq!(leftovers, 0);

    Ok(())
}
