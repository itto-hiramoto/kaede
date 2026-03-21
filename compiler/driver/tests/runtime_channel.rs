use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::path::Path;
use std::process::Command;

fn compile(
    file_paths: &[&Path],
    root_dir: &Path,
    output_path: &Path,
) -> anyhow::Result<std::process::Output> {
    let mut args = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<String>>();

    args.push("-o".to_string());
    args.push(output_path.to_string_lossy().to_string());
    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    Ok(Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .output()?)
}

fn compile_project(
    file_paths: &[&Path],
    root_dir: &Path,
) -> anyhow::Result<assert_fs::NamedTempFile> {
    let exe = assert_fs::NamedTempFile::new("channel.out")?;
    let output = compile(file_paths, root_dir, exe.path())?;

    assert!(
        output.status.success(),
        "kaede compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    Ok(exe)
}

fn run_binary(expect: i32, exe_path: &Path) -> anyhow::Result<()> {
    Command::new(exe_path).assert().code(predicate::eq(expect));
    Ok(())
}

fn test(expect: i32, program: &str) -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(program)?;

    let exe = compile_project(&[main.path()], tempdir.path())?;
    run_binary(expect, exe.path())
}

#[test]
fn unbuffered_channel_hands_off_values_between_tasks() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fn producer(ch: Channel<i32>) {
    ch.send(42)
}

fn main(): i32 {
    let ch = Channel<i32>::new()
    spawn producer(ch)

    let value = match ch.recv() {
        Option::Some(v) => v,
        Option::None => return 1,
    }

    if value != 42 {
        return 2
    }

    return 0
}"#,
    )
}

#[test]
fn buffered_channel_applies_backpressure_until_receiver_runs() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.sync.WaitGroup
use std.option.Option

fn sender(ch: Channel<i32>, wg: mut WaitGroup) {
    ch.send(1)
    ch.send(2)
    wg.done()
}

fn main(): i32 {
    let ch = Channel<i32>::with_capacity(1)
    let mut wg = WaitGroup::new()
    wg.add(1)
    spawn sender(ch, wg)

    let first = match ch.recv() {
        Option::Some(v) => v,
        Option::None => return 1,
    }
    let second = match ch.recv() {
        Option::Some(v) => v,
        Option::None => return 2,
    }

    wg.wait()

    if first != 1 {
        return 3
    }
    if second != 2 {
        return 4
    }

    return 0
}"#,
    )
}

#[test]
fn close_wakes_blocked_receivers() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.sys
import std.option

use std.sync.Channel
use std.option.Option

fn closer(ch: Channel<i32>) {
    std.sys.sleep_ms(50)
    ch.close()
}

fn main(): i32 {
    let ch = Channel<i32>::new()
    spawn closer(ch)

    return match ch.recv() {
        Option::Some(_) => 1,
        Option::None => 0,
    }
}"#,
    )
}

#[test]
fn try_send_try_recv_and_close_follow_channel_contract() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fn main(): i32 {
    let ch = Channel<i32>::new()
    if ch.try_send(1) {
        return 1
    }
    if ch.try_recv().is_some() {
        return 2
    }

    let buffered = Channel<i32>::with_capacity(2)
    if !buffered.try_send(10) {
        return 3
    }
    if !buffered.try_send(20) {
        return 4
    }
    if buffered.try_send(30) {
        return 5
    }

    buffered.close()
    if !buffered.is_closed() {
        return 6
    }
    if buffered.try_send(40) {
        return 7
    }

    let first = match buffered.recv() {
        Option::Some(v) => v,
        Option::None => return 8,
    }
    let second = match buffered.try_recv() {
        Option::Some(v) => v,
        Option::None => return 9,
    }
    if first != 10 {
        return 10
    }
    if second != 20 {
        return 11
    }

    return match buffered.recv() {
        Option::Some(_) => 12,
        Option::None => 0,
    }
}"#,
    )
}
