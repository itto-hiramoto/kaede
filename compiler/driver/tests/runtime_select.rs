mod driver_test_support;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

use driver_test_support::{compile_project, run_program as test};

#[test]
fn select_single_recv_arm_behaves_like_plain_recv() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun producer(ch: Channel<i32>) {
    ch.send(42)
}

fun main() -> i32 {
    let ch = Channel<i32>::new()
    spawn producer(ch)

    select {
        case value = ch.recv() => {
            match value {
                Option::Some(v) => {
                    if v != 42 { return 2 }
                    return 0
                },
                Option::None => return 1,
            }
        }
    }
    return 3
}"#,
    )
}

#[test]
fn select_fan_in_from_two_producers() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun producer1(ch: Channel<i32>) {
    ch.send(10)
}

fun producer2(ch: Channel<i32>) {
    ch.send(32)
}

fun main() -> i32 {
    let a = Channel<i32>::new()
    let b = Channel<i32>::new()
    spawn producer1(a)
    spawn producer2(b)

    let mut sum = 0
    let mut received = 0
    loop {
        if received == 2 { break }
        select {
            case va = a.recv() => {
                match va {
                    Option::Some(v) => { sum = sum + v },
                    Option::None => {},
                }
                received = received + 1
            },
            case vb = b.recv() => {
                match vb {
                    Option::Some(v) => { sum = sum + v },
                    Option::None => {},
                }
                received = received + 1
            },
        }
    }

    if sum == 42 { return 0 }
    return 4
}"#,
    )
}

#[test]
fn select_default_falls_through_when_no_case_ready() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun main() -> i32 {
    let ch = Channel<i32>::with_capacity(1)

    select {
        case _ = ch.recv() => return 1,
        default => return 0,
    }
    return 2
}"#,
    )
}

#[test]
fn select_observes_closed_channel_as_none() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun closer(ch: Channel<i32>) {
    ch.close()
}

fun main() -> i32 {
    let ch = Channel<i32>::new()
    spawn closer(ch)

    select {
        case value = ch.recv() => {
            match value {
                Option::None => return 0,
                Option::Some(_) => return 1,
            }
        }
    }
    return 2
}"#,
    )
}

#[test]
fn select_send_case_into_buffered_channel() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun main() -> i32 {
    let ch = Channel<i32>::with_capacity(1)

    select {
        case ch.send(11) => {},
    }

    match ch.recv() {
        Option::Some(v) => {
            if v == 11 { return 0 }
            return 2
        },
        Option::None => return 1,
    }
}"#,
    )
}

#[test]
fn select_send_case_handshakes_with_blocked_receiver() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun receiver(ch: Channel<i32>, out: Channel<i32>) {
    match ch.recv() {
        Option::Some(v) => out.send(v),
        Option::None => out.send(-1),
    }
}

fun main() -> i32 {
    let ch = Channel<i32>::new()
    let out = Channel<i32>::new()
    spawn receiver(ch, out)

    select {
        case ch.send(99) => {},
    }

    match out.recv() {
        Option::Some(v) => {
            if v == 99 { return 0 }
            return 2
        },
        Option::None => return 1,
    }
}"#,
    )
}

#[test]
fn select_send_case_panics_on_closed_channel() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import std.sync

use std.sync.Channel

fun main() -> i32 {
    let ch = Channel<i32>::new()
    ch.close()
    select {
        case ch.send(1) => { return 0 }
        default => { return 1 }
    }
    return 2
}"#,
    )?;

    let (exe, _) = compile_project(&[main.path()], tempdir.path())?;
    Command::new(exe.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("panic: send on closed channel"));

    Ok(())
}

#[test]
fn select_consumes_from_exactly_one_channel() -> anyhow::Result<()> {
    // Both channels hold a value, so either case may be picked. Whichever one
    // fires, the other channel must be left untouched — exactly one value is
    // still there afterwards.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun still_held(ch: Channel<i32>) -> i32 {
    match ch.try_recv() {
        Option::Some(_) => return 1,
        Option::None => return 0,
    }
}

fun main() -> i32 {
    let a = Channel<i32>::with_capacity(1)
    let b = Channel<i32>::with_capacity(1)
    a.send(1)
    b.send(2)

    select {
        case _ = a.recv() => {}
        case _ = b.recv() => {}
    }

    if still_held(a) + still_held(b) != 1 { return 1 }
    return 0
}"#,
    )
}

#[test]
fn select_picks_both_arms_over_many_rounds() -> anyhow::Result<()> {
    // Fairness: with both cases permanently ready, a fixed try order would
    // starve one arm. Over 100 rounds each arm should win at least once; the
    // odds of a correct shuffle failing this are 2^-99.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun main() -> i32 {
    let a = Channel<i32>::with_capacity(1)
    let b = Channel<i32>::with_capacity(1)

    let mut hit_a = 0
    let mut hit_b = 0
    let mut i = 0
    loop {
        if i >= 100 { break }
        a.send(1)
        b.send(2)
        select {
            case _ = a.recv() => { hit_a = hit_a + 1 },
            case _ = b.recv() => { hit_b = hit_b + 1 },
        }
        // Drain whichever one was not taken.
        a.try_recv()
        b.try_recv()
        i = i + 1
    }

    if hit_a == 0 { return 1 }
    if hit_b == 0 { return 2 }
    if hit_a + hit_b != 100 { return 3 }
    return 0
}"#,
    )
}

#[test]
fn select_loops_until_the_channel_closes() -> anyhow::Result<()> {
    // The pattern the docs teach: accumulate on Some, break on None. Also
    // covers `break` inside an arm leaving the enclosing loop.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun producer(ch: Channel<i32>) {
    ch.send(1)
    ch.send(2)
    ch.send(3)
    ch.close()
}

fun main() -> i32 {
    let ch = Channel<i32>::new()
    spawn producer(ch)

    let mut total = 0
    loop {
        select {
            case value = ch.recv() => {
                match value {
                    Option::Some(v) => { total = total + v },
                    Option::None => break,
                }
            }
        }
    }

    if total != 6 { return 1 }
    return 0
}"#,
    )
}

#[test]
fn select_in_a_generic_function_handles_two_instantiations() -> anyhow::Result<()> {
    // A recv arm resolves Option<T> at analysis time. Instantiating the same
    // generic at two element types, one scalar and one pointer-shaped, catches
    // an Option payload type that got stuck on whichever came first.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun first<T>(ch: Channel<T>) -> Option<T> {
    select {
        case v = ch.recv() => { return v }
    }
    return Option::None
}

fun feed_i32(ch: Channel<i32>) { ch.send(7) }
fun feed_str(ch: Channel<str>) { ch.send("hello") }

fun main() -> i32 {
    let a = Channel<i32>::new()
    spawn feed_i32(a)
    match first<i32>(a) {
        Option::Some(x) => { if x != 7 { return 1 } },
        Option::None => return 2,
    }

    let b = Channel<str>::new()
    spawn feed_str(b)
    match first<str>(b) {
        Option::Some(s) => { if s != "hello" { return 3 } },
        Option::None => return 4,
    }

    return 0
}"#,
    )
}

#[test]
fn select_over_more_cases_than_the_stack_buffer_holds() -> anyhow::Result<()> {
    // kaede_select keeps 32 cases on the stack and heap-allocates beyond that,
    // for both the shuffle order and the waiter array. Nothing is ready here,
    // so it has to reach Phase B and allocate the waiters too.
    test(
        32,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun waker(ch: Channel<i32>) {
    ch.send(1)
}

fun main() -> i32 {
    let c0 = Channel<i32>::new()
    let c1 = Channel<i32>::new()
    let c2 = Channel<i32>::new()
    let c3 = Channel<i32>::new()
    let c4 = Channel<i32>::new()
    let c5 = Channel<i32>::new()
    let c6 = Channel<i32>::new()
    let c7 = Channel<i32>::new()
    let c8 = Channel<i32>::new()
    let c9 = Channel<i32>::new()
    let c10 = Channel<i32>::new()
    let c11 = Channel<i32>::new()
    let c12 = Channel<i32>::new()
    let c13 = Channel<i32>::new()
    let c14 = Channel<i32>::new()
    let c15 = Channel<i32>::new()
    let c16 = Channel<i32>::new()
    let c17 = Channel<i32>::new()
    let c18 = Channel<i32>::new()
    let c19 = Channel<i32>::new()
    let c20 = Channel<i32>::new()
    let c21 = Channel<i32>::new()
    let c22 = Channel<i32>::new()
    let c23 = Channel<i32>::new()
    let c24 = Channel<i32>::new()
    let c25 = Channel<i32>::new()
    let c26 = Channel<i32>::new()
    let c27 = Channel<i32>::new()
    let c28 = Channel<i32>::new()
    let c29 = Channel<i32>::new()
    let c30 = Channel<i32>::new()
    let c31 = Channel<i32>::new()
    let c32 = Channel<i32>::new()

    spawn waker(c32)

    select {
            case _ = c0.recv() => { return 0 }
            case _ = c1.recv() => { return 1 }
            case _ = c2.recv() => { return 2 }
            case _ = c3.recv() => { return 3 }
            case _ = c4.recv() => { return 4 }
            case _ = c5.recv() => { return 5 }
            case _ = c6.recv() => { return 6 }
            case _ = c7.recv() => { return 7 }
            case _ = c8.recv() => { return 8 }
            case _ = c9.recv() => { return 9 }
            case _ = c10.recv() => { return 10 }
            case _ = c11.recv() => { return 11 }
            case _ = c12.recv() => { return 12 }
            case _ = c13.recv() => { return 13 }
            case _ = c14.recv() => { return 14 }
            case _ = c15.recv() => { return 15 }
            case _ = c16.recv() => { return 16 }
            case _ = c17.recv() => { return 17 }
            case _ = c18.recv() => { return 18 }
            case _ = c19.recv() => { return 19 }
            case _ = c20.recv() => { return 20 }
            case _ = c21.recv() => { return 21 }
            case _ = c22.recv() => { return 22 }
            case _ = c23.recv() => { return 23 }
            case _ = c24.recv() => { return 24 }
            case _ = c25.recv() => { return 25 }
            case _ = c26.recv() => { return 26 }
            case _ = c27.recv() => { return 27 }
            case _ = c28.recv() => { return 28 }
            case _ = c29.recv() => { return 29 }
            case _ = c30.recv() => { return 30 }
            case _ = c31.recv() => { return 31 }
            case _ = c32.recv() => { return 32 }
    }

    return 99
}"#,
    )
}

#[test]
fn select_mixes_send_and_recv_arms_at_runtime() -> anyhow::Result<()> {
    // Send and recv arms take structurally different codegen paths — the send
    // arm emits a status check and a panic block, the recv arm emits the
    // Option materialization. Interleaving them in one select is where block
    // ordering goes wrong.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun main() -> i32 {
    let src = Channel<i32>::with_capacity(1)
    let sink = Channel<i32>::with_capacity(1)

    // Only the send can proceed: src is empty, sink has room.
    select {
        case _ = src.recv() => { return 1 }
        case sink.send(9) => {}
    }

    match sink.try_recv() {
        Option::Some(v) => { if v != 9 { return 2 } },
        Option::None => return 3,
    }

    // Now only the recv can proceed: sink is full, src has a value.
    sink.send(0)
    src.send(5)
    select {
        case v = src.recv() => {
            match v {
                Option::Some(x) => { if x != 5 { return 4 } },
                Option::None => return 5,
            }
        }
        case sink.send(9) => { return 6 }
    }

    return 0
}"#,
    )
}

#[test]
fn concurrent_selects_split_values_without_losing_any() -> anyhow::Result<()> {
    // Two tasks parked in select on the same channel. Each value must wake
    // exactly one of them — this is what the `done` check in
    // pop_live_waiter_locked exists for. A lost value hangs the test; a
    // double delivery overshoots the total.
    test(
        0,
        r#"import std.sync
import std.option

use std.sync.Channel
use std.option.Option

fun consumer(work: Channel<i32>, results: Channel<i32>) {
    let mut seen = 0
    loop {
        select {
            case value = work.recv() => {
                match value {
                    Option::Some(_) => { seen = seen + 1 },
                    Option::None => break,
                }
            }
        }
    }
    results.send(seen)
}

fun main() -> i32 {
    let work = Channel<i32>::new()
    let results = Channel<i32>::new()

    spawn consumer(work, results)
    spawn consumer(work, results)

    let mut i = 0
    loop {
        if i >= 20 { break }
        work.send(i)
        i = i + 1
    }
    work.close()

    let mut total = 0
    let mut got = 0
    loop {
        if got >= 2 { break }
        match results.recv() {
            Option::Some(n) => { total = total + n },
            Option::None => return 1,
        }
        got = got + 1
    }

    if total != 20 { return 2 }
    return 0
}"#,
    )
}
