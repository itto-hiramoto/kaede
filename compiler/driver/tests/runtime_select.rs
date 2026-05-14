mod driver_test_support;

use driver_test_support::run_program as test;

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
