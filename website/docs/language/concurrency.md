---
title: Concurrency
description: Typed channels, task spawning, and synchronization patterns used in Kaede.
sidebar_position: 8
---

# Concurrency

Kaede includes built-in concurrency primitives that are already exercised in the test suite and the standard library.

## Spawning work

Use `spawn` to start work in another task:

```rust
fn producer(ch: Channel<i32>) {
    ch.send(42)
}

fn main(): i32 {
    ch := Channel<i32>::new()
    spawn producer(ch)
    return 0
}
```

## Typed channels

Channels are generic and can be created unbuffered or buffered:

```rust
ch := Channel<i32>::new()
buffered := Channel<i32>::with_capacity(2)
```

Sending and receiving can use method syntax:

```rust
ch.send(42)
value := match ch.recv() {
    Option::Some(v) => v,
    Option::None => return 1,
}
```

Kaede also supports channel operators:

```rust
ch <- 42

value := match <-ch {
    Option::Some(v) => v,
    Option::None => return 1,
}
```

## Synchronization helpers

The current tests also use a `WaitGroup`:

```rust
mut wg := WaitGroup::new()
wg.add(1)
spawn sender(ch, wg)
wg.wait()
```

This keeps concurrent examples straightforward when you need to wait for spawned work to finish.
