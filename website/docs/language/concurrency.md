---
title: Concurrency
description: Typed channels, task spawning, and synchronization patterns used in Kaede.
sidebar_position: 8
---

# Concurrency

Kaede includes built-in primitives for spawning tasks, passing values across channels, and waiting for concurrent work to finish.

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

Use `WaitGroup` when one task needs to wait for other spawned tasks to finish:

```rust
mut wg := WaitGroup::new()
wg.add(1)
spawn sender(ch, wg)
wg.wait()
```

Call `add()` before spawning work, then `wait()` in the coordinating task. Each worker should call `done()` when it completes.
