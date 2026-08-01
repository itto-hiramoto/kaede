---
title: Concurrency
description: Typed channels, task spawning, and synchronization patterns used in Kaede.
sidebar_position: 9
---

# Concurrency

Kaede includes built-in primitives for spawning tasks, passing values across channels, and waiting for concurrent work to finish.

## Spawning work

Use `spawn` to start work in another task:

```rust
fun producer(ch: Channel<i32>) {
    ch.send(42)
}

fun main() {
    ch := Channel<i32>::new()
    spawn producer(ch)
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

## Selecting across multiple channels

`select` blocks the current task until one of its channel operations can
proceed, then runs the matching arm. It follows Go's `select`:

```rust
select {
    case value = ch1.recv() => {
        // value: Option<T>; None means ch1 was closed.
    }
    case _ = ch3.recv() => {
        // received value is discarded
    }
    case ch2.send(x) => {
        // x was handed off on ch2
    }
    default => {
        // taken only if no other case is immediately ready
    }
}
```

Notes:

- Each `case` must be a `ch.recv()` or `ch.send(value)` method call —
  arbitrary expressions are rejected at parse time.
- The bound name on a `recv` arm has type `Option<T>`. A closed channel is
  always ready; once anything still buffered has been drained the arm fires
  immediately with `None`, mirroring `ch.recv()`'s contract.
- A `send` arm on a closed channel is also ready, and taking it panics, just
  as `ch.send(x)` does. `select` does not route around a closed channel — if
  that matters, check `is_closed()` first or use a separate cancellation
  channel.
- With `default`, `select` is non-blocking. Without it, the task blocks
  until at least one case can proceed.
- When multiple cases are simultaneously ready, one is chosen at random to
  avoid starvation (Go semantics).
- Channel expressions and `send` value expressions are evaluated in source
  order on entry, exactly once per `select` evaluation.

### Looping over a select

A `select` handles exactly one operation and then completes, so serving a
channel continuously means putting it in a loop:

```rust
loop {
    select {
        case value = results.recv() => {
            match value {
                Option::Some(v) => { total = total + v },
                Option::None => break,
            }
        }
        case _ = cancel.recv() => break
    }
}
```

Handling `None` is not optional here. A closed channel is always immediately
ready, so a loop that ignores closure will spin at full CPU forever once the
channel closes.

`break` inside an arm leaves the enclosing loop, as it does anywhere else.
Coming from Go this is worth noting: there, `break` leaves the `select` and a
labelled `break` is needed to exit the surrounding `for`.

## Synchronization helpers

Use `WaitGroup` when one task needs to wait for other spawned tasks to finish:

```rust
mut wg := WaitGroup::new()
wg.add(1)
spawn sender(ch, wg)
wg.wait()
```

Call `add()` before spawning work, then `wait()` in the coordinating task. Each worker should call `done()` when it completes.
