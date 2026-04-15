---
title: Language overview
description: A compact tour of Kaede syntax using patterns that already exist in the repository.
sidebar_position: 4
---

# Language overview

This page stays intentionally high level. It focuses on syntax and patterns that are already used in the Kaede repository and examples.

## Imports and names

Kaede uses `import` for modules and `use` to bring names into shorter scope.

```rust
import std.http
import std.sync

use std.http.Status
use std.sync.Mutex
```

Qualified names use `::`:

```rust
mut app := std.http.App::new()
```

## Bindings

Kaede supports both the short declaration form and `let` bindings:

```rust
vec := Vector<i32>::new()
mut app := std.http.App::new()
let count: i32 = 3
```

In practice, current Kaede code tends to use:

- `:=` for local bindings when the type is obvious from the right-hand side
- `mut ... :=` for mutable locals with inferred types
- `let` when you want the explicit `let` form, especially with a type annotation such as `let count: i32 = 3`

`let x = 1` and `let mut x = 1` are also valid.

## Functions and return types

Functions declare return types with `-> Type`.

```rust
fun greet() {
    println("hello, world!")
}

fun add(a: i32, b: i32) -> i32 {
    return a + b
}
```

## Closures

Closures are commonly used in HTTP handlers and collection helpers.

```rust
app.get("/", |req, res| {
    res.send("Kaede!")
})

subscribers.retain(|subscriber| {
    if subscriber.events.try_send(json) {
        return true
    }

    subscriber.events.close()
    return false
})
```

## Data types and methods

Structs, enums, and `impl` blocks are part of everyday Kaede code.

```rust
struct Counter {
    value: u64,
}

impl Counter {
    fun new(start: u64) -> mut Counter {
        return Counter { value: start }
    }

    fun next(mut self) -> u64 {
        id := self.value
        self.value = id + 1
        return id
    }
}
```

Continue with:

- [Types](./types.md)
- [Generics](./generics.md)
- [Control flow](./control-flow.md)
- [Concurrency](./concurrency.md)
- [Rust interop](./rust-interop.md)
