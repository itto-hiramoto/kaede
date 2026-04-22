---
title: Control flow
description: Branching and looping patterns currently used throughout Kaede examples and tests.
sidebar_position: 8
---

# Control flow

Kaede code in this repository uses familiar control-flow building blocks: `if`, `match`, `loop`, `while`, `break`, and `return`.

## if

```rust
if author.len() > 32 {
    res.status(Status::BadRequest).send("author too long")
    return
}
```

`if` also works as an expression:

```rust
author := if normalized_author.len() == 0 {
    String::from("anonymous")
} else {
    normalized_author.clone()
}
```

## match

Pattern matching is used heavily for enums and optional values.

```rust
value := match <-ch {
    Option::Some(v) => v,
    Option::None => return 1,
}
```

It is also used for nested dispatch:

```rust
match msg.kind {
    std.net.http.WebSocketMessageKind::Close => return
    _ => ws.send(msg)
}
```

## loop and break

Infinite loops are written with `loop`:

```rust
loop {
    match s.at(i) {
        Option::Some(ch) => {
            i += 1
        }
        Option::None => break,
    }
}
```

## while

`while` appears in existing examples when index-based iteration is clearer than iterator-style code.

```rust
mut i := 0
while i < pending.len() {
    comment := pending.at(i).unwrap()
    out.push_line(comment.json.as_str())
    i += 1
}
```

## return

Functions use explicit `return` in the current codebase:

```rust
fun foo() -> i32 {
    return 123
}
```

Functions returning `Result<T, E>` or `Option<T>` can use postfix `?` for early returns.
`Result::Err(e)?` becomes `return Result::Err(e)`, and `Option::None?` becomes `return Option::None`.

```rust
import std.result
use std.result.Result

fun read_value() -> Result<i32, str> {
    value := parse()?
    return Result::Ok(value)
}
```

```rust
import std.option
use std.option.Option

fun read_header() -> Option<str> {
    value := next_header()?
    return Option::Some(value)
}
```

Continue with [Concurrency](./concurrency.md) for channels and `spawn`.
