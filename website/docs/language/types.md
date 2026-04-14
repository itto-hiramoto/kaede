---
title: Types
description: The kinds of values and type patterns that show up in current Kaede code.
sidebar_position: 5
---

# Types

Kaede already uses a mix of primitive values, generic containers, enums, and user-defined structs.

## Primitive and standard-library types

Examples in the repository use types such as:

- `i32`
- `i64`
- `u64`
- `bool`
- `str`
- `String`
- `Vector`

## Generic types

Generic types appear in both the standard library and user code.

```rust
ch := Channel<i32>::new()
mut comments := Vector<Comment>::new()
```

You will also see generic enums from the standard library:

```rust
match <-ch {
    Option::Some(v) => v,
    Option::None => return 1,
}

`Result<T, E>` is available from `std.result`:

```rust
import std.result
use std.result.Result

fun parse_id() -> Result<i32, str> {
    return Result::Ok(42)
}
```
```

For generic functions, generic structs, and generic `impl` blocks, see [Generics](./generics.md).

## Structs

Structs use named fields:

```rust
struct Comment {
    id: u64,
    json: String,
}
```

Construction uses field syntax:

```rust
Comment {
    id,
    json,
}
```

## Enums

Enums can carry values:

```rust
export enum Token {
    Num(i32),
    Add,
    Sub,
    Mul,
    Div,
}
```

Pattern matching is the main way to work with them:

```rust
match token {
    Token::Num(n) => n,
    _ => 0,
}
```

## Methods and impl

Methods live in `impl` blocks:

```rust
impl Counter {
    fun next(mut self) -> u64 {
        id := self.value
        self.value = id + 1
        return id
    }
}
```

See [Control flow](./control-flow.md) for the branching and looping constructs that show up inside those methods.
