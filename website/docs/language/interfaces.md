---
title: Interfaces
description: Define behavior contracts and use them as generic bounds in Kaede.
sidebar_position: 7
---

# Interfaces

An `interface` declares a set of method signatures. Any type whose method set covers those signatures **conforms to the interface implicitly** — there is no `impl X for Y` step. This is the same model Go uses.

```rust
interface Stringer {
    fun to_string(self) -> String
}
```

A struct conforms simply by defining matching methods:

```rust
struct Counter {
    value: u64,
}

impl Counter {
    fun to_string(self) -> String {
        return String::from("Counter")
    }
}
```

`Counter` now satisfies `Stringer`. Nothing else is required.

## Generic bounds

The most common use is as a bound on a generic type parameter. Inside the function body, only the methods declared on the interface are callable on the bound parameter.

```rust
interface Stringer {
    fun to_string(self) -> String
}

fun describe<S: Stringer>(value: S) -> String {
    return value.to_string()
}
```

Any type with a matching `to_string` method can be passed in:

```rust
let counter = Counter { value: 0 }
let label = describe(counter)
```

A generic parameter without a bound (`fun identity<T>(value: T) -> T`) accepts any type but cannot call any methods on it.

## Standard-library interfaces

`std.io` declares two interfaces used throughout the stdlib I/O surface:

```rust
interface Reader {
    fun read(self, buf: mut [u8]) -> Option<u64>
}

interface Writer {
    fun write(self, buf: [u8]) -> Option<u64>
}
```

`std.sys.Fd` (the file-descriptor wrapper used by `std.http`) satisfies both, so any helper that takes `<R: Reader>` or `<W: Writer>` works directly with a TCP connection or open file:

```rust
import std.io
import std.sys

use std.io.Writer
use std.sys.Fd

fun greet<W: Writer>(out: W) -> bool {
    return out.write(b"hello\n").is_some()
}

fun main() -> i32 {
    let stdout = Fd::new(1)
    if greet(stdout) {
        return 0
    }
    return 1
}
```

Other stdlib interfaces follow the same pattern:

- `std.io.Stringer` — anything with `fun to_string(self) -> String`
- `std.http.Payload` — bodies accepted by `Response::send` (impls for `str` and `[u8]`)
- `std.http.WebSocketPayload` — payloads accepted by `WebSocket::send`

To plug in your own type, just give it the right method. No registration is needed.

## When to reach for an interface

- The same operation is meaningful on several unrelated types (printing, byte I/O, message payloads).
- You want to write one generic helper that works for every conforming type without enumerating them in an `enum`.
- You want callers to be able to plug in their own types later without modifying the helper.

For one-off polymorphism, a plain enum is usually simpler.

## See also

- [Generics](./generics.md) — declaring `<T>` parameters and inferring type arguments.
- [Types](./types.md) — structs, enums, and how methods are declared.
