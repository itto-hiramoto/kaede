---
title: Interfaces
description: Declare method contracts and use them as interface values or generic bounds in Kaede.
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

### Stringer and `$"..."` interpolation

Any type that satisfies `Stringer` can be dropped straight into a string-interpolation placeholder. Each `{}` calls `to_string` on its argument:

```rust
let c = Counter { value: 7 }
println($"counter = {c}")
```

Giving a type `fun to_string(self) -> String` is all you need to make it printable this way — no registration, no trait derivation.

## Interface values

An interface name can be used directly as a type — as a parameter, return type, local, or element type inside a container like `Vector<I>`. A value held through an interface type (an **interface value**) carries its concrete type along with it, and each method call is resolved at runtime against that concrete type.

```rust
interface Scorer {
    fun score(self) -> i32
}

struct Alpha {
    n: i32,
}

struct Beta {
    n: i32,
}

impl Alpha {
    fun score(self) -> i32 {
        return self.n + 10
    }
}

impl Beta {
    fun score(self) -> i32 {
        return self.n * 2
    }
}

fun run(s: Scorer) -> i32 {
    return s.score()
}

fun main() -> i32 {
    let a = Alpha { n: 40 }
    let b = Beta { n: 4 }
    return run(a) + run(b)
}
```

Both `Alpha` and `Beta` satisfy `Scorer`, and `run` dispatches to the right `score` for each concrete type.

The same works for heterogeneous collections:

```rust
let mut items = Vector<Scorer>::new()
items.push(Alpha { n: 40 })
items.push(Beta { n: 4 })
```

Each element remembers the concrete type it was built from, and calling `.score()` on an element picks the matching implementation.

## Generic bounds

An interface can also be used as a bound on a generic type parameter. Inside the function body, only the methods declared on the interface are callable on the bound parameter.

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

A generic parameter without a bound (`fun identity<T>(value: T) -> T`) accepts any type; the bound is a constraint on what callers may pass.

### Dynamic vs. static dispatch

Both forms share the same conformance rule — the method set must match — but they differ in how method calls are resolved:

- **Interface values** (`fun f(s: Scorer)`) use **dynamic dispatch**. One function body, and the method is looked up at runtime against the value's concrete type. Different concrete types can flow through the same variable.
- **Generic bounds** (`fun f<S: Scorer>(s: S)`) use **static dispatch**. One specialization per concrete `S`, resolved at compile time. No runtime lookup, but each call site fixes a single concrete type.

Reach for an interface value when the concrete type can vary at runtime (heterogeneous containers, plug-in style APIs). Reach for a generic bound when each caller has a single concrete type and you want the compiler to monomorphize.

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
