---
title: Generics
description: Define generic functions, structs, enums, and methods in Kaede.
sidebar_position: 6
---

# Generics

Kaede supports generic functions, structs, enums, and `impl` blocks.

Type parameters are written in angle brackets:

```rust
fn identity<T>(value: T): T {
    return value
}

struct Box<T> {
    value: T,
}
```

## Generic functions

Generic functions can declare one or more type parameters:

```rust
fn first<T, U>(a: T, b: U): T {
    return a
}
```

You can call them with explicit type arguments:

```rust
answer := first<i32, str>(58, "kaede")
```

Kaede can also infer type arguments from the call:

```rust
answer := first(58, "kaede")
```

## Generic structs

Generic structs work the same way:

```rust
struct Box<T> {
    value: T,
}

item := Box<i32> { value: 58 }
```

You will see this style throughout the standard library too:

```rust
mut comments := Vector<Comment>::new()
ch := Channel<i32>::new()
```

## Generic enums

Enums can also carry type parameters:

```rust
enum Maybe<T> {
    Some(T),
    None,
}
```

Pattern matching works as usual:

```rust
value := match Maybe<i32>::Some(58) {
    Maybe::Some(n) => n,
    Maybe::None => 0,
}
```

Standard library enums such as `Option<T>` follow the same pattern.

## Generic impl blocks

Methods on generic types go in `impl<T> ...` blocks:

```rust
struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    fn new(value: T): Box<T> {
        return Box<T> { value: value }
    }

    fn get(self): T {
        return self.value
    }
}
```

That lets you write:

```rust
item := Box<i32>::new(58)
value := item.get()
```

## Type argument inference

Kaede can infer generic type arguments in several common cases.

Function calls:

```rust
fn identity<T>(value: T): T {
    return value
}

result := identity(58)
```

Constructors and static methods:

```rust
mut values := Vector::new()
values.push(58)
```

Enum payloads:

```rust
opt := Option::Some(58)
```

## When explicit type arguments help

Sometimes you should write the type arguments or add an explicit type annotation to make the intended type clear.

For example, a bare `Option::None` usually needs context:

```rust
let missing: Option<i32> = Option::None
```

Without that context, Kaede may not be able to infer the type parameter.

## Practical rule

In everyday code:

- omit type arguments when the compiler can infer them cleanly
- write `Foo<T>` or `foo<T>(...)` when it improves clarity or inference needs help
- use `let name: Type = ...` when you want to pin the type explicitly
