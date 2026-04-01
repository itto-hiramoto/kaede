---
title: First program
description: Create a Kaede project, run it locally, and inspect the generated layout.
sidebar_position: 3
---

# First program

## Create a new project

```bash
kaede new hello_kaede
cd hello_kaede
```

Kaede creates a project with a `src/` directory and a starter `main.kd` file:

```rust
fn main() {
    println("hello, world!")
}
```

## Run it

```bash
kaede run
```

The `run` command builds the project and then runs `build/main`. With the default scaffold, it prints:

```text
hello, world!
```

If you want the steps separately:

```bash
kaede build
./build/main
```

## Project layout

The minimum project shape is:

```text
hello_kaede/
└── src/
    └── main.kd
```

Kaede treats `src/` as the project root when you use the project commands.

## Rust interop scaffold

If you know you want Rust interop from the start, create the project with:

```bash
kaede new hello_kaede --rust
```

That gives you both Kaede sources and a `rust/` crate that Kaede can import.

Next: read the [language overview](../language/overview.md).
