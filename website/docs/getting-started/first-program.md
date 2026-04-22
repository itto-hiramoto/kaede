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

Kaede creates a project with a `Kaede.toml` manifest, a `src/` directory, and a starter `main.kd` file:

```rust
fun main() {
    println("hello, world!")
}
```

The generated `Kaede.toml`:

```toml
[package]
name = "hello_kaede"
version = "0.1.0"

[build]
src = "src"
out = "build/main"
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
├── Kaede.toml
└── src/
    └── main.kd
```

`Kaede.toml` marks the project root. `kaede build` and `kaede run` require it in the working directory; the `[build]` section controls the source directory and the output binary path.

## Rust interop scaffold

If you know you want Rust interop from the start, create the project with:

```bash
kaede new hello_kaede --rust
```

That gives you both Kaede sources and a `rust/` crate that Kaede can import. The generated `Kaede.toml` additionally contains a `[rust]` section that records the crate path.

Next: read the [language overview](../language/overview.md).
