---
title: Rust interop
description: Import Rust crates into Kaede and scaffold a mixed-language project.
sidebar_position: 10
---

# Rust interop

One of Kaede's defining features is direct Rust interop.

## Importing a Rust crate

Kaede imports Rust code with `import rust::<crate>`:

```rust
import rust::rust_interop

fun main() -> i32 {
    rust::rust_interop::greetings()
    return rust::rust_interop::add(10, 20)
}
```

## Create a project with Rust support

Use:

```bash
kaede new myproject --rust
```

That scaffold creates a layout like:

```text
myproject/
├── Kaede.toml
├── src/
│   └── main.kd
└── rust/
    ├── Cargo.toml
    └── src/
        └── lib.rs
```

The generated `Kaede.toml` contains a `[rust]` section, whose presence signals that the project uses Rust interop:

```toml
[rust]
path = "rust"
```

The generated Kaede entry file imports the Rust crate, and the generated Rust crate starts with a tiny function you can extend.

Generated `src/main.kd`:

```rust
import rust::myproject

fun main() -> i32 {
    return rust::myproject::add(10, 20)
}
```

Generated `rust/src/lib.rs`:

```rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

That is the basic shape of Rust interop in Kaede:

- Kaede imports the Rust crate with `import rust::...`
- Rust exposes plain Rust functions
- Kaede calls those functions through the imported module path

## When to use it

Rust interop is useful when:

- you already have logic in a Rust crate
- you want CPU-bound work to run in Rust and avoid GC overhead in hot paths
- you want to wrap platform-specific code in Rust
- you need a lower-level implementation behind a small Kaede-facing API

In practice, a good split is often:

- Kaede for request handling, orchestration, and higher-level application code
- Rust for tight compute-heavy loops or low-level primitives

See the repository example at [`example/rust_interop`](https://github.com/itto-hiramoto/kaede/tree/main/example/rust_interop).
