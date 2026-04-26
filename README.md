<div align="center">
<h1>The Kaede Programming Language</h1>

[Official site](https://itto-hiramoto.github.io/kaede/)
| [Docs](https://itto-hiramoto.github.io/kaede/docs/intro)
| [Examples](https://itto-hiramoto.github.io/kaede/docs/examples)
| [VSCode extension](https://github.com/itto-hiramoto/kaede/tree/main/editors/vscode)
</div>

<p align="center">
  <a href="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-nix-linux.yml"><img alt="CI (Linux)" src="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-nix-linux.yml/badge.svg"></a>
  <a href="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-nix-macos.yml"><img alt="CI (macOS)" src="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-nix-macos.yml/badge.svg"></a>
</p>

> [!WARNING]
> Kaede is still in pre-release. The language specification and standard-library
> APIs may change significantly before 1.0.

**Kaede** is a statically-typed, compiled language with a rich feature set for
writing servers concisely without giving up performance.

## Why Kaede?

- **Write concisely** - Garbage collection manages memory for you so you can focus on your program's logic
- **Concurrency made easy** - Lightweight threads, typed channels (`<-`), and non-blocking I/O
- **Rust when you need it** - `import rust::<crate>` to use Rust crates directly
- **Rich syntax and types** - Mostly expression-oriented code, with sum types, pattern matching (`match`), generics, structs and methods, closures, and modules

## Quick Start

Read [Installation](https://itto-hiramoto.github.io/kaede/docs/getting-started/installation) on the official site.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](./LICENSE-APACHE))
- MIT license ([LICENSE-MIT](./LICENSE-MIT))

at your option.
