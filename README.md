<div align="center">
<h1>The Kaede Programming Language</h1>

[Official site](https://itto-hiramoto.github.io/kaede/)
| [Docs](https://itto-hiramoto.github.io/kaede/docs/intro)
| [Examples](https://itto-hiramoto.github.io/kaede/docs/examples)
| [VSCode extension](https://github.com/itto-hiramoto/kaede/tree/main/editors/vscode)
</div>

<p align="center">
  <a href="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-linux.yml"><img alt="CI (Linux)" src="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-linux.yml/badge.svg"></a>
  <a href="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-macos.yml"><img alt="CI (macOS)" src="https://github.com/itto-hiramoto/kaede/actions/workflows/ci-macos.yml/badge.svg"></a>
</p>

> [!WARNING]
> As this project is still in the pre-release phase, there is still a possibility that the language specifications could change significantly!

**Kaede** is a statically-typed, compiled language with a rich feature set for writing servers concisely without giving up performance.

- **Write concisely** — Garbage collection manages memory for you so you can focus on your program's logic
- **Concurrency made easy** — Lightweight threads, typed channels (`<-`), and non-blocking I/O
- **Rust when you need it** — `import rust::<crate>` to call Rust functions directly
- **Rich syntax and types** — Mostly expression-oriented code, with sum types, pattern matching (`match`), generics, structs and methods, closures, and modules

```rust
import std.http

mut app := std.http.App::new()

app.get("/", |req, res| {
    res.send_text("hello, world!")
})

app.ws("/ws/echo", |req, ws| {
    loop {
        let msg = ws.receive().unwrap()
        match msg.kind {
            std.http.WebSocketMessageKind::Close => return
            _ => ws.send(msg)
        }
    }
})

app.listen(port=8080)
```

## Installation

### Prerequisites

- LLVM 17
- Rust toolchain (`rustc` and `cargo`)
- (Optional, for Rust interop) Cargo nightly (`cargo +nightly`)
- Python 3
- CMake
- C compiler (gcc or clang)
- OpenSSL development files and `pkg-config`
- **Supported architectures**: x86-64, AArch64

### macOS / Linux (Homebrew)

```bash
$ brew install llvm@17 cmake python
$ export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
```

To make it permanent, add to your shell profile (`~/.profile`, `~/.zshrc`, etc.):

```bash
$ echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> ~/.profile
```

### Build & Install

```bash
$ ./install.py
$ kaede -h
```

## Usage

```bash
# Create a new project
$ kaede new myproject
$ cd myproject

# Run the project
$ kaede run
```

## Documentation

The official site is built with Docusaurus and lives in `website/`.

- Site: <https://itto-hiramoto.github.io/kaede/>
- Docs: <https://itto-hiramoto.github.io/kaede/docs/intro>

### Website Development

Use Node 20-24 when working on the site.

```bash
$ cd website
$ npm install
$ npm start
```

To produce a static build:

```bash
$ cd website
$ npm run build
```

## Editor Support

- **VSCode**: Extension available in `editors/vscode/`

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](./LICENSE-APACHE))
- MIT license ([LICENSE-MIT](./LICENSE-MIT))

at your option.
