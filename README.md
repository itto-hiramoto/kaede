[![CI (Linux)](https://github.com/itto-hiramoto/kaede/actions/workflows/ci-linux.yml/badge.svg)](https://github.com/itto-hiramoto/kaede/actions/workflows/ci-linux.yml)
[![CI (macOS)](https://github.com/itto-hiramoto/kaede/actions/workflows/ci-macos.yml/badge.svg)](https://github.com/itto-hiramoto/kaede/actions/workflows/ci-macos.yml)

> [!WARNING]
> As this project is still in the pre-release phase, there is still a possibility that the language specifications could change significantly!

**Kaede** is a statically-typed, compiled language with a rich feature set for writing servers concisely without giving up performance.

- **Write concisely** — Garbage collection manages memory for you so you can focus on your program's logic
- **Rich types and syntax** — Mostly expression-oriented code, with sum types, pattern matching (`match`), generics, structs and methods, closures, and modules
- **Concurrency made easy** — Lightweight threads, typed channels (`<-`), and non-blocking I/O
- **Rust when you need it** — `import rust::<crate>` to call Rust functions directly

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

## Editor Support

- **VSCode**: Extension available in `editors/vscode/`

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](./LICENSE-APACHE))
- MIT license ([LICENSE-MIT](./LICENSE-MIT))

at your option.
