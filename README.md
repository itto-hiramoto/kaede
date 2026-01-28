[![test](https://github.com/itto-hiramoto/kaede/actions/workflows/test.yml/badge.svg)](https://github.com/itto-hiramoto/kaede/actions/workflows/test.yml)

> [!WARNING]
> As this project is still in the pre-release phase, there is still a possibility that the language specifications could change significantly!

**Kaede** is an experimental systems programming language that combines:

- **Go-style green threads** — Lightweight concurrent tasks with multi-core scheduling
- **Rust-like syntax** — Modern, expressive syntax with static typing
- **Bidirectional type inference** — Minimal type annotations with full type safety
- **Seamless Rust interop** — Auto-generated bindings to call Rust functions directly
- **Garbage collection** — Automatic memory management without ownership complexity

## Installation

### Prerequisites

- LLVM 17
- Python 3
- CMake
- C compiler (gcc or clang)
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

# Build the project
$ kaede build
```

## Editor Support

- **VSCode**: Extension available in `editors/vscode/`

## License

[TBD]
