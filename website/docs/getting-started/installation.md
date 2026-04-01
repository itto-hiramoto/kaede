---
title: Installation
description: Install Kaede and verify the compiler is available on your machine.
sidebar_position: 2
---

# Installation

## Prerequisites

Kaede currently expects:

- LLVM 17
- Rust toolchain (`rustc` and `cargo`)
- Python 3
- CMake
- C compiler such as `gcc` or `clang`
- `pkg-config`
- OpenSSL development files

Optional:

- Cargo nightly, when you want Rust interop support that depends on nightly tooling

Supported architectures:

- x86-64
- AArch64

## macOS / Linux (Homebrew)

```bash
brew install llvm@17 cmake python
export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
```

If you want that `LIBRARY_PATH` change to persist, add it to your shell profile:

```bash
echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> ~/.profile
```

## Build and install Kaede

From the repository root:

```bash
./install.py
kaede -h
```

If OpenSSL support is missing on your machine, confirm this command succeeds:

```bash
pkg-config --cflags --libs openssl
```

## Verify the install

Once `kaede -h` works, continue to [First program](./first-program.md).
