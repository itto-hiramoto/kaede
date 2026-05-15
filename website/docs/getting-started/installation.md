---
title: Installation
description: Install Kaede and verify the compiler is available on your machine.
sidebar_position: 2
---

# Installation

## macOS / Linux (recommended)

Install via the [`itto-hiramoto/kaede`](https://github.com/itto-hiramoto/homebrew-kaede) Homebrew tap:

```bash
brew tap itto-hiramoto/kaede
brew install kaede
kaede -h
```

Homebrew resolves LLVM 17, OpenSSL, CMake, and the Rust toolchain on its own. The first install compiles Kaede from source, which takes a few minutes.

Supported architectures: x86-64, AArch64.

Once `kaede -h` runs, continue to [First program](./first-program.md).

## Build from source

Use this path if you want to hack on the compiler, need a customized build, or your platform isn't covered by the Homebrew tap.

### Prerequisites

- LLVM 17
- Rust toolchain (`rustc` and `cargo`)
- Python 3
- CMake
- C compiler such as `gcc` or `clang`
- `pkg-config`
- OpenSSL dev package

Optional:

- Cargo nightly, when you want Rust interop support that depends on nightly tooling

### Ubuntu / Debian

```bash
sudo apt update
sudo apt install -y build-essential cmake python3 pkg-config libssl-dev curl wget git
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 17
curl https://sh.rustup.rs -sSf | sh -s -- -y
. "$HOME/.cargo/env"

git clone --recursive https://github.com/itto-hiramoto/kaede.git
cd kaede

export LLVM_SYS_170_PREFIX=/usr/lib/llvm-17
./install.py
. "$HOME/.kaede/env"
kaede -h
```

## Rust interop

If you need nightly-only Rust interop support:

```bash
rustup toolchain install nightly --profile minimal
```

Once `kaede -h` works, continue to [First program](./first-program.md).
