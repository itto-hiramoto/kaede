---
title: Installation
description: Install Kaede and verify the compiler is available on your machine.
sidebar_position: 2
---

# Installation

## Prerequisites

- LLVM 17
- Rust toolchain (`rustc` and `cargo`)
- Python 3
- CMake
- C compiler such as `gcc` or `clang`
- `pkg-config`
- OpenSSL dev package

Optional:

- Cargo nightly, when you want Rust interop support that depends on nightly tooling

Run the block for your platform.

Supported architectures:

- x86-64
- AArch64

## macOS / Linux (Homebrew)

```bash
brew install git llvm@17 cmake python pkg-config openssl@3
curl https://sh.rustup.rs -sSf | sh -s -- -y
. "$HOME/.cargo/env"

case "$(basename "$SHELL")" in
  zsh)  PROFILE=~/.zprofile ;;
  *)    PROFILE=~/.profile ;;
esac
echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> "$PROFILE"
. "$PROFILE"

git clone --recursive https://github.com/itto-hiramoto/kaede.git
cd kaede

./install.py
. "$HOME/.kaede/env"
kaede -h
```

## Ubuntu / Debian

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
