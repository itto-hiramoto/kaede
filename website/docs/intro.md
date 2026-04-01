---
title: Introduction
description: Start here to understand what Kaede is for and how the documentation is organized.
sidebar_position: 1
---

# Introduction

Kaede is a statically typed, compiled language designed to make server-side programs compact without giving up performance.

It currently emphasizes:

- garbage-collected memory management
- typed concurrency primitives
- Rust interop through `import rust::<crate>`
- expression-oriented code with pattern matching, enums, structs, methods, and generics

:::warning Pre-release language
Kaede is still in pre-release. Language details and standard-library APIs may change before 1.0.
:::

## Start with these pages

- [Installation](./getting-started/installation.md) for prerequisites and setup
- [First program](./getting-started/first-program.md) for a quick local project
- [Language overview](./language/overview.md) for the syntax and shape of Kaede code
- [Examples](./examples.md) for real sample projects in the repository

## What Kaede looks like

```rust
import std.http

mut app := std.http.App::new()

app.get("/", |req, res| {
    res.send_text("hello, world!")
})

app.listen(port=8080)
```

## Editor support

A VSCode extension is available in [`editors/vscode`](https://github.com/itto-hiramoto/kaede/tree/main/editors/vscode).
