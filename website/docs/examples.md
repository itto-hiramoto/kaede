---
title: Examples
description: Repository examples that show Kaede in realistic shapes.
sidebar_position: 4
---

# Examples

The `example/` directory is the source of truth for runnable Kaede samples.

## Included samples

- [`calculator`](https://github.com/itto-hiramoto/kaede/tree/main/example/calculator)
  A small interpreter-style program that exercises enums, parsing, and evaluation.
- [`http_server`](https://github.com/itto-hiramoto/kaede/tree/main/example/http_server)
  A compact HTTP and WebSocket server.
- [`http_kv_store`](https://github.com/itto-hiramoto/kaede/tree/main/example/http_kv_store)
  A larger service that uses structs, methods, maps, routing, and request parsing.
- [`comment_app`](https://github.com/itto-hiramoto/kaede/tree/main/example/comment_app)
  A fuller demo with frontend assets, a live comment feed, and Rust helper code.
- [`rust_interop`](https://github.com/itto-hiramoto/kaede/tree/main/example/rust_interop)
  The smallest end-to-end example of importing a Rust crate from Kaede.

## Recommended order

If you are new to Kaede:

1. Start with `http_server`
2. Read `calculator` for enums and parser-style code
3. Move to `comment_app` or `http_kv_store`
4. Inspect `rust_interop` when you need native integration

## Running an example

Most examples follow the same shape:

```bash
cd example/http_server
kaede run
```

If you do not have an installed `kaede` binary yet, build from the repository root first with `./install.py`.
