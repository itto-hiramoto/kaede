---
title: Files
description: Durable file I/O patterns available through std.fs.
sidebar_position: 11
---

# Files

`std.fs` provides typed file APIs for ordinary file I/O, directory-relative
operations, and durable replacement writes.

## Opening Files

Use `OpenOptions` when you need explicit control over creation, truncation, and
append behavior.

```rust
import std.fs

use std.fs.OpenOptions

fun main() {
    let file = std.fs.open_file("db.log", OpenOptions {
        read: false,
        write: true,
        create: true,
        create_new: false,
        truncate: false,
        append: true,
        mode: 420,
    }).unwrap()

    file.write(b"entry\n").unwrap()
    file.sync_data().unwrap()
    file.close().unwrap()
}
```

Common constructors are available for the usual cases:

```rust
OpenOptions::read_only()
OpenOptions::write_create_truncate(420)
OpenOptions::append_create(420)
```

## Directory Operations

Use `Dir` for operations that should be scoped to a parent directory, including
renames and parent-directory syncs.

```rust
dir := std.fs.open_dir("data").unwrap()
tmp := dir.create_temp("main.tmp-*").unwrap()
tmp.write(bytes).unwrap()
tmp.sync_all().unwrap()
tmp.close().unwrap()
dir.rename(tmp.name(), "main").unwrap()
dir.sync().unwrap()
dir.close().unwrap()
```

## Atomic Replacement

For the common durable replace pattern, `write_atomic` writes a temporary file,
syncs it when requested, renames it into place, and optionally syncs the parent
directory.

```rust
std.fs.write_atomic("data/main", bytes, std.fs.AtomicWriteOptions {
    mode: 420,
    sync_file: true,
    sync_dir: true,
    replace: true,
}).unwrap()
```

`AtomicWriteOptions::durable_replace(mode)` is a shortcut for the fully durable
replace case.
