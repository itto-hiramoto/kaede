# comment_server

Minimal comment server example for Kaede's HTTP/WebSocket stdlib plus Rust interop.

The shape matches `kaede new --rust comment_server`:

- `src/main.kd` contains the Kaede server
- `rust/src/lib.rs` contains Rust helper functions

The example is intentionally small:

- viewers post comments over HTTP
- a frontend consumes comments over WebSocket
- comments are kept in memory only

## Endpoints

- `POST /comments?author=<name>` accepts the comment body as plain text
- `GET /comments?since=<id>` returns newline-delimited JSON history
- `GET /ws/comments?since=<id>` streams one JSON message per WebSocket text frame

## Run

```bash
cd example/comment_server
kaede build
./build/main
```

If you are running from this repo without an installed `kaede` binary:

```bash
cd example/comment_server
cargo run -p kaede_compiler_driver -- build
./build/main
```

## Try It

Post a comment:

```bash
curl -X POST 'http://127.0.0.1:8080/comments?author=alice' --data 'hello world'
```

Read history:

```bash
curl 'http://127.0.0.1:8080/comments?since=0'
```

Subscribe over WebSocket with `wscat`:

```bash
wscat -c 'ws://127.0.0.1:8080/ws/comments?since=0'
```

## Notes

- This example is meant for a single frontend connection.
- The server uses polling plus cooperative sleep for WebSocket fan-out.
- Rust interop here demonstrates `&str` parameters and `String` return values.
- The Rust helper uses `serde_json` to show that Kaede/Rust interop can use normal Rust ecosystem crates.
