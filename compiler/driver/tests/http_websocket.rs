mod http_test_support;

use assert_fs::prelude::*;
use http_test_support::{
    assert_stream_closed, compile, connect_with_retry, free_port, make_binary_path,
    read_http_response, read_ws_frame, send_ws_masked_frame, send_ws_masked_frame_in_chunks,
    websocket_handshake, TestServer,
};
use std::fs;
use std::io::Write;
use std::net::TcpStream;
use std::time::Duration;

fn spawn_ws_server() -> anyhow::Result<(assert_fs::TempDir, TestServer, u16, std::path::PathBuf)> {
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    src_dir.create_dir_all()?;

    let port = free_port()?;
    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.net.http
import std.option

use std.option.Option

let mut app = std.net.http.App::new()

app.ws("/ws/echo", |req, ws| {{
    loop {{
        let msg = ws.receive()
        match msg {{
            Option::Some(msg) => {{
                match msg.kind {{
                    std.net.http.WebSocketMessageKind::Text => {{
                        if !ws.send(msg) {{
                            return
                        }}
                    }}
                    std.net.http.WebSocketMessageKind::Binary => {{
                        if !ws.send(msg) {{
                            return
                        }}
                    }}
                    std.net.http.WebSocketMessageKind::Close => {{
                        return
                    }}
                    std.net.http.WebSocketMessageKind::Ping => {{}}
                    std.net.http.WebSocketMessageKind::Pong => {{}}
                }}
            }}
            Option::None => {{
                return
            }}
        }}
    }}
}})

app.listen(ip="127.0.0.1", port={port})
"#
    ))?;

    let binary_path = make_binary_path("http-websocket-tests", "server", port)?;
    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let server = TestServer::spawn(&binary_path, tempdir.path(), port)?;
    Ok((tempdir, server, port, binary_path))
}

fn websocket_stream(port: u16) -> anyhow::Result<TcpStream> {
    let addr = format!("127.0.0.1:{port}");
    let stream = connect_with_retry(&addr, Duration::from_secs(10))?;
    stream.set_nodelay(true)?;
    stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    Ok(stream)
}

#[test]
fn std_http_websocket_upgrade_ping_pong_and_echo_work_with_partial_frames() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_ws_server()?;
    let mut stream = websocket_stream(port)?;

    let response = websocket_handshake(&mut stream, "/ws/echo")?;
    assert_eq!(response.status_code, 101);
    assert_eq!(
        response.headers.get("upgrade").map(String::as_str),
        Some("websocket")
    );
    assert_eq!(
        response.headers.get("connection").map(String::as_str),
        Some("Upgrade")
    );

    send_ws_masked_frame_in_chunks(&mut stream, 0x9, b"?", &[1, 2, 2])?;
    let pong = read_ws_frame(&mut stream)?;
    assert!(pong.fin);
    assert_eq!(pong.opcode, 0xA);
    assert_eq!(pong.payload, b"?");

    send_ws_masked_frame_in_chunks(&mut stream, 0x1, b"hello over ws", &[1, 3, 2, 4])?;
    let echo = read_ws_frame(&mut stream)?;
    assert!(echo.fin);
    assert_eq!(echo.opcode, 0x1);
    assert_eq!(echo.payload, b"hello over ws");

    send_ws_masked_frame(&mut stream, 0x8, b"")?;
    let close = read_ws_frame(&mut stream)?;
    assert_eq!(close.opcode, 0x8);
    assert_stream_closed(&mut stream)?;

    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn std_http_websocket_rejects_invalid_upgrade_requests() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_ws_server()?;
    let mut stream = websocket_stream(port)?;

    let request = concat!(
        "GET /ws/echo HTTP/1.1\r\n",
        "Host: 127.0.0.1\r\n",
        "Upgrade: websocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n",
        "\r\n"
    );
    stream.write_all(request.as_bytes())?;

    let response = read_http_response(&mut stream)?;
    assert_eq!(response.status_code, 400);
    assert!(response.body.is_empty());
    assert_stream_closed(&mut stream)?;

    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn std_http_websocket_protocol_errors_send_a_close_frame() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_ws_server()?;
    let mut stream = websocket_stream(port)?;

    let response = websocket_handshake(&mut stream, "/ws/echo")?;
    assert_eq!(response.status_code, 101);

    let payload = vec![b'x'; 126];
    send_ws_masked_frame(&mut stream, 0x9, &payload)?;

    let close = read_ws_frame(&mut stream)?;
    assert_eq!(close.opcode, 0x8);
    assert!(close.payload.is_empty());
    assert_stream_closed(&mut stream)?;

    let _ = fs::remove_file(binary_path);
    Ok(())
}
