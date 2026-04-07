mod http_test_support;

use assert_fs::prelude::*;
use http_test_support::{
    assert_stream_closed, compile, connect_with_retry, free_port, make_binary_path,
    read_http_response, write_bytes_in_chunks, TestServer,
};
use std::fs;
use std::io::Write;
use std::net::TcpStream;
use std::time::Duration;

fn spawn_http_server() -> anyhow::Result<(assert_fs::TempDir, TestServer, u16, std::path::PathBuf)>
{
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    src_dir.create_dir_all()?;

    let port = free_port()?;
    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.http

let mut app = std.http.App::new()

app.get("/hello", |req, res| {{
    res.send_text("hello")
}})

app.get("/query", |req, res| {{
    author := match req.query("author") {{
        std.option.Option::Some(value) => value,
        std.option.Option::None => std.string.String::from("missing"),
    }}
    res.send_text(author.as_str())
}})

app.post("/echo", |req, res| {{
    res.send_bytes(req.body)
}})

app.listen(ip="127.0.0.1", port={port})
"#
    ))?;

    let binary_path = make_binary_path("http-runtime-tests", "server", port)?;
    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let server = TestServer::spawn(&binary_path, tempdir.path(), port)?;
    Ok((tempdir, server, port, binary_path))
}

fn runtime_stream(port: u16) -> anyhow::Result<TcpStream> {
    let addr = format!("127.0.0.1:{port}");
    let stream = connect_with_retry(&addr, Duration::from_secs(10))?;
    stream.set_nodelay(true)?;
    stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    Ok(stream)
}

#[test]
fn std_http_keeps_connections_alive_and_handles_partial_request_delivery() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_http_server()?;
    let mut stream = runtime_stream(port)?;

    let first_request = b"GET /hello HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: keep-alive\r\n\r\n";
    write_bytes_in_chunks(&mut stream, first_request, &[8, 9, 7])?;

    let first_response = read_http_response(&mut stream)?;
    assert_eq!(first_response.status_code, 200);
    assert_eq!(first_response.body, b"hello");

    let second_request = concat!(
        "POST /echo HTTP/1.1\r\n",
        "Host: 127.0.0.1\r\n",
        "Content-Length: 11\r\n",
        "Connection: keep-alive\r\n",
        "\r\n",
        "hello world"
    )
    .as_bytes()
    .to_vec();
    write_bytes_in_chunks(&mut stream, &second_request, &[5, 17, 11, 9, 3, 2])?;

    let second_response = read_http_response(&mut stream)?;
    assert_eq!(second_response.status_code, 200);
    assert_eq!(second_response.body, b"hello world");

    let third_request = concat!(
        "POST /echo HTTP/1.1\r\n",
        "Host: 127.0.0.1\r\n",
        "Content-Length: 4097\r\n",
        "Connection: keep-alive\r\n",
        "\r\n"
    )
    .as_bytes()
    .to_vec();
    write_bytes_in_chunks(&mut stream, &third_request, &[13, 7, 19])?;

    let third_response = read_http_response(&mut stream)?;
    assert_eq!(third_response.status_code, 413);
    assert!(third_response.body.is_empty());
    assert_stream_closed(&mut stream)?;

    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn std_http_honors_close_semantics_and_rejects_bad_content_length() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_http_server()?;

    {
        let mut stream = runtime_stream(port)?;
        let request = b"GET /hello HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n";
        stream.write_all(request)?;

        let response = read_http_response(&mut stream)?;
        assert_eq!(response.status_code, 200);
        assert_eq!(response.body, b"hello");
        assert_stream_closed(&mut stream)?;
    }

    {
        let mut stream = runtime_stream(port)?;
        let request = b"GET /hello HTTP/1.0\r\nHost: 127.0.0.1\r\n\r\n";
        stream.write_all(request)?;

        let response = read_http_response(&mut stream)?;
        assert_eq!(response.status_code, 200);
        assert_eq!(response.body, b"hello");
        assert_stream_closed(&mut stream)?;
    }

    {
        let mut stream = runtime_stream(port)?;
        let request = concat!(
            "POST /echo HTTP/1.1\r\n",
            "Host: 127.0.0.1\r\n",
            "Content-Length: nope\r\n",
            "Connection: close\r\n",
            "\r\n"
        );
        stream.write_all(request.as_bytes())?;

        let response = read_http_response(&mut stream)?;
        assert_eq!(response.status_code, 400);
        assert!(response.body.is_empty());
        assert_stream_closed(&mut stream)?;
    }

    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn std_http_query_percent_decoding_preserves_utf8() -> anyhow::Result<()> {
    let (_tempdir, _server, port, binary_path) = spawn_http_server()?;

    let response =
        http_test_support::http_get(port, "/query?author=%E3%83%9E%E3%82%A4%E3%82%B1%E3%83%AB")?;
    assert_eq!(response.status_code, 200);
    assert_eq!(std::str::from_utf8(&response.body)?, "マイケル");

    let _ = fs::remove_file(binary_path);
    Ok(())
}
