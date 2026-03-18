use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::io::{Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

fn compile(file_paths: &[&Path], root_dir: &Path, output_path: &Path) -> anyhow::Result<()> {
    let mut args = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<String>>();

    args.push("-o".to_string());
    args.push(output_path.to_string_lossy().to_string());
    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .assert()
        .success();

    Ok(())
}

fn free_port() -> anyhow::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    let port = listener.local_addr()?.port();
    drop(listener);
    Ok(port)
}

struct TestServer {
    child: Child,
}

impl TestServer {
    fn spawn(binary: &Path, workdir: &Path, port: u16) -> anyhow::Result<Self> {
        let mut child = Command::new(binary)
            .current_dir(workdir)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()?;

        let addr = format!("127.0.0.1:{port}");
        let deadline = Instant::now() + Duration::from_secs(10);

        loop {
            if TcpStream::connect(&addr).is_ok() {
                break;
            }

            if let Some(status) = child.try_wait()? {
                anyhow::bail!("server exited early with status {status}");
            }

            if Instant::now() >= deadline {
                anyhow::bail!("server did not start listening on {addr}");
            }

            thread::sleep(Duration::from_millis(50));
        }

        Ok(Self { child })
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

struct HttpResponse {
    status_code: u16,
    headers: HashMap<String, String>,
    body: Vec<u8>,
}

fn http_get(port: u16, path: &str) -> anyhow::Result<HttpResponse> {
    let addr = format!("127.0.0.1:{port}");
    let mut stream = TcpStream::connect(&addr)?;
    stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    stream.write_all(
        format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n").as_bytes(),
    )?;
    stream.shutdown(Shutdown::Write)?;

    let mut raw = Vec::new();
    stream.read_to_end(&mut raw)?;

    let header_end = raw
        .windows(4)
        .position(|window| window == b"\r\n\r\n")
        .ok_or_else(|| anyhow::anyhow!("missing HTTP header terminator"))?;

    let header_bytes = &raw[..header_end];
    let body = raw[(header_end + 4)..].to_vec();
    let header_text = String::from_utf8(header_bytes.to_vec())?;
    let mut lines = header_text.lines();
    let status_line = lines
        .next()
        .ok_or_else(|| anyhow::anyhow!("missing HTTP status line"))?;
    let status_code = status_line
        .split_whitespace()
        .nth(1)
        .ok_or_else(|| anyhow::anyhow!("missing HTTP status code"))?
        .parse::<u16>()?;

    let mut headers = HashMap::new();
    for line in lines {
        if let Some((name, value)) = line.split_once(':') {
            headers.insert(name.trim().to_ascii_lowercase(), value.trim().to_string());
        }
    }

    Ok(HttpResponse {
        status_code,
        headers,
        body,
    })
}

#[test]
fn std_http_serves_static_files_with_expected_routing_rules() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    let public_dir = tempdir.child("public");
    let assets_dir = public_dir.child("assets");
    src_dir.create_dir_all()?;
    assets_dir.create_dir_all()?;

    public_dir
        .child("index.html")
        .write_str("<!doctype html><title>static home</title>")?;
    public_dir
        .child("hello.txt")
        .write_str("static file should lose")?;
    public_dir.child("secret.txt").write_str("keep out")?;
    assets_dir
        .child("app.css")
        .write_str("body { color: tomato; }")?;

    let port = free_port()?;
    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.http

let mut app = std.http.App::new()

app.get("/hello", |req, res| {{
    res.send_string("route wins")
}})

app.static_file("/", "public/index.html")
app.static_file("/hello", "public/hello.txt")
app.static_dir("/assets", "public/assets")

app.listen(ip="127.0.0.1", port={port})
"#
    ))?;

    let binary_dir = std::env::current_dir()?
        .join("target")
        .join("http-static-tests");
    fs::create_dir_all(&binary_dir)?;
    let binary_path = binary_dir.join(format!("server-{}-{}", std::process::id(), port));

    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let _server = TestServer::spawn(&binary_path, tempdir.path(), port)?;

    let root = http_get(port, "/")?;
    assert_eq!(root.status_code, 200);
    assert_eq!(
        root.headers.get("content-type").map(String::as_str),
        Some("text/html; charset=utf-8")
    );
    assert_eq!(
        root.headers.get("cache-control").map(String::as_str),
        Some("no-store")
    );
    assert!(String::from_utf8(root.body)?.contains("static home"));

    let route = http_get(port, "/hello")?;
    assert_eq!(route.status_code, 200);
    assert_eq!(String::from_utf8(route.body)?, "route wins");

    let asset = http_get(port, "/assets/app.css")?;
    assert_eq!(asset.status_code, 200);
    assert_eq!(
        asset.headers.get("content-type").map(String::as_str),
        Some("text/css; charset=utf-8")
    );
    assert!(String::from_utf8(asset.body)?.contains("tomato"));

    let traversal = http_get(port, "/assets/../secret.txt")?;
    assert_eq!(traversal.status_code, 404);

    let encoded_traversal = http_get(port, "/assets/%2e%2e/secret.txt")?;
    assert_eq!(encoded_traversal.status_code, 404);

    let missing = http_get(port, "/assets/missing.js")?;
    assert_eq!(missing.status_code, 404);

    let _ = fs::remove_file(binary_path);

    Ok(())
}
