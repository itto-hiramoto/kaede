mod http_test_support;

use assert_fs::prelude::*;
use http_test_support::{compile, free_port, http_get, make_binary_path, TestServer};
use std::fs;

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
    res.send("route wins")
}})

app.static_file("/", "public/index.html")
app.static_file("/hello", "public/hello.txt")
app.static_dir("/assets", "public/assets")

app.listen(ip="127.0.0.1", port={port})
"#
    ))?;

    let binary_path = make_binary_path("http-static-tests", "server", port)?;

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
