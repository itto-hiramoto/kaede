#![allow(dead_code)]
// Shared helper code is compiled into multiple integration test crates, and each
// crate exercises only the subset it needs.

use assert_cmd::prelude::*;
use std::collections::HashMap;
use std::io::{ErrorKind, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

pub fn compile(file_paths: &[&Path], root_dir: &Path, output_path: &Path) -> anyhow::Result<()> {
    let mut args = file_paths
        .iter()
        .map(|path| path.to_string_lossy().to_string())
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

pub fn free_port() -> anyhow::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    let port = listener.local_addr()?.port();
    drop(listener);
    Ok(port)
}

pub fn make_binary_path(suite: &str, name: &str, port: u16) -> anyhow::Result<PathBuf> {
    let binary_dir = std::env::current_dir()?.join("target").join(suite);
    std::fs::create_dir_all(&binary_dir)?;
    Ok(binary_dir.join(format!("{name}-{}-{port}", std::process::id())))
}

pub fn connect_with_retry(addr: &str, timeout: Duration) -> anyhow::Result<TcpStream> {
    let deadline = Instant::now() + timeout;
    loop {
        match TcpStream::connect(addr) {
            Ok(stream) => return Ok(stream),
            Err(err) => {
                if Instant::now() >= deadline {
                    return Err(err.into());
                }
                thread::sleep(Duration::from_millis(50));
            }
        }
    }
}

pub struct TestServer {
    child: Child,
}

impl TestServer {
    pub fn spawn(binary: &Path, workdir: &Path, port: u16) -> anyhow::Result<Self> {
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

#[derive(Debug)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

pub fn http_get(port: u16, path: &str) -> anyhow::Result<HttpResponse> {
    let addr = format!("127.0.0.1:{port}");
    let mut stream = TcpStream::connect(&addr)?;
    stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    stream.write_all(
        format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n").as_bytes(),
    )?;
    stream.shutdown(std::net::Shutdown::Write)?;
    read_http_response(&mut stream)
}

pub fn read_http_response(stream: &mut TcpStream) -> anyhow::Result<HttpResponse> {
    let mut raw = Vec::new();
    let mut buffer = [0u8; 1024];
    let mut header_end = None;
    let mut expected_body_len = None;

    loop {
        if let Some(end) = header_end {
            let body_len = expected_body_len.unwrap_or(0);
            if raw.len() >= end + 4 + body_len {
                break;
            }
        }

        let n = stream.read(&mut buffer)?;
        if n == 0 {
            break;
        }
        raw.extend_from_slice(&buffer[..n]);

        if header_end.is_none() {
            if let Some(pos) = raw.windows(4).position(|window| window == b"\r\n\r\n") {
                let header_text = std::str::from_utf8(&raw[..pos])?;
                expected_body_len = parse_content_length(header_text)?;
                header_end = Some(pos);
                if expected_body_len.is_none() || expected_body_len == Some(0) {
                    break;
                }
            }
        }
    }

    let header_end = header_end.ok_or_else(|| anyhow::anyhow!("missing HTTP header terminator"))?;
    let body_start = header_end + 4;
    let header_text = std::str::from_utf8(&raw[..header_end])?;
    let body_len = expected_body_len.unwrap_or(0);
    if raw.len() < body_start + body_len {
        anyhow::bail!("response closed before full body was received");
    }

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
        body: raw[body_start..(body_start + body_len)].to_vec(),
    })
}

fn parse_content_length(header_text: &str) -> anyhow::Result<Option<usize>> {
    for line in header_text.lines().skip(1) {
        if let Some((name, value)) = line.split_once(':') {
            if name.trim().eq_ignore_ascii_case("content-length") {
                let body_len = value.trim().parse::<usize>()?;
                return Ok(Some(body_len));
            }
        }
    }

    Ok(None)
}

pub fn write_bytes_in_chunks(
    stream: &mut TcpStream,
    bytes: &[u8],
    chunk_sizes: &[usize],
) -> anyhow::Result<()> {
    if chunk_sizes.is_empty() {
        stream.write_all(bytes)?;
        return Ok(());
    }

    let mut offset = 0;
    for &chunk_size in chunk_sizes {
        if offset >= bytes.len() {
            break;
        }

        let end = (offset + chunk_size).min(bytes.len());
        stream.write_all(&bytes[offset..end])?;
        stream.flush()?;
        offset = end;

        if offset < bytes.len() {
            thread::sleep(Duration::from_millis(25));
        }
    }

    if offset < bytes.len() {
        stream.write_all(&bytes[offset..])?;
        stream.flush()?;
    }

    Ok(())
}

#[derive(Debug)]
pub struct WsFrame {
    pub fin: bool,
    pub opcode: u8,
    pub payload: Vec<u8>,
}

pub fn websocket_handshake(stream: &mut TcpStream, path: &str) -> anyhow::Result<HttpResponse> {
    let request = format!(
        "GET {path} HTTP/1.1\r\n\
         Host: 127.0.0.1\r\n\
         Upgrade: websocket\r\n\
         Connection: Upgrade\r\n\
         Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
         Sec-WebSocket-Version: 13\r\n\
         \r\n"
    );
    stream.write_all(request.as_bytes())?;
    read_http_response(stream)
}

pub fn send_ws_masked_frame(
    stream: &mut TcpStream,
    opcode: u8,
    payload: &[u8],
) -> anyhow::Result<()> {
    send_ws_masked_frame_in_chunks(stream, opcode, payload, &[])
}

pub fn send_ws_masked_frame_in_chunks(
    stream: &mut TcpStream,
    opcode: u8,
    payload: &[u8],
    chunk_sizes: &[usize],
) -> anyhow::Result<()> {
    let frame = build_masked_ws_frame(opcode, payload);
    write_bytes_in_chunks(stream, &frame, chunk_sizes)
}

fn build_masked_ws_frame(opcode: u8, payload: &[u8]) -> Vec<u8> {
    let mut frame = Vec::with_capacity(payload.len() + 14);
    let mask_key = [0x12u8, 0x34, 0x56, 0x78];

    frame.push(0x80 | (opcode & 0x0f));
    if payload.len() < 126 {
        frame.push(0x80 | (payload.len() as u8));
    } else if payload.len() < 65_536 {
        frame.push(0x80 | 126);
        frame.extend_from_slice(&(payload.len() as u16).to_be_bytes());
    } else {
        frame.push(0x80 | 127);
        frame.extend_from_slice(&(payload.len() as u64).to_be_bytes());
    }

    frame.extend_from_slice(&mask_key);
    for (index, byte) in payload.iter().enumerate() {
        frame.push(byte ^ mask_key[index % mask_key.len()]);
    }

    frame
}

pub fn read_ws_frame(stream: &mut TcpStream) -> anyhow::Result<WsFrame> {
    let mut first_bytes = [0u8; 2];
    stream.read_exact(&mut first_bytes)?;

    let first = first_bytes[0];
    let second = first_bytes[1];
    let fin = (first & 0x80) != 0;
    let opcode = first & 0x0f;
    let masked = (second & 0x80) != 0;
    if masked {
        anyhow::bail!("server frames must not be masked");
    }

    let mut payload_len = (second & 0x7f) as usize;
    if payload_len == 126 {
        let mut len_bytes = [0u8; 2];
        stream.read_exact(&mut len_bytes)?;
        payload_len = u16::from_be_bytes(len_bytes) as usize;
    } else if payload_len == 127 {
        let mut len_bytes = [0u8; 8];
        stream.read_exact(&mut len_bytes)?;
        payload_len = u64::from_be_bytes(len_bytes) as usize;
    }

    let mut payload = vec![0u8; payload_len];
    if payload_len > 0 {
        stream.read_exact(&mut payload)?;
    }

    Ok(WsFrame {
        fin,
        opcode,
        payload,
    })
}

pub fn assert_stream_closed(stream: &mut TcpStream) -> anyhow::Result<()> {
    let deadline = Instant::now() + Duration::from_secs(2);
    let mut buf = [0u8; 1];

    loop {
        match stream.read(&mut buf) {
            Ok(0) => return Ok(()),
            Ok(n) => anyhow::bail!("expected EOF, read {n} extra bytes"),
            Err(err)
                if err.kind() == ErrorKind::WouldBlock
                    || err.kind() == ErrorKind::TimedOut
                    || err.kind() == ErrorKind::Interrupted =>
            {
                if Instant::now() >= deadline {
                    anyhow::bail!("expected peer to close the connection");
                }
                thread::sleep(Duration::from_millis(25));
            }
            Err(err) => return Err(err.into()),
        }
    }
}
