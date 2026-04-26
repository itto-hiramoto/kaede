use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::fs;
use std::io::{Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::{Duration, Instant};

const SOCKET_IO_TIMEOUT: Duration = Duration::from_secs(5);
const CHILD_EXIT_TIMEOUT: Duration = Duration::from_secs(15);

fn test_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

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

fn connect_with_retry(addr: &str, timeout: Duration) -> anyhow::Result<TcpStream> {
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

struct ChildGuard {
    child: Child,
}

impl ChildGuard {
    fn spawn(binary: &Path, workdir: &Path) -> anyhow::Result<Self> {
        let child = Command::new(binary)
            .current_dir(workdir)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()?;
        Ok(Self { child })
    }

    fn wait_for_exit(&mut self, timeout: Duration) -> anyhow::Result<std::process::ExitStatus> {
        let deadline = Instant::now() + timeout;
        loop {
            if let Some(status) = self.child.try_wait()? {
                return Ok(status);
            }

            if Instant::now() >= deadline {
                anyhow::bail!("child did not exit within {:?}", timeout);
            }

            thread::sleep(Duration::from_millis(50));
        }
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

#[test]
fn std_sys_accept_does_not_starve_runnable_handlers() -> anyhow::Result<()> {
    let _guard = test_lock().lock().unwrap();
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    src_dir.create_dir_all()?;

    let port = free_port()?;
    let idle_connections = std::thread::available_parallelism()
        .map(|count| count.get() + 8)
        .unwrap_or(16);
    let total_connections = idle_connections + 1;

    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.sys
import std.sync
import std.net.tcp

use std.sync.WaitGroup

fun handle(conn: std.sys.Fd, wg: mut WaitGroup) {{
    let mut buf = [0; 1]
    let n = match conn.read(buf) {{
        std.result.Result::Ok(value) => value,
        std.result.Result::Err(_) => {{
            std.sys.close_quiet(conn)
            wg.done()
            return
        }}
    }}

    if n > 0 && conn.write(b"ok").is_err() {{
        std.sys.close_quiet(conn)
        wg.done()
        return
    }}

    std.sys.close_quiet(conn)
    wg.done()
}}

fun main() -> i32 {{
    let listener = std.net.tcp.socket_tcp4()
    std.net.tcp.set_reuseaddr(listener)
    std.net.tcp.bind_ipv4(listener, "127.0.0.1", {port})
    std.net.tcp.listen(listener, std.net.tcp.somaxconn())

    let mut wg = WaitGroup::new()
    let mut i = 0
    while i < {total_connections} {{
        let conn = std.net.tcp.accept(listener)
        wg.add(1)
        spawn handle(conn, wg)
        i += 1
    }}

    std.sys.close(listener)
    wg.wait()
    return 0
}}
"#
    ))?;

    let binary_dir = std::env::current_dir()?
        .join("target")
        .join("runtime-netpoll-tests");
    fs::create_dir_all(&binary_dir)?;
    let binary_path = binary_dir.join(format!("accept-starvation-{}-{}", std::process::id(), port));

    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let mut server = ChildGuard::spawn(&binary_path, tempdir.path())?;

    let addr = format!("127.0.0.1:{port}");
    let mut idle_streams = Vec::with_capacity(idle_connections);
    idle_streams.push(connect_with_retry(&addr, Duration::from_secs(10))?);
    for _ in 1..idle_connections {
        idle_streams.push(connect_with_retry(&addr, Duration::from_secs(10))?);
    }

    let mut active = connect_with_retry(&addr, Duration::from_secs(10))?;
    active.set_read_timeout(Some(SOCKET_IO_TIMEOUT))?;
    active.write_all(b"x")?;

    let mut reply = [0u8; 2];
    active.read_exact(&mut reply)?;
    assert_eq!(&reply, b"ok");
    let _ = active.shutdown(Shutdown::Both);

    drop(idle_streams);

    let status = server.wait_for_exit(CHILD_EXIT_TIMEOUT)?;
    assert!(status.success());

    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn std_sys_close_wakes_tasks_parked_on_the_same_fd() -> anyhow::Result<()> {
    let _guard = test_lock().lock().unwrap();
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    src_dir.create_dir_all()?;

    let port = free_port()?;
    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.sys
import std.sync
import std.net.tcp

use std.sync.WaitGroup

fun reader(conn: std.sys.Fd, wg: mut WaitGroup) {{
    let mut buf = [0; 1]
    let _ = conn.read(buf)
    wg.done()
}}

fun closer(conn: std.sys.Fd, wg: mut WaitGroup) {{
    std.sys.sleep_ms(100)
    std.sys.close_quiet(conn)
    wg.done()
}}

fun main() -> i32 {{
    let listener = std.net.tcp.socket_tcp4()
    std.net.tcp.set_reuseaddr(listener)
    std.net.tcp.bind_ipv4(listener, "127.0.0.1", {port})
    std.net.tcp.listen(listener, std.net.tcp.somaxconn())

    let conn = std.net.tcp.accept(listener)
    std.sys.close(listener)

    let mut wg = WaitGroup::new()
    wg.add(2)
    spawn reader(conn, wg)
    spawn closer(conn, wg)
    wg.wait()
    return 0
}}
"#
    ))?;

    let binary_dir = std::env::current_dir()?
        .join("target")
        .join("runtime-netpoll-tests");
    fs::create_dir_all(&binary_dir)?;
    let binary_path = binary_dir.join(format!("close-wake-{}-{}", std::process::id(), port));

    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let mut server = ChildGuard::spawn(&binary_path, tempdir.path())?;

    let addr = format!("127.0.0.1:{port}");
    let client = connect_with_retry(&addr, Duration::from_secs(10))?;
    client.set_read_timeout(Some(SOCKET_IO_TIMEOUT))?;

    let status = server.wait_for_exit(CHILD_EXIT_TIMEOUT)?;
    assert!(status.success());

    drop(client);
    let _ = fs::remove_file(binary_path);
    Ok(())
}

#[test]
fn runtime_shutdown_exits_with_background_tasks_still_parked_on_io() -> anyhow::Result<()> {
    let _guard = test_lock().lock().unwrap();
    let tempdir = assert_fs::TempDir::new()?;
    let src_dir = tempdir.child("src");
    src_dir.create_dir_all()?;

    let port = free_port()?;
    let main = src_dir.child("main.kd");
    main.write_str(&format!(
        r#"import std.sys
import std.net.tcp

fun reader(conn: std.sys.Fd) {{
    let mut buf = [0; 1]
    let _ = conn.read(buf)
}}

fun main() -> i32 {{
    let listener = std.net.tcp.socket_tcp4()
    std.net.tcp.set_reuseaddr(listener)
    std.net.tcp.bind_ipv4(listener, "127.0.0.1", {port})
    std.net.tcp.listen(listener, std.net.tcp.somaxconn())

    let conn = std.net.tcp.accept(listener)
    std.sys.close(listener)
    spawn reader(conn)
    std.sys.sleep_ms(100)
    return 0
}}
"#
    ))?;

    let binary_dir = std::env::current_dir()?
        .join("target")
        .join("runtime-netpoll-tests");
    fs::create_dir_all(&binary_dir)?;
    let binary_path = binary_dir.join(format!("shutdown-parked-{}-{}", std::process::id(), port));

    compile(&[main.path()], tempdir.path(), &binary_path)?;
    let mut server = ChildGuard::spawn(&binary_path, tempdir.path())?;

    let addr = format!("127.0.0.1:{port}");
    let client = connect_with_retry(&addr, Duration::from_secs(10))?;

    let status = server.wait_for_exit(CHILD_EXIT_TIMEOUT)?;
    assert!(status.success());

    drop(client);
    let _ = fs::remove_file(binary_path);
    Ok(())
}
