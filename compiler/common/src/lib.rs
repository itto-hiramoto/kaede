use std::path::PathBuf;

fn kaede_dir() -> PathBuf {
    PathBuf::from(
        std::env::var("KAEDE_DIR").unwrap_or(concat!(env!("HOME"), "/.kaede").to_string()),
    )
}

pub fn kaede_gc_lib_path() -> PathBuf {
    kaede_dir().join("lib/libkgc.so")
}

pub fn kaede_lib_path() -> PathBuf {
    kaede_dir().join("lib/libkd.so")
}

pub fn kaede_lib_src_dir() -> PathBuf {
    kaede_dir().join("lib/src")
}

pub fn kaede_autoload_dir() -> PathBuf {
    kaede_lib_src_dir().join("autoload")
}

pub fn rust_function_prefix() -> &'static str {
    "kaede_rust_bridge_"
}
