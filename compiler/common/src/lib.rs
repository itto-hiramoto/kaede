use std::{ffi::OsString, fs, path::PathBuf};

fn kaede_dir() -> PathBuf {
    PathBuf::from(
        std::env::var("KAEDE_DIR").unwrap_or(concat!(env!("HOME"), "/.kaede").to_string()),
    )
}

fn kaede_lib_dir() -> PathBuf {
    kaede_dir().join("lib")
}

fn kaede_third_party_lib_dirs() -> Vec<PathBuf> {
    let mut lib_dirs = match fs::read_dir(kaede_dir().join("third_party")) {
        Ok(entries) => entries
            .filter_map(Result::ok)
            .map(|entry| entry.path().join("lib"))
            .filter(|path| path.is_dir())
            .collect::<Vec<_>>(),
        Err(_) => Vec::new(),
    };
    lib_dirs.sort();
    lib_dirs
}

pub fn lib_extension() -> &'static str {
    if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    }
}

pub fn kaede_gc_lib_path() -> PathBuf {
    kaede_lib_dir().join(format!("libkgc.{}", lib_extension()))
}

pub fn kaede_lib_path() -> PathBuf {
    kaede_lib_dir().join(format!("libkd.{}", lib_extension()))
}

pub fn kaede_runtime_lib_path() -> PathBuf {
    kaede_lib_dir().join("libkaede_runtime.a")
}

pub fn kaede_lib_src_dir() -> PathBuf {
    kaede_lib_dir().join("src")
}

pub fn kaede_autoload_dir() -> PathBuf {
    kaede_lib_src_dir().join("autoload")
}

pub fn kaede_runtime_linker_flags() -> Vec<OsString> {
    let mut flags = Vec::new();

    for lib_dir in std::iter::once(kaede_lib_dir()).chain(kaede_third_party_lib_dirs()) {
        flags.push(OsString::from("-Xlinker"));
        flags.push(OsString::from("-rpath"));
        flags.push(OsString::from("-Xlinker"));
        flags.push(lib_dir.into_os_string());
    }

    flags
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangLinkage {
    Default,
    C,
}
