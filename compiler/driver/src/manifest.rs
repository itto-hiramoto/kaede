use std::{
    fs,
    io::ErrorKind,
    path::{Path, PathBuf},
};

use anyhow::Context as _;
use serde::Deserialize;

pub(crate) const MANIFEST_FILENAME: &str = "Kaede.toml";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct KaedeManifest {
    #[allow(dead_code)]
    pub package: PackageSection,
    pub build: BuildSection,
    // Presence of `[rust]` enables Rust interop; `rust.path` tells the
    // compiler which subdirectory holds the Rust crate.
    pub rust: Option<RustSection>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PackageSection {
    #[allow(dead_code)]
    pub name: String,
    #[allow(dead_code)]
    pub version: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuildSection {
    pub src: PathBuf,
    pub out: PathBuf,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct RustSection {
    pub path: PathBuf,
}

pub(crate) fn load_from_cwd() -> anyhow::Result<KaedeManifest> {
    let path = Path::new(MANIFEST_FILENAME);
    let contents = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) if err.kind() == ErrorKind::NotFound => {
            let cwd = std::env::current_dir()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|_| String::from("unknown"));
            anyhow::bail!(
                "`{MANIFEST_FILENAME}` not found in the current directory.\n\
                 `kaede build`/`kaede run` require a Kaede project manifest.\n\
                 To create a new project: kaede new <project_name> [--rust]\n\
                 Current directory: {cwd}"
            );
        }
        Err(err) => {
            return Err(err).with_context(|| format!("Failed to read {MANIFEST_FILENAME}"));
        }
    };

    toml::from_str::<KaedeManifest>(&contents)
        .with_context(|| format!("Failed to parse {MANIFEST_FILENAME}"))
}

pub(crate) fn render_default(name: &str, with_rust: bool) -> String {
    let mut out = String::new();
    out.push_str("[package]\n");
    out.push_str(&format!("name = \"{name}\"\n"));
    out.push_str("version = \"0.1.0\"\n");
    out.push('\n');
    out.push_str("[build]\n");
    out.push_str("src = \"src\"\n");
    out.push_str(&format!("out = \"build/{name}\"\n"));
    if with_rust {
        out.push('\n');
        out.push_str("[rust]\n");
        out.push_str("path = \"rust\"\n");
    }
    out
}
