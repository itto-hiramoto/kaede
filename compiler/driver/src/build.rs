use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, Context as _};
use inkwell::OptimizationLevel;
use kaede_common::lib_extension;

use crate::{compile_and_link, CompileOption, CompileUnitInfo};

fn ensure_kaede_project_root() -> anyhow::Result<()> {
    if !Path::new("src").exists() {
        anyhow::bail!(
            "This command expects a Kaede project.\n\
             Expected structure:\n\
             └── src/           (Kaede source files)\n\
             \n\
             To create a new project: kaede new <project_name> [--rust]\n\
             Current directory: {}",
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("unknown"))
                .display()
        );
    }

    Ok(())
}

fn build_rust_library_if_present() -> anyhow::Result<Vec<PathBuf>> {
    let mut additional_libs = Vec::new();

    if Path::new("rust").exists() {
        if !Path::new("rust/Cargo.toml").exists() {
            anyhow::bail!(
                "rust/Cargo.toml not found. Make sure you're in a Kaede Rust bridge project directory.\n\
                 Current directory: {}",
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("unknown"))
                    .display()
            );
        }

        println!("🔨 Building Rust library...");
        let status = Command::new("cargo")
            .args(["build"])
            .current_dir("rust")
            .status()
            .context("Failed to run 'cargo build'")?;

        if !status.success() {
            anyhow::bail!("Failed to build Rust library");
        }

        let cargo_toml_content =
            fs::read_to_string("rust/Cargo.toml").context("Failed to read rust/Cargo.toml")?;

        let package_name = cargo_toml_content
            .lines()
            .find(|line| line.starts_with("name = "))
            .and_then(|line| line.split('"').nth(1))
            .ok_or_else(|| anyhow!("Could not find package name in rust/Cargo.toml"))?;

        let rust_lib_path = format!(
            "rust/target/debug/lib{}.{}",
            package_name.replace('-', "_"),
            lib_extension()
        );
        if !Path::new(&rust_lib_path).exists() {
            anyhow::bail!(
                "Rust library not found at: {}. Make sure cargo build succeeded.",
                rust_lib_path
            );
        }

        additional_libs.push(PathBuf::from(rust_lib_path));
    }

    Ok(additional_libs)
}

fn find_kd_files(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut kd_files = Vec::new();
    let entries = fs::read_dir(dir)?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().is_some_and(|ext| ext == "kd") {
            kd_files.push(path);
        } else if path.is_dir() {
            kd_files.extend(find_kd_files(&path)?);
        }
    }

    Ok(kd_files)
}

fn collect_kaede_unit_infos(src_root: &Path) -> anyhow::Result<Vec<CompileUnitInfo>> {
    let file_paths = find_kd_files(src_root)?;

    if file_paths.is_empty() {
        anyhow::bail!("No .kd files found in kaede directory");
    }

    println!("🔨 Compiling Kaede files...");
    let mut unit_infos = Vec::new();
    for file_path in &file_paths {
        unit_infos.push(CompileUnitInfo {
            program: fs::read_to_string(file_path)
                .with_context(|| format!("Failed to read file: {}", file_path.display()))?,
            file_path: file_path.clone(),
        });
    }

    Ok(unit_infos)
}

pub(crate) fn build_project() -> anyhow::Result<()> {
    ensure_kaede_project_root()?;

    let additional_libs = build_rust_library_if_present()?;

    fs::create_dir_all("build").context("Failed to create build directory")?;

    let src_root = PathBuf::from("src");
    let unit_infos = collect_kaede_unit_infos(&src_root)?;

    let option = CompileOption {
        opt_level: OptimizationLevel::Default,
        display_llvm_ir: false,
        output_file_path: PathBuf::from("build/main"),
        root_dir: Some(src_root),
        no_autoload: false,
        no_prelude: false,
        no_gc: false,
        additional_libs,
    };

    if option.additional_libs.is_empty() {
        println!("🔗 Linking Kaede executable...");
    } else {
        println!("🔗 Linking with Rust library...");
    }
    compile_and_link(unit_infos, option)?;

    println!("✅ Build completed successfully!");
    println!("📁 Output: build/main");
    println!("🚀 Run with: ./build/main");

    Ok(())
}
