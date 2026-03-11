use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context as _;
use inkwell::OptimizationLevel;

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
    let mut file_paths = find_kd_files(src_root)?;
    file_paths.sort();

    if file_paths.is_empty() {
        anyhow::bail!("No .kd files found in kaede directory");
    }

    println!("🔨 Compiling Kaede files...");
    let mut unit_infos = Vec::new();
    let entry_path = src_root.join("main.kd");
    for file_path in &file_paths {
        unit_infos.push(CompileUnitInfo {
            program: fs::read_to_string(file_path)
                .with_context(|| format!("Failed to read file: {}", file_path.display()))?,
            file_path: file_path.clone(),
            is_entry_unit: file_path == &entry_path,
        });
    }

    Ok(unit_infos)
}

pub(crate) fn build_project() -> anyhow::Result<()> {
    ensure_kaede_project_root()?;

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
        additional_libs: Vec::new(),
    };

    println!("🔗 Linking Kaede executable...");
    compile_and_link(unit_infos, option)?;

    println!("✅ Build completed successfully!");
    println!("📁 Output: build/main");
    println!("🚀 Run with: ./build/main");

    Ok(())
}
