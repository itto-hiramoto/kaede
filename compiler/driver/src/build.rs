use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context as _;
use inkwell::OptimizationLevel;

use crate::{compile_and_link, manifest, select_entry_unit, CompileOption, CompileUnitInfo};

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
    for file_path in &file_paths {
        unit_infos.push(CompileUnitInfo {
            program: fs::read_to_string(file_path)
                .with_context(|| format!("Failed to read file: {}", file_path.display()))?,
            file_path: file_path.clone(),
            is_entry_unit: false,
        });
    }

    select_entry_unit(&mut unit_infos)?;

    Ok(unit_infos)
}

pub(crate) fn build_project() -> anyhow::Result<PathBuf> {
    let manifest = manifest::load_from_cwd()?;
    let src_root = manifest.build.src;
    let output_path = manifest.build.out;

    if !src_root.exists() {
        anyhow::bail!(
            "Kaede source directory '{}' does not exist (configured by build.src in Kaede.toml)",
            src_root.display()
        );
    }

    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).with_context(|| {
                format!("Failed to create output directory '{}'", parent.display())
            })?;
        }
    }

    let unit_infos = collect_kaede_unit_infos(&src_root)?;

    let option = CompileOption {
        opt_level: OptimizationLevel::Default,
        display_llvm_ir: false,
        output_file_path: output_path.clone(),
        root_dir: Some(src_root),
        no_autoload: false,
        no_prelude: false,
        no_gc: false,
        additional_libs: Vec::new(),
    };

    println!("🔗 Linking Kaede executable...");
    compile_and_link(unit_infos, option)?;

    println!("✅ Build completed successfully!");
    println!("📁 Output: {}", output_path.display());
    println!("🚀 Run with: ./{}", output_path.display());

    Ok(output_path)
}
