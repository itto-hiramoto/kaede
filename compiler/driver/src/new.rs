use std::{fs, path::Path, process::Command};

use anyhow::Context as _;

fn ensure_project_does_not_exist(project_dir: &Path, project_name: &str) -> anyhow::Result<()> {
    if project_dir.exists() {
        anyhow::bail!("Directory '{project_name}' already exists");
    }

    Ok(())
}

fn create_kaede_only_project(project_dir: &Path, project_name: &str) -> anyhow::Result<()> {
    let src_dir = project_dir.join("src");

    fs::create_dir_all(&src_dir)?;
    fs::write(
        src_dir.join("main.kd"),
        r#"fn main(): i32 {
    return 0
}"#,
    )?;

    println!("✅ Successfully created Kaede project: {project_name}");
    println!("📁 Project structure:");
    println!("  {project_name}/");
    println!("  └── src/");
    println!("      └── main.kd");
    println!();
    println!("🚀 To get started:");
    println!("  1. Add your Kaede files in {project_name}/src/");
    println!("  2. Build with: cd {project_name} && kaede build");
    println!("  3. Need Rust bindings? Recreate with: kaede new {project_name} --rust");

    Ok(())
}

fn create_rust_bridge_project(project_dir: &Path, project_name: &str) -> anyhow::Result<()> {
    // Step 1: Create new Rust library project
    let status = Command::new("cargo")
        .args(["new", "--lib", project_name])
        .status()
        .context("Failed to run 'cargo new --lib'")?;

    if !status.success() {
        anyhow::bail!("Failed to create new Rust library project");
    }

    let rust_dir = project_dir.join("rust");
    let src_dir = project_dir.join("src");

    // Create rust subdirectory and move cargo files there
    fs::create_dir_all(&rust_dir)?;
    fs::rename(project_dir.join("Cargo.toml"), rust_dir.join("Cargo.toml"))?;
    fs::rename(project_dir.join("src"), rust_dir.join("src"))?;

    // Step 2: Modify Cargo.toml to add workspace section
    let cargo_toml_path = rust_dir.join("Cargo.toml");
    let mut cargo_toml_content = fs::read_to_string(&cargo_toml_path)?;

    // Add workspace section
    cargo_toml_content.push_str("\n[workspace]\n");

    fs::write(&cargo_toml_path, cargo_toml_content)?;

    // Step 3: Create src directory
    fs::create_dir_all(&src_dir)?;
    fs::write(
        src_dir.join("main.kd"),
        format!(
            "import rust::{project_name}\n\nfn main(): i32 {{\n    return rust::{project_name}::add(10, 20)\n}}"
        ),
    )?;

    // Step 4: Create lib.rs file
    let lib_rs_path = rust_dir.join("src/lib.rs");
    let lib_rs_content = r#"pub fn add(a: i32, b: i32) -> i32 {
    a + b
}"#;
    fs::write(&lib_rs_path, lib_rs_content)?;

    println!("✅ Successfully created Kaede Rust interop project: {project_name}");
    println!("📁 Project structure:");
    println!("  {project_name}/");
    println!("  ├── src/");
    println!("  │   └── main.kd");
    println!("  └── rust/");
    println!("      ├── Cargo.toml");
    println!("      └── src/");
    println!("          └── lib.rs");
    println!();
    println!("🚀 To get started:");
    println!("  1. Add your Rust functions to {project_name}/rust/src/lib.rs");
    println!("  2. Create your Kaede files in {project_name}/src/");
    println!("  3. Build with: cd {project_name} && kaede build");

    Ok(())
}

pub(crate) fn create_new_project(project_name: String, with_rust: bool) -> anyhow::Result<()> {
    if with_rust
        && !project_name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        anyhow::bail!(
            "Rust interop projects currently require an ASCII crate name (letters/digits/_): `{project_name}`"
        );
    }

    let project_dir = Path::new(&project_name);

    ensure_project_does_not_exist(project_dir, &project_name)?;

    if with_rust {
        create_rust_bridge_project(project_dir, &project_name)?;
    } else {
        create_kaede_only_project(project_dir, &project_name)?;
    }

    Ok(())
}
