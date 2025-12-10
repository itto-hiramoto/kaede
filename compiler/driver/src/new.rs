use std::{fs, path::Path, process::Command};

use anyhow::Context as _;
use kaede_common::kaede_rust_bridge_codegen_path;

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

    // Step 2: Modify Cargo.toml to add cdylib crate-type and dependencies
    let cargo_toml_path = rust_dir.join("Cargo.toml");
    let mut cargo_toml_content = fs::read_to_string(&cargo_toml_path)?;

    // Add [lib] section with crate-type at the beginning
    let lib_section = "[lib]\ncrate-type = [\"cdylib\"]\n\n";
    cargo_toml_content = lib_section.to_string() + &cargo_toml_content;

    // Add build-dependencies section
    cargo_toml_content.push_str("\n[build-dependencies]\n");
    let codegen_path = kaede_rust_bridge_codegen_path();
    cargo_toml_content.push_str(&format!(
        "kaede-rust-bridge-codegen = {{ path = \"{}\" }}\n",
        codegen_path.display()
    ));

    // Add workspace section
    cargo_toml_content.push_str("\n[workspace]\n");

    fs::write(&cargo_toml_path, cargo_toml_content)?;

    // Step 3: Create build.rs file
    let build_rs_path = rust_dir.join("build.rs");
    let build_rs_content = r#"fn main() {
    kaede_rust_bridge_codegen::generate("src/lib.rs", "../src").unwrap();
}
"#;
    fs::write(&build_rs_path, build_rs_content)?;

    // Step 4: Create src directory
    fs::create_dir_all(&src_dir)?;
    fs::write(
        src_dir.join("main.kd"),
        r#"import krb_generated
use krb_generated.*

fn main(): i32 {
    if is_even(2) {
        greetings()
    }

    return 0
}"#,
    )?;

    // Step 5: Create lib.rs file
    let lib_rs_path = rust_dir.join("src/lib.rs");
    let lib_rs_content = r#"pub fn greetings() {
    println!("hello, world!");
}

pub fn is_even(n: i32) -> bool {
    n % 2 == 0
}

include!(concat!(env!("OUT_DIR"), "/kaede_bindings.rs"));"#;
    fs::write(&lib_rs_path, lib_rs_content)?;

    println!("✅ Successfully created Kaede Rust bridge project: {project_name}");
    println!("📁 Project structure:");
    println!("  {project_name}/");
    println!("  ├── src/");
    println!("  │   └── main.kd");
    println!("  └── rust/");
    println!("      ├── Cargo.toml");
    println!("      ├── build.rs");
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
    let project_dir = Path::new(&project_name);

    ensure_project_does_not_exist(project_dir, &project_name)?;

    if with_rust {
        create_rust_bridge_project(project_dir, &project_name)?;
    } else {
        create_kaede_only_project(project_dir, &project_name)?;
    }

    Ok(())
}
