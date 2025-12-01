use core::panic;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::Command,
    vec,
};

use anyhow::{anyhow, Context as _};
use colored::Colorize;
use inkwell::{context::Context, module::Module, OptimizationLevel};
use kaede_codegen::{error::CodegenError, CodeGenerator, CodegenCtx};
use kaede_common::{kaede_gc_lib_path, kaede_lib_path, kaede_rust_bridge_codegen_path};
use kaede_parse::Parser;
use kaede_semantic::SemanticAnalyzer;
use tempfile::{NamedTempFile, TempPath};

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,

    #[arg(long, action)]
    display_llvm_ir: bool,

    #[arg(value_name = "FILE")]
    files: Vec<PathBuf>,

    #[arg(
        short,
        long,
        help = "Instead of a file, we receive a program from the commandline argument"
    )]
    program: Option<String>,

    #[arg(short = 'o')]
    output: Option<PathBuf>,

    #[arg(short = 'O', default_value_t = 2, help = "Optimization level (0-3)")]
    opt_level: u8,

    #[arg(
        short = 'c',
        action,
        help = "Outputs object files without invoking the linker"
    )]
    c: bool,

    #[arg(long, help = "Project root directory")]
    root_dir: Option<PathBuf>,

    // Do not load standard libraries that automatically load
    // Will not be used except when building standard libraries
    #[arg(long, action)]
    no_autoload: bool,

    #[arg(long, action)]
    no_prelude: bool,

    #[arg(long, action)]
    no_gc: bool,
}

#[derive(clap::Subcommand, Debug)]
enum Commands {
    /// Create a new Kaede Rust bridge project
    New {
        /// Name of the project to create
        project_name: String,
    },
    /// Build a Kaede Rust bridge project
    Build,
}

fn to_inkwell_opt_level(level: u8) -> OptimizationLevel {
    match level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => panic!("Optimization levels range from 0 to 3!"),
    }
}

struct CompileUnitInfo {
    pub file_path: PathBuf,
    pub program: String,
}

fn emit_bitcode_to_tempfile(module: &Module) -> anyhow::Result<TempPath> {
    let tempfile = NamedTempFile::new()?;

    let temppath = tempfile.into_temp_path();

    module.write_bitcode_to_path(&temppath);

    Ok(temppath)
}

fn emit_object_file_to_tempfile(bitcode_path: &Path) -> anyhow::Result<TempPath> {
    let tempfile = NamedTempFile::new()?;

    let temppath = tempfile.into_temp_path();

    let status = Command::new("llc")
        .args([
            "-filetype=obj",
            "-relocation-model=pic", // Set relocation model
            "-o",
            &temppath.to_string_lossy(),
            &bitcode_path.to_string_lossy(),
        ])
        .status()?;

    if !status.success() {
        anyhow::bail!("Failed to emit object file using 'llc'")
    }

    Ok(temppath)
}

fn emit_exe_file(
    obj_path: &Path,
    output_file_path: &Path,
    additional_libs: &[PathBuf],
) -> anyhow::Result<()> {
    let mut args = vec![
        OsStr::new("-fPIE"), // Enable position-independent executable
        OsStr::new("-o"),
        output_file_path.as_os_str(),
        obj_path.as_os_str(),
    ];

    // Add additional libraries first (like Rust libraries)
    for lib in additional_libs {
        args.push(lib.as_os_str());
    }

    let kaede_lib_path = kaede_lib_path();
    let kaede_gc_lib_path = kaede_gc_lib_path();

    // Add standard libraries
    args.push(kaede_lib_path.as_os_str()); // Link with standard library
    args.push(kaede_gc_lib_path.as_os_str()); // Link with garbage collector

    let status = Command::new("cc").args(&args).status()?;

    if !status.success() {
        anyhow::bail!("Failed to emit executable file using 'cc'")
    }

    Ok(())
}

fn compile<'ctx>(
    cgcx: &'ctx CodegenCtx<'_>,
    unit_infos: Vec<CompileUnitInfo>,
    root_dir: &'ctx Path,
    no_autoload: bool,
    no_prelude: bool,
) -> anyhow::Result<Module<'ctx>> {
    let mut compiled_modules = Vec::new();

    for unit_info in unit_infos {
        let file = unit_info.file_path.into();

        let ast = Parser::new(&unit_info.program, file).run()?;

        let ir = SemanticAnalyzer::new(file, root_dir.to_path_buf()).analyze(
            ast,
            no_autoload,
            no_prelude,
        )?;

        let code_generator = CodeGenerator::new(cgcx)?;

        let module = code_generator.codegen(ir)?;

        compiled_modules.push(module);
    }

    let module = compiled_modules.pop().unwrap();

    // Link modules
    for other_module in compiled_modules {
        module
            .link_in_module(other_module)
            .map_err(|e| anyhow!(e.to_string()))?;
    }

    Ok(module)
}

fn display_optimized_llvm_ir(opt_level: OptimizationLevel, module: &Module) -> anyhow::Result<()> {
    let bitcode_path = emit_bitcode_to_tempfile(module)?;

    let status = Command::new("opt")
        .args([
            "-S",
            &format!("-O{}", opt_level as u32),
            &bitcode_path.to_string_lossy(),
        ])
        .status()?;

    if !status.success() {
        anyhow::bail!("Failed to optimize using 'opt'")
    }

    Ok(())
}

fn optimize_with_opt(
    opt_level: OptimizationLevel,
    bitcode_path: &Path,
) -> anyhow::Result<TempPath> {
    let tempfile = NamedTempFile::new()?;

    let temppath = tempfile.into_temp_path();

    let status = Command::new("opt")
        .args([
            &format!("-O{}", opt_level as u32),
            "-o",
            &temppath.to_string_lossy(),
            &bitcode_path.to_string_lossy(),
        ])
        .status()?;

    if !status.success() {
        anyhow::bail!("Failed to optimize using 'opt'")
    }

    Ok(temppath)
}

fn emit_optimized_object_file_to_tempfile(
    opt_level: OptimizationLevel,
    module: &Module,
) -> anyhow::Result<TempPath> {
    let bitcode_path = emit_bitcode_to_tempfile(module)?;

    let optimized_bitcode_path = optimize_with_opt(opt_level, &bitcode_path)?;

    emit_object_file_to_tempfile(&optimized_bitcode_path)
}

fn compile_and_output_obj(
    unit_infos: Vec<CompileUnitInfo>,
    option: CompileOption,
) -> anyhow::Result<()> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context, option.no_gc)?;

    let root_dir = option.root_dir.unwrap_or(PathBuf::from("."));

    let module = compile(
        &cgcx,
        unit_infos,
        &root_dir,
        option.no_autoload,
        option.no_prelude,
    )?;

    // Emit
    if option.display_llvm_ir {
        display_optimized_llvm_ir(option.opt_level, &module)?;
    } else {
        let obj_path = emit_optimized_object_file_to_tempfile(option.opt_level, &module)?;

        fs::rename(obj_path, option.output_file_path)?;
    }

    Ok(())
}

fn compile_and_link(unit_infos: Vec<CompileUnitInfo>, option: CompileOption) -> anyhow::Result<()> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context, option.no_gc)?;

    let root_dir = option.root_dir.unwrap_or(PathBuf::from("."));

    let module = compile(
        &cgcx,
        unit_infos,
        &root_dir,
        option.no_autoload,
        option.no_prelude,
    )?;

    if module.get_function("main").is_none() {
        return Err(CodegenError::MainNotFound.into());
    }

    // Emit
    if option.display_llvm_ir {
        display_optimized_llvm_ir(option.opt_level, &module)?;
    } else {
        let obj_path = emit_optimized_object_file_to_tempfile(option.opt_level, &module)?;

        emit_exe_file(&obj_path, &option.output_file_path, &option.additional_libs)?;
    }

    Ok(())
}

fn create_new_project(project_name: String) -> anyhow::Result<()> {
    use std::fs;

    // Step 1: Create new Rust library project
    let status = Command::new("cargo")
        .args(["new", "--lib", &project_name])
        .status()
        .context("Failed to run 'cargo new --lib'")?;

    if !status.success() {
        anyhow::bail!("Failed to create new Rust library project");
    }

    let project_dir = Path::new(&project_name);
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

fn build_project() -> anyhow::Result<()> {
    use std::fs;

    // Check if we're in a Kaede Rust bridge project
    if !Path::new("rust").exists() || !Path::new("src").exists() {
        anyhow::bail!(
            "This is not a Kaede Rust bridge project.\n\
             Expected structure:\n\
             ├── rust/          (Rust code and Cargo.toml)\n\
             └── src/           (Kaede source files)\n\
             \n\
             To create a new project: kaede new <project_name>\n\
             Current directory: {}",
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("unknown"))
                .display()
        );
    }

    // Check if rust/Cargo.toml exists
    if !Path::new("rust/Cargo.toml").exists() {
        anyhow::bail!(
            "rust/Cargo.toml not found. Make sure you're in a Kaede Rust bridge project directory.\n\
             Current directory: {}", 
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from("unknown")).display()
        );
    }

    // Step 1: Build Rust library
    println!("🔨 Building Rust library...");
    let status = Command::new("cargo")
        .args(["build"])
        .current_dir("rust")
        .status()
        .context("Failed to run 'cargo build'")?;

    if !status.success() {
        anyhow::bail!("Failed to build Rust library");
    }

    // Step 2: Get package name from Cargo.toml
    let cargo_toml_content =
        fs::read_to_string("rust/Cargo.toml").context("Failed to read rust/Cargo.toml")?;

    let package_name = cargo_toml_content
        .lines()
        .find(|line| line.starts_with("name = "))
        .and_then(|line| line.split('"').nth(1))
        .ok_or_else(|| anyhow!("Could not find package name in rust/Cargo.toml"))?;

    // Step 3: Find all .kd files in kaede directory
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

    let file_paths = find_kd_files(&PathBuf::from("src"))?;

    if file_paths.is_empty() {
        anyhow::bail!("No .kd files found in kaede directory");
    }

    // Step 4: Create build directory
    fs::create_dir_all("build").context("Failed to create build directory")?;

    // Step 5: Prepare unit infos for compilation
    println!("🔨 Compiling Kaede files...");
    let mut unit_infos = Vec::new();
    for file_path in &file_paths {
        unit_infos.push(CompileUnitInfo {
            program: fs::read_to_string(file_path)
                .with_context(|| format!("Failed to read file: {}", file_path.display()))?,
            file_path: file_path.clone(),
        });
    }

    // Step 6: Set up Rust library path
    let rust_lib_path = format!("rust/target/debug/lib{}.so", package_name.replace('-', "_"));
    if !Path::new(&rust_lib_path).exists() {
        anyhow::bail!(
            "Rust library not found at: {}. Make sure cargo build succeeded.",
            rust_lib_path
        );
    }

    // Step 7: Create compile option with Rust library
    let option = CompileOption {
        opt_level: OptimizationLevel::Default,
        display_llvm_ir: false,
        output_file_path: PathBuf::from("build/main"),
        root_dir: Some(PathBuf::from("src")),
        no_autoload: false,
        no_prelude: false,
        no_gc: false,
        additional_libs: vec![PathBuf::from(rust_lib_path)],
    };

    // Step 8: Use existing compile_and_link function
    println!("🔗 Linking with Rust library...");
    compile_and_link(unit_infos, option)?;

    println!("✅ Build completed successfully!");
    println!("📁 Output: build/main");
    println!("🚀 Run with: ./build/main");

    Ok(())
}

struct CompileOption {
    opt_level: OptimizationLevel,
    display_llvm_ir: bool,
    output_file_path: PathBuf,
    root_dir: Option<PathBuf>,
    no_autoload: bool,
    no_prelude: bool,
    no_gc: bool,
    additional_libs: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    use clap::Parser;

    let args = Args::parse();

    // Handle subcommands first
    if let Some(command) = args.command {
        match command {
            Commands::New { project_name } => {
                create_new_project(project_name)?;
                return Ok(());
            }
            Commands::Build => {
                build_project()?;
                return Ok(());
            }
        }
    }

    let file_paths = args.files;

    let option = CompileOption {
        opt_level: to_inkwell_opt_level(args.opt_level),
        display_llvm_ir: args.display_llvm_ir,
        output_file_path: args.output.unwrap_or(PathBuf::from(if args.c {
            "a.o"
        } else {
            "a.out"
        })),
        root_dir: args.root_dir,
        no_autoload: args.no_autoload,
        no_prelude: args.no_prelude,
        no_gc: args.no_gc,
        additional_libs: Vec::new(),
    };

    if let Some(program) = args.program {
        compile_and_link(
            vec![CompileUnitInfo {
                file_path: PathBuf::from("<commandline>"),
                program,
            }],
            option,
        )?;

        return Ok(());
    }

    if file_paths.is_empty() {
        return Err(anyhow!("No input files"));
    }

    // --- Compile ---

    let mut programs = Vec::new();

    for file_path in file_paths {
        programs.push(CompileUnitInfo {
            program: fs::read_to_string(&file_path)
                .with_context(|| format!("Failed to open file: {}", file_path.to_string_lossy()))?,
            file_path,
        });
    }

    if args.c {
        // Emit object files
        if let Err(err) = compile_and_output_obj(programs, option) {
            // If backtrace is enabled, it is also displayed
            eprintln!("{}: {:?}", "Error".bright_red(), err);
            std::process::exit(1);
        }
    } else {
        // Emit exe files
        if let Err(err) = compile_and_link(programs, option) {
            // If backtrace is enabled, it is also displayed
            eprintln!("{}: {:?}", "Error".bright_red(), err);
            std::process::exit(1);
        }
    }

    Ok(())
}
