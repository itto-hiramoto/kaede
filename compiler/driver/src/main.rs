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
use kaede_common::{kaede_gc_lib_path, kaede_lib_path};
use kaede_parse::Parser;
use kaede_semantic::SemanticAnalyzer;
use tempfile::{NamedTempFile, TempPath};

mod build;
mod new;

use build::build_project;
use new::create_new_project;

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
    /// Create a new Kaede project (optionally with Rust bridge)
    New {
        /// Name of the project to create
        project_name: String,
        #[arg(long, action, help = "Generate Rust bridge scaffolding")]
        rust: bool,
    },
    /// Build a Kaede Rust bridge project
    Build,
    /// Run Kaede Language Server
    Lsp {
        #[arg(long, action, hide = true)]
        stdio: bool,
    },
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

pub(crate) struct CompileUnitInfo {
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

pub(crate) fn compile_and_output_obj(
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

pub(crate) fn compile_and_link(
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

pub(crate) struct CompileOption {
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
            Commands::New { project_name, rust } => {
                create_new_project(project_name, rust)?;
                return Ok(());
            }
            Commands::Build => {
                build_project()?;
                return Ok(());
            }
            Commands::Lsp { stdio: _ } => {
                let runtime = tokio::runtime::Builder::new_multi_thread()
                    .enable_all()
                    .build()?;
                runtime.block_on(kaede_lsp::run_stdio());
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
