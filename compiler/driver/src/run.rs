use std::process::Command;

use crate::build::build_project;

pub(crate) fn run_project(args: Vec<String>) -> anyhow::Result<()> {
    build_project()?;

    let status = Command::new("build/main").args(args).status()?;

    if let Some(code) = status.code() {
        std::process::exit(code);
    }

    anyhow::bail!("Process terminated by signal")
}
