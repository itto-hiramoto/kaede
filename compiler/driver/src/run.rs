use std::process::Command;

use crate::build::build_project;

pub(crate) fn run_project(args: Vec<String>) -> anyhow::Result<()> {
    let binary = build_project()?;

    let status = Command::new(&binary).args(args).status()?;

    if let Some(code) = status.code() {
        std::process::exit(code);
    }

    anyhow::bail!("Process terminated by signal")
}
