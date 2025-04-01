use std::path::PathBuf;

use kaede_ir as ir;
use kaede_parse::Parser;
use kaede_semantic::SemanticAnalyzer;
use kaede_span::file::FilePath;

pub fn semantic_analyze(program: &str) -> anyhow::Result<ir::CompileUnit> {
    let ast = Parser::new(program, FilePath::from(PathBuf::from("test.kd"))).run()?;

    let mut analyzer = SemanticAnalyzer::new(FilePath::from(PathBuf::from("test.kd")));
    let result = analyzer.analyze(ast)?;

    // If you want to see the result, you can put --nocapture to the test command.
    eprintln!("{:?}", result);

    Ok(result)
}
