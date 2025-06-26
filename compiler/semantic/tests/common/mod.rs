use std::path::PathBuf;

use kaede_ir as ir;
use kaede_parse::Parser;
use kaede_semantic::{SemanticAnalyzer, SemanticError};
use kaede_span::file::FilePath;

fn semantic_analyze_internal(program: &str) -> anyhow::Result<ir::CompileUnit> {
    let ast = Parser::new(program, FilePath::from(PathBuf::from("test.kd"))).run()?;
    let mut analyzer = SemanticAnalyzer::new_for_single_file_test();
    analyzer.analyze(ast)
}

#[allow(dead_code)]
pub fn semantic_analyze(program: &str) -> anyhow::Result<ir::CompileUnit> {
    let result = semantic_analyze_internal(program)?;

    // If you want to see the result, you can put --nocapture to the test command.
    eprintln!("{:?}", result);

    Ok(result)
}

#[allow(dead_code)]
pub fn semantic_analyze_expect_error(program: &str) -> anyhow::Result<SemanticError> {
    let result = semantic_analyze_internal(program).expect_err("Expected error");

    // If you want to see the result, you can put --nocapture to the test command.
    eprintln!("{:?}", result);

    Ok(result.downcast::<SemanticError>().unwrap())
}
