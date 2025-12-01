use std::{fmt::Display, path::PathBuf};

use kaede_ir as ir;
use kaede_parse::Parser;
use kaede_semantic::{SemanticAnalyzer, SemanticError};
use kaede_span::file::FilePath;
use kaede_type_infer::TypeInferError;

fn semantic_analyze_internal(program: &str) -> anyhow::Result<ir::CompileUnit> {
    let ast = Parser::new(program, FilePath::from(PathBuf::from("test.kd"))).run()?;
    let mut analyzer = SemanticAnalyzer::new_for_single_file_test();
    analyzer.analyze(ast, false, false)
}

#[allow(dead_code)]
pub fn semantic_analyze(program: &str) -> anyhow::Result<ir::CompileUnit> {
    let result = semantic_analyze_internal(program)?;

    // If you want to see the result, you can put --nocapture to the test command.
    eprintln!("{result:?}");

    Ok(result)
}

fn to_display<T: Display + 'static>(err: T) -> Box<dyn Display> {
    Box::new(err)
}

#[allow(dead_code)]
pub fn semantic_analyze_expect_error(program: &str) -> anyhow::Result<Box<dyn Display>> {
    let result = semantic_analyze_internal(program).expect_err("Expected error");

    // If you want to see the result, you can put --nocapture to the test command.
    eprintln!("{result:?}");

    // Try to downcast to SemanticError first, then TypeInferError
    match result.downcast::<SemanticError>() {
        Ok(err) => Ok(to_display(err)),
        Err(result) => match result.downcast::<TypeInferError>() {
            Ok(err) => Ok(to_display(err)),
            Err(result) => panic!("Expected SemanticError or TypeInferError, but got: {result:?}"),
        },
    }
}
