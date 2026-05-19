use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::{expr::ExprKind, top::TopLevelKind, ModuleItem};
use kaede_parse::Parser;
use kaede_span::file::FilePath;
use kaede_symbol::Symbol;

fn file() -> FilePath {
    FilePath::from(PathBuf::from("test.kd"))
}

#[test]
fn parse_multiline_call_arguments_without_trailing_comma() -> Result<()> {
    let mut parser = Parser::new(
        r#"
foo(
    1,
    bar = 2
)
"#,
        file(),
    );
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    let args: Vec<_> = call.args.args.iter().collect();
    assert_eq!(args.len(), 2);
    assert!(args[0].name.is_none());
    assert_eq!(
        args[1].name.as_ref().expect("keyword argument").symbol(),
        Symbol::from("bar".to_string())
    );

    Ok(())
}

#[test]
fn parse_multiline_call_arguments_with_trailing_comma() -> Result<()> {
    let mut parser = Parser::new(
        r#"
foo(
    1,
    bar = 2,
)
"#,
        file(),
    );
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    assert_eq!(call.args.args.len(), 2);

    Ok(())
}

#[test]
fn parse_multiline_fn_params_without_trailing_comma() -> Result<()> {
    let mut parser = Parser::new(
        r#"
fun f(
    a: i32,
    b: i32 = 2
) {}
"#,
        file(),
    );
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top_level) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };
    let TopLevelKind::Fn(func) = &top_level.kind else {
        panic!("expected function top-level");
    };

    assert_eq!(func.decl.params.v.len(), 2);

    Ok(())
}

#[test]
fn parse_multiline_fn_params_and_generics_with_trailing_commas() -> Result<()> {
    let mut parser = Parser::new(
        r#"
fun f<
    T,
    U,
>(
    value: Result<
        T,
        U,
    >,
) -> Result<
    U,
    T,
> {}
"#,
        file(),
    );
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top_level) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };
    let TopLevelKind::Fn(func) = &top_level.kind else {
        panic!("expected function top-level");
    };

    assert_eq!(
        func.decl
            .generic_params
            .as_ref()
            .expect("generic params")
            .params
            .len(),
        2
    );
    assert_eq!(func.decl.params.v.len(), 1);

    Ok(())
}
