use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::expr::ExprKind;
use kaede_parse::Parser;
use kaede_span::file::FilePath;
use kaede_symbol::Symbol;

#[test]
fn parse_keyword_argument() -> Result<()> {
    let mut parser = Parser::new("foo(bar: 1)", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    let arg = call.args.args.front().expect("argument");
    let name = arg.name.as_ref().expect("keyword argument name");
    assert_eq!(name.symbol(), Symbol::from("bar".to_string()));
    assert!(matches!(arg.value.kind, ExprKind::Int(_)));

    Ok(())
}

#[test]
fn parse_positional_then_keyword_arguments() -> Result<()> {
    let mut parser = Parser::new("foo(1, bar: 2)", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    let mut iter = call.args.args.iter();
    let first = iter.next().expect("first argument");
    assert!(first.name.is_none());

    let second = iter.next().expect("second argument");
    let name = second.name.as_ref().expect("keyword argument name");
    assert_eq!(name.symbol(), Symbol::from("bar".to_string()));

    Ok(())
}
