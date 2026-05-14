use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::expr::{ExprKind, SelectBinding, SelectOp};
use kaede_parse::Parser;
use kaede_span::file::FilePath;

fn parse(source: &str) -> Result<kaede_ast::expr::Expr> {
    let mut parser = Parser::new(source, FilePath::from(PathBuf::from("test.kd")));
    parser.expr()
}

#[test]
fn parses_single_recv_binding() -> Result<()> {
    let expr = parse("select { case value = ch.recv() => 1 }")?;
    let select = match expr.kind {
        ExprKind::Select(s) => s,
        other => panic!("expected select, got {other:?}"),
    };
    assert_eq!(select.arms.len(), 1);
    assert!(select.default.is_none());
    match &select.arms[0].op {
        SelectOp::Recv { binding, channel } => {
            match binding {
                SelectBinding::Named(ident) => assert_eq!(ident.symbol().as_str(), "value"),
                other => panic!("expected named binding, got {other:?}"),
            }
            assert!(matches!(channel.kind, ExprKind::Ident(_)));
        }
        other => panic!("expected recv op, got {other:?}"),
    }
    Ok(())
}

#[test]
fn parses_wildcard_recv() -> Result<()> {
    let expr = parse("select { case _ = ch.recv() => 1 }")?;
    let select = match expr.kind {
        ExprKind::Select(s) => s,
        other => panic!("expected select, got {other:?}"),
    };
    match &select.arms[0].op {
        SelectOp::Recv { binding, .. } => {
            assert!(matches!(binding, SelectBinding::Wildcard));
        }
        other => panic!("expected recv op, got {other:?}"),
    }
    Ok(())
}

#[test]
fn parses_send_arm() -> Result<()> {
    let expr = parse("select { case ch.send(42) => 1 }")?;
    let select = match expr.kind {
        ExprKind::Select(s) => s,
        other => panic!("expected select, got {other:?}"),
    };
    match &select.arms[0].op {
        SelectOp::Send { channel, value } => {
            assert!(matches!(channel.kind, ExprKind::Ident(_)));
            assert!(matches!(value.kind, ExprKind::Int(_)));
        }
        other => panic!("expected send op, got {other:?}"),
    }
    Ok(())
}

#[test]
fn parses_default_arm() -> Result<()> {
    let expr = parse("select { default => 7 }")?;
    let select = match expr.kind {
        ExprKind::Select(s) => s,
        other => panic!("expected select, got {other:?}"),
    };
    assert_eq!(select.arms.len(), 0);
    assert!(select.default.is_some());
    Ok(())
}

#[test]
fn parses_mixed_arms_with_default() -> Result<()> {
    let expr = parse("select { case v = ch.recv() => 1, case ch.send(2) => 3, default => 4 }")?;
    let select = match expr.kind {
        ExprKind::Select(s) => s,
        other => panic!("expected select, got {other:?}"),
    };
    assert_eq!(select.arms.len(), 2);
    assert!(select.default.is_some());
    assert!(matches!(select.arms[0].op, SelectOp::Recv { .. }));
    assert!(matches!(select.arms[1].op, SelectOp::Send { .. }));
    Ok(())
}

#[test]
fn rejects_empty_select() {
    let result = parse("select {}");
    assert!(result.is_err(), "expected empty select to fail to parse");
}

#[test]
fn rejects_non_method_case() {
    let result = parse("select { case 1 + 2 => 0 }");
    assert!(result.is_err(), "expected non-method case to fail to parse");
}
