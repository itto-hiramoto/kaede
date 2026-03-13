use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::{expr::ExprKind, top::TopLevelKind, ModuleItem};
use kaede_ast_type::TyKind;
use kaede_parse::{ParseError, Parser};
use kaede_span::file::FilePath;
use kaede_symbol::Symbol;

#[test]
fn parse_keyword_argument() -> Result<()> {
    let mut parser = Parser::new("foo(bar = 1)", FilePath::from(PathBuf::from("test.kd")));
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
    let mut parser = Parser::new("foo(1, bar = 2)", FilePath::from(PathBuf::from("test.kd")));
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

#[test]
fn parse_fn_param_default() -> Result<()> {
    let mut parser = Parser::new(
        "fn f(a: i32 = 1, b: i32) {}",
        FilePath::from(PathBuf::from("test.kd")),
    );
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top_level) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Fn(func) = &top_level.kind else {
        panic!("expected fn top-level");
    };

    let params: Vec<_> = func.decl.params.v.iter().collect();
    assert!(params[0].default.is_some());
    assert!(params[1].default.is_none());
    assert!(matches!(
        params[0].default.as_ref().unwrap().as_ref().kind,
        ExprKind::Int(_)
    ));

    Ok(())
}

#[test]
fn parse_interpolated_string_lowers_to_format_call() -> Result<()> {
    let mut parser = Parser::new(
        r#"$"hello {name}""#,
        FilePath::from(PathBuf::from("test.kd")),
    );
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    let callee = match call.callee.kind {
        ExprKind::Ident(id) => id,
        other => panic!("expected ident callee, got {other:?}"),
    };
    assert_eq!(callee.symbol(), Symbol::from("__format".to_string()));

    assert_eq!(call.args.args.len(), 2);
    let args: Vec<_> = call.args.args.into_iter().collect();
    match &args[0].value.kind {
        ExprKind::StringLiteral(s) => assert_eq!(s.syb, Symbol::from("hello {}".to_string())),
        other => panic!("expected template string literal, got {other:?}"),
    }
    assert!(matches!(args[1].value.kind, ExprKind::Ident(_)));

    Ok(())
}

#[test]
fn parse_interpolated_string_escaped_braces() -> Result<()> {
    let mut parser = Parser::new(
        r#"$"{{x}} {name}""#,
        FilePath::from(PathBuf::from("test.kd")),
    );
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };
    let first = call.args.args.front().expect("template arg");
    match &first.value.kind {
        ExprKind::StringLiteral(s) => assert_eq!(s.syb, Symbol::from("{x} {}".to_string())),
        other => panic!("expected string literal, got {other:?}"),
    }

    Ok(())
}

#[test]
fn parse_match_arms_without_commas() -> Result<()> {
    let mut parser = Parser::new(
        "match x {\n  1 => 10\n  2 => 20\n}",
        FilePath::from(PathBuf::from("test.kd")),
    );
    let expr = parser.expr()?;

    let m = match expr.kind {
        ExprKind::Match(m) => m,
        other => panic!("expected match expr, got {other:?}"),
    };

    assert_eq!(m.arms.len(), 2);
    Ok(())
}

#[test]
fn parse_hex_integer_literal() -> Result<()> {
    let mut parser = Parser::new("0x80", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let int = match expr.kind {
        ExprKind::Int(int) => int,
        other => panic!("expected int literal, got {other:?}"),
    };

    assert_eq!(int.as_u64(), 0x80);

    Ok(())
}

#[test]
fn parse_hex_array_size() -> Result<()> {
    let mut parser = Parser::new(
        "fn f(values: [i32; 0x10]) {}",
        FilePath::from(PathBuf::from("test.kd")),
    );
    let unit = parser.run()?;

    let ModuleItem::Decl(top_level) = unit.items.front().expect("top level") else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Fn(func) = &top_level.kind else {
        panic!("expected fn top-level");
    };

    let param = func.decl.params.v.front().expect("function parameter");
    match param.ty.kind.as_ref() {
        TyKind::Reference(reference) => match reference.refee_ty.kind.as_ref() {
            TyKind::Array((_, size)) => assert_eq!(*size, 0x10),
            other => panic!("expected array type, got {other:?}"),
        },
        other => panic!("expected reference type, got {other:?}"),
    }

    Ok(())
}

#[test]
fn parse_foreign_rust_import() -> Result<()> {
    let mut parser = Parser::new(
        "import rust::example_crate",
        FilePath::from(PathBuf::from("test.kd")),
    );
    let unit = parser.run()?;
    let ModuleItem::Decl(top) = unit.items.front().expect("top level") else {
        panic!("expected declaration");
    };

    let TopLevelKind::Import(import) = &top.kind else {
        panic!("expected import top-level");
    };

    match &import.kind {
        kaede_ast::top::ImportKind::Foreign { lang, crate_name } => {
            assert_eq!(lang.as_str(), "rust");
            assert_eq!(crate_name.as_str(), "example_crate");
        }
        other => panic!("expected foreign import, got {other:?}"),
    }

    Ok(())
}

#[test]
fn parse_mixed_module_items_keeps_source_order() -> Result<()> {
    let mut parser = Parser::new(
        "fn helper() {}\nlet x = 1\nx",
        FilePath::from(PathBuf::from("test.kd")),
    );
    let unit = parser.run()?;

    assert!(matches!(unit.items[0], ModuleItem::Decl(_)));
    assert!(matches!(unit.items[1], ModuleItem::Stmt(_)));
    assert!(matches!(unit.items[2], ModuleItem::Stmt(_)));

    Ok(())
}

#[test]
fn parse_bridge_is_plain_parse_error() -> Result<()> {
    let mut parser = Parser::new(
        r#"pub bridge "Rust" fn hello()"#,
        FilePath::from(PathBuf::from("test.kd")),
    );

    let err = parser.run().expect_err("bridge syntax must fail");
    let parse_err = err.downcast::<ParseError>().expect("expected parse error");

    assert!(matches!(parse_err, ParseError::ExpectedError { .. }));

    Ok(())
}

#[test]
fn parse_pub_let_is_rejected() -> Result<()> {
    let mut parser = Parser::new("pub let x = 1", FilePath::from(PathBuf::from("test.kd")));

    let err = parser.run().expect_err("pub let must fail");
    let parse_err = err.downcast::<ParseError>().expect("expected parse error");

    assert!(matches!(parse_err, ParseError::ExpectedError { .. }));

    Ok(())
}

#[test]
fn parse_impl_body_rejects_statements() -> Result<()> {
    let mut parser = Parser::new(
        "struct Foo {}\nimpl Foo { let x = 1 }",
        FilePath::from(PathBuf::from("test.kd")),
    );

    let err = parser.run().expect_err("impl statements must fail");
    let parse_err = err.downcast::<ParseError>().expect("expected parse error");

    assert!(matches!(parse_err, ParseError::ExpectedError { .. }));

    Ok(())
}
