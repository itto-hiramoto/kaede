use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::{top::TopLevelKind, ModuleItem};
use kaede_ast_type::Mutability;
use kaede_parse::Parser;
use kaede_span::file::FilePath;
use kaede_symbol::Symbol;

fn file() -> FilePath {
    FilePath::from(PathBuf::from("test.kd"))
}

#[test]
fn parse_interface_with_methods() -> Result<()> {
    let src = "\
export interface Reader {
    fun read(mut self, buf: i32) -> i32
    fun peek(self) -> i32
}
";
    let mut parser = Parser::new(src, file());
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Interface(interface) = &top.kind else {
        panic!("expected interface top-level");
    };

    assert!(interface.vis.is_public());
    assert_eq!(interface.name.symbol(), Symbol::from("Reader".to_string()));
    assert_eq!(interface.methods.len(), 2);

    let read = &interface.methods[0];
    assert_eq!(read.name.symbol(), Symbol::from("read".to_string()));
    assert_eq!(read.self_, Some(Mutability::Mut));
    assert_eq!(read.params.v.len(), 1);

    let peek = &interface.methods[1];
    assert_eq!(peek.name.symbol(), Symbol::from("peek".to_string()));
    assert_eq!(peek.self_, Some(Mutability::Not));
    assert_eq!(peek.params.v.len(), 0);

    Ok(())
}

#[test]
fn parse_interface_without_export() -> Result<()> {
    let src = "\
interface Stringer {
    fun to_string(self) -> i32
}
";
    let mut parser = Parser::new(src, file());
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Interface(interface) = &top.kind else {
        panic!("expected interface top-level");
    };

    assert!(interface.vis.is_private());
    assert_eq!(interface.methods.len(), 1);

    Ok(())
}

#[test]
fn parse_empty_interface() -> Result<()> {
    let src = "interface Empty {}\n";
    let mut parser = Parser::new(src, file());
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Interface(interface) = &top.kind else {
        panic!("expected interface top-level");
    };

    assert_eq!(interface.methods.len(), 0);
    Ok(())
}

#[test]
fn parse_generic_bound() -> Result<()> {
    let src = "\
fun drain<T: Reader>(r: T) {}
";
    let mut parser = Parser::new(src, file());
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Fn(func) = &top.kind else {
        panic!("expected function top-level");
    };

    let gp = func.decl.generic_params.as_ref().expect("generic params");
    assert_eq!(gp.params.len(), 1);
    let bound = gp.params[0].bound.as_ref().expect("bound");
    assert_eq!(bound.symbol(), Symbol::from("Reader".to_string()));

    Ok(())
}

#[test]
fn parse_generic_without_bound() -> Result<()> {
    let src = "fun id<T>(x: T) -> T { return x }\n";
    let mut parser = Parser::new(src, file());
    let compile_unit = parser.run()?;

    let ModuleItem::Decl(top) = compile_unit.items.front().unwrap() else {
        panic!("expected top-level declaration");
    };

    let TopLevelKind::Fn(func) = &top.kind else {
        panic!("expected function top-level");
    };

    let gp = func.decl.generic_params.as_ref().expect("generic params");
    assert_eq!(gp.params.len(), 1);
    assert!(gp.params[0].bound.is_none());

    Ok(())
}
