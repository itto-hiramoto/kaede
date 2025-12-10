mod common;

use common::semantic_analyze;

use crate::common::semantic_analyze_expect_error;
use kaede_parse::Parser;
use kaede_span::file::FilePath;
use std::fs;
use tempfile::tempdir;

#[test]
fn empty_function() -> anyhow::Result<()> {
    semantic_analyze("fn foo() {}")?;
    Ok(())
}

#[test]
fn empty_struct() -> anyhow::Result<()> {
    semantic_analyze("struct Foo {}")?;
    Ok(())
}

#[test]
fn empty_enum() -> anyhow::Result<()> {
    semantic_analyze("enum Foo {}")?;
    Ok(())
}

#[test]
fn function_with_return() -> anyhow::Result<()> {
    semantic_analyze("fn foo(): i32 { return 1 }")?;
    Ok(())
}

#[test]
fn simple_struct() -> anyhow::Result<()> {
    semantic_analyze("struct Foo { a: i32, b: i32 }")?;
    Ok(())
}

#[test]
fn simple_enum() -> anyhow::Result<()> {
    semantic_analyze("enum Foo { A, B }")?;
    Ok(())
}

#[test]
fn function_with_params() -> anyhow::Result<()> {
    semantic_analyze("fn foo(a: i32, b: i32): i32 { return a + b }")?;
    Ok(())
}

#[test]
fn function_with_generic_params() -> anyhow::Result<()> {
    semantic_analyze(
        "fn foo<T, U>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<i32, i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fn foo<T>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fn foo<T, U>(a: T, b: U): T {
        return a + b
    }
    fn f() {
        foo<123>(1, 2)
    }",
    )?;

    Ok(())
}

#[test]
fn generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        fn f() {
            let x = Foo<i32> { a: 1 }
        }
    ",
    )?;
    semantic_analyze(
        "enum Foo<T> { A, B(T) }
        fn f() {
            let x = Foo<i32>::B(1)
        }
    ",
    )?;
    semantic_analyze(
        "fn foo<T>(a: T): T { return a }
        fn f() {
            foo<i32>(1)
        }
    ",
    )?;
    semantic_analyze(
        "fn foo<T>(a: T): T { return a }
        fn f() {
            foo<i32>(1)
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_for_generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        impl<T> Foo<T> {
            fn f(self): T {
                return self.a
            }
        }
        fn f() {
            let foo = Foo<i32> { a: 1 }
            let x = foo.f()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        impl Foo {
            fn f(self): i32 {
                return self.a
            }
        }
        fn f() {
            let foo = Foo { a: 1 }
            let x = foo.f()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_with_static_method() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        impl Foo {
            fn new(n: i32): Foo {
                return Foo { a: n }
            }
            fn get_a(self): i32 {
                return self.a
            }
        }
        fn f(): i32 {
            let foo = Foo::new(1)
            return foo.get_a()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn impl_with_static_method_for_generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        impl<T> Foo<T> {
            fn new(n: T): Foo<T> {
                return Foo<T> { a: n }
            }
            fn get_a(self): T {
                return self.a
            }
        }
        fn f(): i32 {
            let foo = Foo<i32>::new(1)
            return foo.get_a()
        }
    ",
    )?;
    Ok(())
}

#[test]
fn extern_() -> anyhow::Result<()> {
    semantic_analyze(
        r#"extern "C" fn foo(): i32
        fn f() {
            foo()
        }
    "#,
    )?;
    Ok(())
}

#[test]
fn top_level_statements_generate_main() -> anyhow::Result<()> {
    semantic_analyze(
        "
        let x: i32 = 1
        let y: i32 = x + 1
        ",
    )?;
    Ok(())
}

#[test]
fn top_level_statements_and_main_conflict() -> anyhow::Result<()> {
    let err = semantic_analyze_expect_error(
        "
        fn main(): i32 { return 1 }
        let x: i32 = 2
        ",
    )?;

    assert!(err
        .to_string()
        .contains("top-level statements cannot be used"));

    Ok(())
}

#[test]
fn top_level_statements_disallowed_in_non_entry_module() -> anyhow::Result<()> {
    let dir = tempdir()?;
    let file_path = dir.path().join("module.kd");
    fs::write(&file_path, "let x: i32 = 1")?;
    let file = FilePath::from(file_path.clone());
    let ast = Parser::new("let x: i32 = 1", file.clone()).run()?;
    let mut analyzer = kaede_semantic::SemanticAnalyzer::new(file, dir.path().to_path_buf(), false);

    let err = analyzer.analyze(ast, false, false).unwrap_err();
    assert!(err
        .to_string()
        .contains("top-level statements are only allowed in the entry module"));

    Ok(())
}
