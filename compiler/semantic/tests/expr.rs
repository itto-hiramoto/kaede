mod common;

use std::collections::HashSet;

use common::{semantic_analyze, semantic_analyze_expect_error};
use kaede_ir::{expr::ExprKind as IrExprKind, stmt::Stmt, top::TopLevel, ty::TyKind as IrTyKind};
use kaede_symbol::Symbol;

#[test]
fn int() -> anyhow::Result<()> {
    semantic_analyze("fn f() { 1 }")?;
    Ok(())
}

#[test]
fn array_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { [1, 2, 3] }")?;
    Ok(())
}

#[test]
fn boolean_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { true }")?;
    semantic_analyze("fn f() { false }")?;
    Ok(())
}

#[test]
fn block() -> anyhow::Result<()> {
    semantic_analyze("fn f() { { 1 } }")?;
    Ok(())
}

#[test]
fn string_literal() -> anyhow::Result<()> {
    semantic_analyze("fn f() { \"hello\" }")?;
    Ok(())
}

#[test]
fn arithmetic_binary() -> anyhow::Result<()> {
    semantic_analyze("fn f() { 1 + 2 }")?;
    semantic_analyze("fn f() { 1 - 2 }")?;
    semantic_analyze("fn f() { 1 * 2 }")?;
    semantic_analyze("fn f() { 1 / 2 }")?;
    semantic_analyze("fn f() { 1 % 2 }")?;
    semantic_analyze("fn f() { 1 < 2 }")?;
    semantic_analyze("fn f() { 1 <= 2 }")?;
    semantic_analyze("fn f() { 1 > 2 }")?;
    semantic_analyze("fn f() { 1 >= 2 }")?;
    semantic_analyze("fn f() { 1 == 2 }")?;
    semantic_analyze("fn f() { 1 != 2 }")?;
    Ok(())
}

#[test]
fn logical_binary() -> anyhow::Result<()> {
    semantic_analyze("fn f() { true && false }")?;
    semantic_analyze("fn f() { true || false }")?;
    Ok(())
}

#[test]
fn logical_not() -> anyhow::Result<()> {
    semantic_analyze("fn f() { !true }")?;
    Ok(())
}

#[test]
fn array_indexing() -> anyhow::Result<()> {
    semantic_analyze("fn f() { [0, 1, 2][1] }")?;
    Ok(())
}

#[test]
fn slice_type_annotation() -> anyhow::Result<()> {
    semantic_analyze(
        "fn len(s: [i32]) {
            let _ = s[0]
        }",
    )?;
    Ok(())
}

#[test]
fn array_to_slice_param() -> anyhow::Result<()> {
    semantic_analyze(
        "fn take(s: [i32]) { let _ = s[1]; }
        fn main() {
            take([1, 2, 3])
        }",
    )?;
    Ok(())
}

#[test]
fn array_to_slice_return() -> anyhow::Result<()> {
    semantic_analyze(
        "fn make(): [i32] { [10, 20, 30] }
        fn use_slice() {
            let s = make()
            let _ = s[2]
        }",
    )?;
    Ok(())
}

#[test]
fn array_to_slice_var() -> anyhow::Result<()> {
    semantic_analyze(
        "fn take(s: [i32]): i32 { return s[1] }

        fn f(): i32 {
            let a: [i32; 3] = [1, 2, 3]
            return take(a)
        }",
    )?;
    Ok(())
}

#[test]
fn function_call() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(a: i32, b: i32): i32 { return a + b }\n
        fn g(): i32 { return f(48, 10) }
    ",
    )?;
    Ok(())
}

#[test]
fn if_() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(a: i32): i32 {
            return if a == 0 {
                1
            } else if a > 0 {
                -1
            } else {
                0
            }
        }",
    )?;

    // Test that `a < 0` is correctly parsed as a comparison, not generic syntax
    semantic_analyze(
        "fn f(a: i32): i32 {
            return if a == 0 {
                1
            } else if a < 0 {
                -1
            } else {
                0
            }
        }",
    )?;

    Ok(())
}

#[test]
fn tuple_literal() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(): (i32, bool) {
            return (42, true)
        }",
    )?;
    Ok(())
}

#[test]
fn tuple_indexing() -> anyhow::Result<()> {
    semantic_analyze(
        r#"fn f(): i32 {
            let t = (42, true, "hello, world")
            return t.0
        }"#,
    )?;
    Ok(())
}

#[test]
fn struct_literal() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32, b: bool }
        fn f(): Foo {
            return Foo { a: 42, b: true }
        }",
    )?;
    Ok(())
}

#[test]
fn struct_access() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo { a: i32 }
        fn f(): i32 {
            let a = Foo { a: 58 }
            return a.a
        }",
    )?;
    Ok(())
}

#[test]
fn loop_and_break() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f() {
            loop {
                break
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_bool() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: bool): i32 {
            return match x {
                true => 1,
                false => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_int() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                1 => 2,
                _ => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_on_enum() -> anyhow::Result<()> {
    semantic_analyze(
        "enum Foo { A, B, C }
        fn f(x: Foo): i32 {
            return match x {
                Foo::A => 1,
                Foo::B => 2,
                Foo::C => 3,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_with_catch_all() -> anyhow::Result<()> {
    semantic_analyze(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                _ => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_enum() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B, C }
        fn f(x: Foo): i32 {
            return match x {
                Foo::A => 1,
                Foo::B => 2,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_int() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(x: i32): i32 {
            return match x {
                0 => 1,
                1 => 0,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_not_exhaustive_bool() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(x: bool): i32 {
            return match x {
                true => 1,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn enum_variant() -> anyhow::Result<()> {
    semantic_analyze(
        "enum Foo { A, B(i32), C }
        fn f(): i32 {
            let x = Foo::B(1)
            return match x {
                Foo::A => 1,
                Foo::B(y) => y,
                Foo::C => 3,
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_only_int_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(): i32 {
            let x = 42
            return match x {
                _ => 1
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_only_enum_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B }
        fn f(): i32 {
            let x = Foo::A
            return match x {
                _ => 1
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_undefined_function_int_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "fn f(): i32 {
            let x = 42
            return match x {
                1 => 10,
                2 => 20,
                _ => g()  // g() is undefined
            }
        }",
    )?;
    Ok(())
}

#[test]
fn match_catch_all_undefined_function_enum_error() -> anyhow::Result<()> {
    semantic_analyze_expect_error(
        "enum Foo { A, B }
        fn f(): i32 {
            let x = Foo::A
            return match x {
                Foo::A => 10,
                _ => g()  // g() is undefined
            }
        }",
    )?;
    Ok(())
}

fn find_function_body(ir: kaede_ir::CompileUnit, name: &str) -> kaede_ir::stmt::Block {
    for top in ir.top_levels {
        if let TopLevel::Fn(f) = top {
            if f.decl.name.symbol().as_str() == name {
                return f.body.clone().expect("expected function body");
            }
        }
    }
    panic!("no function found");
}

#[test]
fn closure_captures_outer_variables() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        "
        fn f(): i32 {
            let a: i32 = 10
            let b: i32 = 2
            let c = | | a + b
            return 0
        }
    ",
    )?;

    let body = find_function_body(ir, "f");

    let closure_expr = match &body.body[2] {
        Stmt::Let(let_stmt) => let_stmt.init.as_ref().unwrap(),
        _ => panic!("expected let binding for closure"),
    };

    let (captures, param_len, capture_tys_len) = match &closure_expr.kind {
        IrExprKind::Closure(closure) => {
            let captured: HashSet<Symbol> = closure
                .captures
                .iter()
                .map(|c| match &c.kind {
                    IrExprKind::Variable(v) => v.name,
                    _ => panic!("unexpected capture kind"),
                })
                .collect();

            let captured_tys_len = match closure_expr.ty.kind.as_ref() {
                IrTyKind::Closure(ty) => ty.captures.len(),
                _ => panic!("expected closure type"),
            };

            (captured, closure.params.len(), captured_tys_len)
        }
        kind => panic!("expected closure expr, got {kind:?}"),
    };

    assert_eq!(
        captures,
        HashSet::from([Symbol::from("a".to_string()), Symbol::from("b".to_string())])
    );
    assert_eq!(param_len, 0);
    assert_eq!(capture_tys_len, 2);

    Ok(())
}
