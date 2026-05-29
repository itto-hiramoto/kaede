mod common;

use common::{semantic_analyze, semantic_analyze_as_non_entry};
use kaede_ir::{
    expr::{Expr, ExprKind},
    stmt::Stmt,
    top::TopLevel,
    ty::contains_type_var,
    CompileUnit,
};
use kaede_symbol::Symbol;

use crate::common::{semantic_analyze_expect_error, semantic_analyze_non_entry_expect_error};

fn find_generic_callee_in_expr(expr: &Expr) -> Option<kaede_ir::qualified_symbol::QualifiedSymbol> {
    match &expr.kind {
        ExprKind::GenericFnCall(call) => Some(call.callee.name.clone()),
        ExprKind::FnCall(call) => {
            if call.callee.link_once {
                Some(call.callee.name.clone())
            } else {
                None
            }
        }
        ExprKind::Return(ret) => ret
            .as_ref()
            .and_then(|inner| find_generic_callee_in_expr(inner)),
        ExprKind::Block(block) => find_generic_callee_in_block(block),
        _ => None,
    }
}

fn find_generic_callee_in_block(
    block: &kaede_ir::stmt::Block,
) -> Option<kaede_ir::qualified_symbol::QualifiedSymbol> {
    for stmt in &block.body {
        match stmt {
            Stmt::Expr(expr) => {
                if let Some(name) = find_generic_callee_in_expr(expr) {
                    return Some(name);
                }
            }
            Stmt::Let(let_stmt) => {
                if let Some(init) = &let_stmt.init {
                    if let Some(name) = find_generic_callee_in_expr(init) {
                        return Some(name);
                    }
                }
            }
            Stmt::TupleUnpack(tuple_unpack) => {
                if let Some(name) = find_generic_callee_in_expr(&tuple_unpack.init) {
                    return Some(name);
                }
            }
            Stmt::Assign(assign) => {
                if let Some(name) = find_generic_callee_in_expr(&assign.assignee) {
                    return Some(name);
                }
                if let Some(name) = find_generic_callee_in_expr(&assign.value) {
                    return Some(name);
                }
            }
        }
    }

    block
        .last_expr
        .as_ref()
        .and_then(|expr| find_generic_callee_in_expr(expr))
}

fn main_generic_callee_name(ir: &CompileUnit) -> kaede_ir::qualified_symbol::QualifiedSymbol {
    let main_name = Symbol::from("kdmain".to_owned());
    let main_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_) if fn_.decl.name.symbol() == main_name => Some(fn_),
            _ => None,
        })
        .expect("expected main function");

    let body = main_fn.body.as_ref().expect("expected main body");
    find_generic_callee_in_block(body).expect("expected generated call in main body")
}

fn generated_impl_method_symbols(ir: &CompileUnit) -> Vec<String> {
    ir.top_levels
        .iter()
        .flat_map(|top| match top {
            TopLevel::Impl(impl_) => impl_
                .methods
                .iter()
                .map(|method| method.decl.name.symbol().to_string())
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        })
        .collect()
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    semantic_analyze("fun foo() {}")?;
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
    semantic_analyze("fun foo() -> i32 { return 1 }")?;
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
    semantic_analyze("fun foo(a: i32, b: i32) -> i32 { return a + b }")?;
    Ok(())
}

#[test]
fn function_with_generic_params() -> anyhow::Result<()> {
    semantic_analyze(
        "fun foo<T, U>(a: T, b: U) -> T {
        return a + b
    }
    fun f() {
        foo<i32, i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fun foo<T>(a: T, b: U) -> T {
        return a + b
    }
    fun f() {
        foo<i32>(1, 2)
    }",
    )?;

    semantic_analyze_expect_error(
        "fun foo<T, U>(a: T, b: U) -> T {
        return a + b
    }
    fun f() {
        foo<123>(1, 2)
    }",
    )?;

    // Generic function arguments should be inferred from call arguments.
    semantic_analyze(
        "fun foo<T, U>(a: T, b: U) -> T {
        return a + b
    }
    fun f() -> i32 {
        return foo(1, 2)
    }",
    )?;

    Ok(())
}

#[test]
fn generic_type() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Foo<T> { a: T }
        fun f() {
            let x = Foo<i32> { a: 1 }
        }
    ",
    )?;
    semantic_analyze(
        "enum Foo<T> { A, B(T) }
        fun f() {
            let x = Foo<i32>::B(1)
        }
    ",
    )?;
    semantic_analyze(
        "fun foo<T>(a: T) -> T { return a }
        fun f() {
            foo<i32>(1)
        }
    ",
    )?;
    semantic_analyze(
        "fun foo<T>(a: T) -> T { return a }
        fun f() {
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
            fun f(self) -> T {
                return self.a
            }
        }
        fun f() {
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
            fun f(self) -> i32 {
                return self.a
            }
        }
        fun f() {
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
            fun new(n: i32) -> Foo {
                return Foo { a: n }
            }
            fun get_a(self) -> i32 {
                return self.a
            }
        }
        fun f() -> i32 {
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
            fun new(n: T) -> Foo<T> {
                return Foo<T> { a: n }
            }
            fun get_a(self) -> T {
                return self.a
            }
        }
        fun f() -> i32 {
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
        r#"extern "C" fun foo() -> i32
        fun f() {
            foo()
        }
    "#,
    )?;
    Ok(())
}

#[test]
fn type_alias() -> anyhow::Result<()> {
    // Type alias resolved and used in function signature
    semantic_analyze(
        "type MyInt = i32
        fun foo(x: MyInt) -> i32 {
            return x
        }",
    )?;
    Ok(())
}

#[test]
fn type_alias_pub() -> anyhow::Result<()> {
    semantic_analyze(
        "export type MyInt = i32
        fun foo(x: MyInt) -> i32 {
            return x
        }",
    )?;
    Ok(())
}

#[test]
fn generic_type_alias_ends_at_newline_after_gt() -> anyhow::Result<()> {
    semantic_analyze(
        "struct Vector<T> { value: T }
        type BNode = Vector<u8>
        fun foo(x: BNode) {}",
    )?;
    Ok(())
}

#[test]
fn generated_generic_function_has_concrete_types_after_inference() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fun __test_id<T>(x: T) -> T {
            return x
        }

        fun main() -> i32 {
            let n: i32 = __test_id(1)
            return 58
        }
        "#,
    )?;

    let generated_name = main_generic_callee_name(&ir);
    let generated_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_) if fn_.decl.name == generated_name => Some(fn_),
            _ => None,
        })
        .expect("expected generated generic function");

    assert!(
        !contains_type_var(&generated_fn.decl.return_ty),
        "generated function return type should be concrete after inference"
    );
    assert!(
        generated_fn
            .decl
            .generic_instance
            .as_ref()
            .is_some_and(|instance| !instance.contains_type_var()),
        "generated function should retain a concrete structural generic instance"
    );

    Ok(())
}

#[test]
fn generated_generic_impl_method_has_concrete_types_after_inference() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        struct __TestBox<T> {
            value: T,
        }

        impl<T> __TestBox<T> {
            fun __test_get(self) -> T {
                return self.value
            }
        }

        fun main() -> i32 {
            let b = __TestBox<i32> { value: 58 }
            return b.__test_get()
        }
        "#,
    )?;

    let generated_name = main_generic_callee_name(&ir);
    let generated_method = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Impl(impl_) => impl_
                .methods
                .iter()
                .find(|method| method.decl.name == generated_name),
            _ => None,
        })
        .expect("expected generated generic impl method");

    assert!(
        generated_method
            .decl
            .params
            .iter()
            .all(|param| !contains_type_var(&param.ty)),
        "generated method params should be concrete after inference"
    );
    assert!(
        !contains_type_var(&generated_method.decl.return_ty),
        "generated method return type should be concrete after inference"
    );
    assert!(
        generated_method
            .decl
            .generic_instance
            .as_ref()
            .is_some_and(|instance| !instance.contains_type_var()),
        "generated method should retain a concrete structural generic instance"
    );

    Ok(())
}

#[test]
fn generic_type_instantiation_does_not_emit_impl_method_bodies() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fun main() -> i32 {
            let opt = Option::Some(58)
            return 0
        }
        "#,
    )?;

    let generated_methods = generated_impl_method_symbols(&ir);
    assert!(
        generated_methods
            .iter()
            .all(|name| !name.contains("Option_")),
        "type-only Option instantiation should not emit impl method bodies: {generated_methods:?}"
    );

    Ok(())
}

#[test]
fn generic_impl_method_call_emits_only_referenced_method_body() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fun main() -> i32 {
            let opt = Option::Some(58)
            if opt.is_some() {
                return 1
            }
            return 0
        }
        "#,
    )?;

    let generated_methods = generated_impl_method_symbols(&ir);
    assert!(
        generated_methods
            .iter()
            .any(|name| name.ends_with(".is_some")),
        "expected generated Option.is_some body: {generated_methods:?}"
    );
    assert!(
        generated_methods
            .iter()
            .all(|name| !name.ends_with(".is_none") && !name.ends_with(".unwrap")),
        "unreferenced Option methods should not be emitted: {generated_methods:?}"
    );

    Ok(())
}

#[test]
fn slice_generic_functions_with_distinct_type_variables_analyze() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        fun takes<T>(xs: [T]) -> u64 {
            return xs.len()
        }

        fun also<U>(ys: [U]) -> u64 {
            return ys.len()
        }

        fun main() -> i32 {
            return 0
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn slice_methods_on_distinct_element_types_are_separate_instantiations() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fun main() -> i32 {
            let xs: [u8] = [1, 2]
            let ys: [i32] = [3, 4]
            return (xs.len() + ys.len()) as i32
        }
        "#,
    )?;

    let generated_methods = generated_impl_method_symbols(&ir);
    assert!(
        generated_methods.iter().any(|name| name == "slice_u8.len"),
        "expected slice_u8.len body: {generated_methods:?}"
    );
    assert!(
        generated_methods.iter().any(|name| name == "slice_i32.len"),
        "expected slice_i32.len body: {generated_methods:?}"
    );

    Ok(())
}

#[test]
fn slice_method_call_emits_only_referenced_method_body() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        fun main() -> i32 {
            let buf: [u8] = [1, 2, 3]
            return buf.len() as i32
        }
        "#,
    )?;

    let generated_methods = generated_impl_method_symbols(&ir);
    assert!(
        generated_methods.iter().any(|name| name == "slice_u8.len"),
        "expected generated slice_u8.len body: {generated_methods:?}"
    );
    assert!(
        !generated_methods
            .iter()
            .any(|name| name == "slice_u8.as_ptr"),
        "unreferenced slice_u8.as_ptr should not be emitted: {generated_methods:?}"
    );

    Ok(())
}

#[test]
fn static_method_on_generic_type_allows_omitted_type_args() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        fun main() -> i32 {
            let mut v = Vector::new()
            v.push(58)
            return match v.at(0) {
                Option::Some(n) => n,
                Option::None => 0,
            }
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn option_some_allows_omitted_type_args() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        fun main() -> i32 {
            let opt = Option::Some(58)
            return match opt {
                Option::Some(n) => n,
                Option::None => 0,
            }
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn vector_new_infers_struct_type_from_push() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        struct S {
            x: i32,
        }

        fun main() -> i32 {
            let mut v = Vector::new()
            v.push(S { x: 123 })
            return 0
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn option_some_struct_payload_without_explicit_args() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        struct S {
            n: i32,
        }

        fun main() -> i32 {
            let opt = Option::Some(S { n: 58 })
            return match opt {
                Option::Some(v) => v.n,
                Option::None => 0,
            }
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn option_none_allows_omitted_type_args_when_expected_type_is_known() -> anyhow::Result<()> {
    semantic_analyze(
        r#"
        fun wrap(flag: bool) -> Option<i32> {
            if flag {
                return Option::Some(58)
            }

            return Option::None
        }

        fun main() -> i32 {
            let opt: Option<i32> = Option::None
            return match wrap(false) {
                Option::Some(n) => n,
                Option::None => match opt {
                    Option::Some(n) => n,
                    Option::None => 58,
                },
            }
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn struct_field_accepts_option_some_with_omitted_type_args() -> anyhow::Result<()> {
    // Regression: a struct literal that puts `Option::Some(value)` into a recursive
    // `Option<Self>` field used to be rejected by the pre-inference shape check
    // because the value's payload type was still an unbound type variable.
    semantic_analyze(
        r#"
        struct Node {
            val: i32,
            left: Option<Node>,
        }

        fun main() -> i32 {
            let n1 = Node { val: 1, left: Option<Node>::None }
            let n2 = Node { val: 2, left: Option::Some(n1) }
            return n2.val
        }
        "#,
    )?;

    Ok(())
}

#[test]
fn option_none_without_context_still_requires_type_args() -> anyhow::Result<()> {
    let err = semantic_analyze_expect_error(
        r#"
        fun main() -> i32 {
            let opt = Option::None
            return 0
        }
        "#,
    )?;

    assert!(err.to_string().contains("cannot infer type for expression"));

    Ok(())
}

#[test]
fn type_alias_struct() -> anyhow::Result<()> {
    semantic_analyze(
        "struct S { x: i32 }
         type AliasS = S
         fun foo() -> S {
            return AliasS { x: 1 }
         }",
    )?;
    Ok(())
}

#[test]
fn top_level_statements_synthesize_main() -> anyhow::Result<()> {
    let ir = semantic_analyze(
        r#"
        let x = 1
        return x
        "#,
    )?;

    let main_name = Symbol::from("kdmain".to_owned());
    let main_fn = ir
        .top_levels
        .iter()
        .find_map(|top| match top {
            TopLevel::Fn(fn_) if fn_.decl.name.symbol() == main_name => Some(fn_),
            _ => None,
        })
        .expect("expected synthetic main");

    let body = main_fn.body.as_ref().expect("expected synthetic main body");
    assert!(matches!(body.body.first(), Some(Stmt::Let(_))));

    Ok(())
}

#[test]
fn top_level_statements_conflict_with_explicit_main() -> anyhow::Result<()> {
    let err = semantic_analyze_expect_error(
        r#"
        let x = 1
        fun main() -> i32 { return x }
        "#,
    )?;

    assert!(err
        .to_string()
        .contains("top-level statements cannot be combined"));

    Ok(())
}

#[test]
fn top_level_statements_are_rejected_in_non_entry_units() -> anyhow::Result<()> {
    let err = semantic_analyze_non_entry_expect_error("let x = 1")?;

    assert!(err.to_string().contains("only allowed in the entry unit"));

    Ok(())
}

#[test]
fn main_is_rejected_in_non_entry_units() -> anyhow::Result<()> {
    let err = semantic_analyze_non_entry_expect_error("fun main() -> i32 { return 0 }")?;

    assert!(err.to_string().contains("`main` is only allowed"));

    Ok(())
}

#[test]
fn top_level_functions_do_not_capture_script_locals() -> anyhow::Result<()> {
    let err = semantic_analyze_expect_error(
        r#"
        let x = 1
        fun helper() -> i32 { return x }
        return helper()
        "#,
    )?;

    assert!(err.to_string().contains("`x` was not declared"));

    Ok(())
}

#[test]
fn plain_functions_are_allowed_in_non_entry_units() -> anyhow::Result<()> {
    semantic_analyze_as_non_entry("fun helper() -> i32 { return 0 }")?;
    Ok(())
}

#[test]
fn interface_declarations_are_accepted() -> anyhow::Result<()> {
    // Name the interface something the prelude does not export, so the
    // `find_map` below is guaranteed to match this source file's declaration.
    let unit = semantic_analyze_as_non_entry(
        "\
interface TestReader {
    fun read(mut self, buf: i32) -> i32
    fun peek(self) -> i32
}
",
    )?;

    let iface = unit
        .top_levels
        .iter()
        .find_map(|tl| match tl {
            TopLevel::Interface(iface)
                if iface.name.symbol() == Symbol::from("TestReader".to_string()) =>
            {
                Some(iface)
            }
            _ => None,
        })
        .expect("TestReader interface must be emitted");

    assert_eq!(iface.methods.len(), 2);
    assert_eq!(iface.methods[0].name, Symbol::from("read".to_string()));
    assert_eq!(iface.methods[1].name, Symbol::from("peek".to_string()));

    Ok(())
}

#[test]
fn generic_bounds_accept_interfaces() -> anyhow::Result<()> {
    semantic_analyze_as_non_entry(
        "\
interface Reader {
    fun read(self) -> i32
}

struct Box<T: Reader> {}

fun drain<T: Reader>(x: T) -> i32 {
    return 0
}
",
    )?;

    Ok(())
}

#[test]
fn generic_bounds_reject_non_interfaces() -> anyhow::Result<()> {
    let err = semantic_analyze_non_entry_expect_error(
        "\
struct Reader {}

fun drain<T: Reader>(x: T) -> i32 {
    return 0
}
",
    )?;

    assert!(err
        .to_string()
        .contains("generic bound `Reader` must reference an interface"));

    Ok(())
}

#[test]
fn generic_function_call_checks_bound_conformance() -> anyhow::Result<()> {
    semantic_analyze(
        "\
interface Reader {
    fun read(self) -> i32
}

struct File {}

impl File {
    fun read(self) -> i32 {
        return 1
    }
}

fun drain<T: Reader>(x: T) -> i32 {
    return 0
}

fun main() -> i32 {
    let file = File {}
    return drain(file)
}
",
    )?;

    let err = semantic_analyze_expect_error(
        "\
interface Reader {
    fun read(self) -> i32
}

struct NoRead {}

fun drain<T: Reader>(x: T) -> i32 {
    return 0
}

fun main() -> i32 {
    let value = NoRead {}
    return drain(value)
}
",
    )?;

    assert!(err
        .to_string()
        .contains("does not satisfy interface `Reader`"));

    Ok(())
}

#[test]
fn generic_type_instantiation_checks_bound_conformance() -> anyhow::Result<()> {
    semantic_analyze_as_non_entry(
        "\
interface Reader {
    fun read(self) -> i32
}

struct File {}

impl File {
    fun read(self) -> i32 {
        return 1
    }
}

struct Box<T: Reader> {
    value: T,
}

fun helper() {
    let _ok = Box<File> { value: File {} }
}
",
    )?;

    let err = semantic_analyze_non_entry_expect_error(
        "\
interface Reader {
    fun read(self) -> i32
}

struct NoRead {}

struct Box<T: Reader> {
    value: T,
}

fun helper() {
    let _bad = Box<NoRead> { value: NoRead {} }
}
",
    )?;

    assert!(err
        .to_string()
        .contains("does not satisfy interface `Reader`"));

    Ok(())
}

fn find_interface_box_in_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::InterfaceBox(_) => true,
        ExprKind::Return(ret) => ret.as_deref().is_some_and(find_interface_box_in_expr),
        ExprKind::Block(block) => find_interface_box_in_block(block),
        ExprKind::FnCall(call) => call.args.0.iter().any(find_interface_box_in_expr),
        _ => false,
    }
}

fn find_interface_box_in_block(block: &kaede_ir::stmt::Block) -> bool {
    for stmt in &block.body {
        match stmt {
            Stmt::Expr(expr) => {
                if find_interface_box_in_expr(expr) {
                    return true;
                }
            }
            Stmt::Let(let_stmt) => {
                if let_stmt
                    .init
                    .as_ref()
                    .is_some_and(find_interface_box_in_expr)
                {
                    return true;
                }
            }
            Stmt::Assign(assign) => {
                if find_interface_box_in_expr(&assign.value) {
                    return true;
                }
            }
            _ => {}
        }
    }
    block
        .last_expr
        .as_deref()
        .is_some_and(find_interface_box_in_expr)
}

fn function_body<'a>(
    unit: &'a kaede_ir::CompileUnit,
    name: &str,
) -> Option<&'a kaede_ir::stmt::Block> {
    unit.top_levels.iter().find_map(|tl| match tl {
        TopLevel::Fn(fn_) if fn_.decl.name.symbol() == Symbol::from(name.to_string()) => {
            fn_.body.as_ref()
        }
        _ => None,
    })
}

#[test]
fn coerces_concrete_to_interface_in_let_call_return() -> anyhow::Result<()> {
    let source = "\
interface Reader {
    fun read(self) -> i32
}

struct File {
    handle: i32,
}

impl File {
    fun read(self) -> i32 {
        return self.handle
    }
}

fun take(r: Reader) -> i32 {
    return 0
}

fun make() -> Reader {
    let f = File { handle: 1 }
    return f
}

fun use_it() {
    let f = File { handle: 1 }
    take(f)
    let r: Reader = f
}
";

    let unit = semantic_analyze_as_non_entry(source)?;

    let make_body = function_body(&unit, "make").expect("`make` should be present");
    assert!(
        find_interface_box_in_block(make_body),
        "return site should insert an InterfaceBox"
    );

    let use_it_body = function_body(&unit, "use_it").expect("`use_it` should be present");
    assert!(
        find_interface_box_in_block(use_it_body),
        "call argument or let with annotation should insert an InterfaceBox"
    );

    Ok(())
}

#[test]
fn interface_coercion_rejects_missing_method() {
    let err = semantic_analyze_non_entry_expect_error(
        "\
interface Reader {
    fun read(self) -> i32
}

struct File {
    handle: i32,
}

fun take(r: Reader) -> i32 {
    return 0
}

fun use_it() {
    let f = File { handle: 1 }
    take(f)
}
",
    )
    .unwrap();
    let msg = err.to_string();
    assert!(
        msg.contains("does not satisfy")
            || msg.contains("cannot convert")
            || msg.contains("interface"),
        "unexpected error: {msg}"
    );
}

fn find_interface_method_call_in_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::InterfaceMethodCall(_) => true,
        ExprKind::Return(ret) => ret
            .as_deref()
            .is_some_and(find_interface_method_call_in_expr),
        ExprKind::Block(block) => find_interface_method_call_in_block(block),
        ExprKind::FnCall(call) => call.args.0.iter().any(find_interface_method_call_in_expr),
        ExprKind::InterfaceBox(b) => find_interface_method_call_in_expr(&b.value),
        _ => false,
    }
}

fn find_interface_method_call_in_block(block: &kaede_ir::stmt::Block) -> bool {
    for stmt in &block.body {
        match stmt {
            Stmt::Expr(expr) => {
                if find_interface_method_call_in_expr(expr) {
                    return true;
                }
            }
            Stmt::Let(let_stmt) => {
                if let_stmt
                    .init
                    .as_ref()
                    .is_some_and(find_interface_method_call_in_expr)
                {
                    return true;
                }
            }
            Stmt::Assign(assign) => {
                if find_interface_method_call_in_expr(&assign.value) {
                    return true;
                }
            }
            _ => {}
        }
    }
    block
        .last_expr
        .as_deref()
        .is_some_and(find_interface_method_call_in_expr)
}

#[test]
fn dispatches_method_call_on_interface_value() -> anyhow::Result<()> {
    let source = "\
interface Reader {
    fun read(self) -> i32
}

struct File {
    handle: i32,
}

impl File {
    fun read(self) -> i32 {
        return self.handle
    }
}

fun run(r: Reader) -> i32 {
    return r.read()
}
";

    let unit = semantic_analyze_as_non_entry(source)?;
    let run_body = function_body(&unit, "run").expect("`run` should be present");
    assert!(
        find_interface_method_call_in_block(run_body),
        "method call on interface value should produce InterfaceMethodCall"
    );

    Ok(())
}

#[test]
fn interface_method_call_rejects_unknown_method() {
    let err = semantic_analyze_non_entry_expect_error(
        "\
interface Reader {
    fun read(self) -> i32
}

fun run(r: Reader) -> i32 {
    return r.write()
}
",
    )
    .unwrap();
    let msg = err.to_string();
    assert!(
        msg.contains("no method") || msg.contains("write"),
        "unexpected error: {msg}"
    );
}

#[test]
fn interface_usable_as_parameter_and_field_type() -> anyhow::Result<()> {
    let unit = semantic_analyze_as_non_entry(
        "\
interface Reader {
    fun read(self) -> i32
}

struct Pipeline {
    source: Reader,
}

fun take(r: Reader) -> i32 {
    return 0
}
",
    )?;

    let has_fn = unit.top_levels.iter().any(|tl| match tl {
        TopLevel::Fn(fn_) => fn_.decl.name.symbol() == Symbol::from("take".to_string()),
        _ => false,
    });
    assert!(has_fn, "`take` function should be present in IR");

    Ok(())
}
