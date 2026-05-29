mod common;

use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;

/// Helper struct to manage test project setup and analysis
struct ImportTestProject {
    temp_dir: TempDir,
}

impl ImportTestProject {
    fn new() -> anyhow::Result<Self> {
        let temp_dir = TempDir::new()?;
        Ok(Self { temp_dir })
    }

    /// Create a module file with the given content
    fn create_module(&self, path: &str, content: &str) -> anyhow::Result<()> {
        let file_path = self.temp_dir.path().join(format!("{path}.kd"));
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(file_path, content)?;
        Ok(())
    }

    /// Run semantic analysis on the given main content
    fn analyze(&self, main_content: &str) -> anyhow::Result<kaede_ir::CompileUnit> {
        let main_file = self.temp_dir.path().join("main.kd");
        fs::write(&main_file, main_content)?;

        let ast = kaede_parse::Parser::new(
            main_content,
            kaede_span::file::FilePath::from(main_file.clone()),
        )
        .run()?;

        let mut analyzer = kaede_semantic::SemanticAnalyzer::new(
            kaede_span::file::FilePath::from(main_file),
            self.temp_dir.path().to_path_buf(),
            Some(std::path::PathBuf::from("rust")),
        );

        analyzer.analyze(
            ast,
            kaede_semantic::AnalyzeOptions {
                no_autoload: false,
                no_prelude: false,
                is_entry_unit: true,
            },
        )
    }

    /// Expect analysis to fail
    fn analyze_expect_error(&self, main_content: &str) -> anyhow::Result<()> {
        let result = self.analyze(main_content);
        if result.is_ok() {
            anyhow::bail!("Expected analysis to fail, but it succeeded");
        }
        Ok(())
    }
}

/// Test case definition for cleaner test organization
struct ImportTestCase {
    name: &'static str,
    modules: HashMap<&'static str, &'static str>,
    main_content: &'static str,
    expected_min_top_levels: usize,
    should_fail: bool,
}

impl ImportTestCase {
    fn run(&self) -> anyhow::Result<()> {
        let project = ImportTestProject::new()?;

        // Create all modules
        for (path, content) in &self.modules {
            project.create_module(path, content)?;
        }

        // Run analysis
        if self.should_fail {
            project.analyze_expect_error(self.main_content)?;
        } else {
            let result = project.analyze(self.main_content)?;
            assert!(
                result.top_levels.len() >= self.expected_min_top_levels,
                "Test '{}': Expected at least {} top levels, got {}",
                self.name,
                self.expected_min_top_levels,
                result.top_levels.len()
            );
        }

        Ok(())
    }
}

#[test]
fn import_basic_function() -> anyhow::Result<()> {
    ImportTestCase {
        name: "basic_function",
        modules: HashMap::from([("utils", "export fun helper() -> i32 { return 42 }")]),
        main_content: r#"
            import utils
            fun main() -> i32 {
                return utils.helper()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn imported_function_call_inside_closure_can_use_call_site_local_arg() -> anyhow::Result<()> {
    let project = ImportTestProject::new()?;
    project.create_module("utils", "export fun id(x: i32) -> i32 { return x }")?;

    project.analyze(
        r#"
            import utils

            fun main() -> i32 {
                let x = 42
                let closure = || utils.id(x)
                return closure()
            }
        "#,
    )?;

    Ok(())
}

#[test]
fn imported_module_body_does_not_fallback_to_entry_module_private_items() -> anyhow::Result<()> {
    let project = ImportTestProject::new()?;
    project.create_module(
        "utils",
        r#"
            export fun call_helper<T>(value: T) -> i32 {
                return helper()
            }
        "#,
    )?;

    project.analyze_expect_error(
        r#"
            import utils

            fun helper() -> i32 {
                return 42
            }

            fun main() -> i32 {
                return utils.call_helper(0)
            }
        "#,
    )?;

    Ok(())
}

#[test]
fn import_struct_and_impl() -> anyhow::Result<()> {
    ImportTestCase {
        name: "struct_and_impl",
        modules: HashMap::from([(
            "math",
            r#"
                export struct Point {
                    x: i32,
                    y: i32
                }

                impl Point {
                    export fun new(x: i32, y: i32) -> Point {
                        return Point { x: x, y: y }
                    }

                    export fun add(self, other: Point) -> Point {
                        return Point { x: self.x + other.x, y: self.y + other.y }
                    }

                    export fun get_x(self) -> i32 {
                        return self.x
                    }

                    export fun get_y(self) -> i32 {
                        return self.y
                    }
                }
            "#,
        )]),
        main_content: r#"
            import math
            fun main() -> i32 {
                let p1 = math.Point::new(1, 2)
                let p2 = math.Point::new(3, 4)
                let p3 = p1.add(p2)
                return p3.get_x() + p3.get_y()
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_builtin_method_returning_imported_struct() -> anyhow::Result<()> {
    ImportTestCase {
        name: "builtin_method_returning_imported_struct",
        modules: HashMap::from([(
            "m",
            r#"
                export struct Wrap {
                    value: i32
                }

                impl i32 {
                    export fun to_wrap(self) -> Wrap {
                        return Wrap { value: self }
                    }
                }
            "#,
        )]),
        main_content: r#"
            import m
            fun main() -> i32 {
                let w = 58.to_wrap()
                return w.value
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_nested_module() -> anyhow::Result<()> {
    ImportTestCase {
        name: "nested_module",
        modules: HashMap::from([(
            "nested/deep/module",
            "export fun deep_function() -> i32 { return 999 }",
        )]),
        main_content: r#"
            import nested.deep.module
            fun main() -> i32 {
                return nested.deep.module.deep_function()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_multiple_modules() -> anyhow::Result<()> {
    ImportTestCase {
        name: "multiple_modules",
        modules: HashMap::from([
            ("utils", "export fun util_func() -> i32 { return 1 }"),
            ("math", "export fun math_func() -> i32 { return 2 }"),
        ]),
        main_content: r#"
            import utils
            import math
            fun main() -> i32 {
                return utils.util_func() + math.math_func()
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_duplicate_modules() -> anyhow::Result<()> {
    ImportTestCase {
        name: "duplicate_modules",
        modules: HashMap::from([("utils", "export fun helper() -> i32 { return 42 }")]),
        main_content: r#"
            import utils
            import utils  // Duplicate import should be handled gracefully
            fun main() -> i32 {
                return utils.helper()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_chained_modules() -> anyhow::Result<()> {
    ImportTestCase {
        name: "chained_modules",
        modules: HashMap::from([
            (
                "math",
                "export fun add(a: i32, b: i32) -> i32 { return a + b }",
            ),
            (
                "utils",
                r#"
                import math
                export fun double_add(a: i32, b: i32) -> i32 {
                    return math.add(a, b) * 2
                }
            "#,
            ),
        ]),
        main_content: r#"
            import utils
            fun main() -> i32 {
                return utils.double_add(5, 3)
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_generic_types() -> anyhow::Result<()> {
    ImportTestCase {
        name: "generic_types",
        modules: HashMap::from([(
            "container",
            r#"
                export struct Container<T> {
                    value: T
                }

                impl<T> Container<T> {
                    export fun new(value: T) -> Container<T> {
                        return Container<T> { value: value }
                    }

                    export fun get(self) -> T {
                        return self.value
                    }
                }
            "#,
        )]),
        main_content: r#"
            import container
            fun main() -> i32 {
                let c = container.Container<i32>::new(42)
                return c.get()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_enums() -> anyhow::Result<()> {
    ImportTestCase {
        name: "enums",
        modules: HashMap::from([(
            "types",
            r#"
                export enum Color {
                    Red,
                    Green,
                    Blue
                }

                export enum Opt<T> {
                    Some(T),
                    None
                }

                export fun get_red() -> Color {
                    return Color::Red
                }
            "#,
        )]),
        main_content: r#"
            import types
            fun main() -> i32 {
                let c = types.Color::Red
                match c {
                    types.Color::Red => return 0,
                    types.Color::Green => return 1,
                    types.Color::Blue => return 2,
                }
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_with_use_statement() -> anyhow::Result<()> {
    ImportTestCase {
        name: "with_use_statement",
        modules: HashMap::from([(
            "graphics/shapes",
            r#"
                export struct Circle {
                    radius: i32
                }

                export fun get_radius(c: Circle) -> i32 {
                    return c.radius
                }
            "#,
        )]),
        main_content: r#"
            import graphics.shapes
            use graphics.shapes.Circle
            use graphics.shapes.get_radius

            fun main() -> i32 {
                let c = Circle { radius: 5 }
                return get_radius(c)
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_nonexistent_file() -> anyhow::Result<()> {
    ImportTestCase {
        name: "nonexistent_file",
        modules: HashMap::new(),
        main_content: r#"
            import nonexistent
            fun main() -> i32 {
                return 0
            }
        "#,
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn import_empty_module() -> anyhow::Result<()> {
    ImportTestCase {
        name: "empty_module",
        modules: HashMap::from([("empty", "")]),
        main_content: "import empty\nfun main() -> i32 { return 0 }",
        expected_min_top_levels: 1,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_module_with_only_private_items() -> anyhow::Result<()> {
    ImportTestCase {
        name: "module_with_only_private_items",
        modules: HashMap::from([("private", "fun private_func() -> i32 { return 42 }")]),
        main_content: "import private\nfun main() -> i32 { return 0 }",
        expected_min_top_levels: 1,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_private_items() -> anyhow::Result<()> {
    ImportTestCase {
        name: "import_private_items",
        modules: HashMap::from([("private", "fun private_func() -> i32 { return 42 }")]),
        main_content: "import private\nfun main() -> i32 { return private.private_func() }",
        expected_min_top_levels: 1,
        should_fail: true,
    }
    .run()
}

#[test]
fn import_module_with_top_level_statements_fails() -> anyhow::Result<()> {
    ImportTestCase {
        name: "module_with_top_level_statements",
        modules: HashMap::from([("side_effect", "let x = 1")]),
        main_content: "import side_effect\nfun main() -> i32 { return 0 }",
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn import_module_with_main_fails() -> anyhow::Result<()> {
    ImportTestCase {
        name: "module_with_main",
        modules: HashMap::from([("nested", "fun main() -> i32 { return 0 }")]),
        main_content: "import nested\nfun main() -> i32 { return 0 }",
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn import_mutual_recursive_functions() -> anyhow::Result<()> {
    ImportTestCase {
        name: "mutual_recursive_function",
        modules: HashMap::from([(
            "a",
            r#"
            export fun a(i: i32) -> i32 {
                if i == 0 {
                    return 0
                }
                return b(i - 1)
            }
            fun b(i: i32) -> i32 { return a(i - 1) }
        "#,
        )]),
        main_content: "import a\nfun main() -> i32 { return a.a(10) }",
        expected_min_top_levels: 1,
        should_fail: false,
    }
    .run()
}

// Sibling-file module layout: `net.kd` defines module `net`, and `net/*.kd`
// define its submodules. They can coexist. These tests pin the behavior so it
// cannot regress silently.

#[test]
fn sibling_layout_body_and_submodule() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_body_and_submodule",
        modules: HashMap::from([
            ("net", "export fun root_fn() -> i32 { return 1 }"),
            ("net/tcp", "export fun tcp_fn() -> i32 { return 2 }"),
        ]),
        main_content: r#"
            import net
            import net.tcp
            fun main() -> i32 {
                return net.root_fn() + net.tcp.tcp_fn()
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn sibling_layout_submodule_only_succeeds() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_submodule_only_succeeds",
        modules: HashMap::from([("net/tcp", "export fun tcp_fn() -> i32 { return 7 }")]),
        main_content: r#"
            import net.tcp
            fun main() -> i32 {
                return net.tcp.tcp_fn()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn sibling_layout_body_missing_import_fails() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_body_missing_import_fails",
        modules: HashMap::from([("net/tcp", "export fun tcp_fn() -> i32 { return 7 }")]),
        main_content: r#"
            import net
            fun main() -> i32 {
                return 0
            }
        "#,
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn sibling_layout_nested_hierarchy() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_nested_hierarchy",
        modules: HashMap::from([
            ("net", "export fun root_fn() -> i32 { return 1 }"),
            ("net/http", "export fun http_fn() -> i32 { return 10 }"),
            (
                "net/http/request",
                "export fun request_fn() -> i32 { return 100 }",
            ),
        ]),
        main_content: r#"
            import net
            import net.http
            import net.http.request
            fun main() -> i32 {
                return net.root_fn() + net.http.http_fn() + net.http.request.request_fn()
            }
        "#,
        expected_min_top_levels: 4,
        should_fail: false,
    }
    .run()
}

#[test]
fn sibling_layout_body_imports_own_submodule() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_body_imports_own_submodule",
        modules: HashMap::from([
            (
                "net",
                r#"
                import net.tcp
                export fun root_fn() -> i32 { return net.tcp.tcp_fn() + 1 }
            "#,
            ),
            ("net/tcp", "export fun tcp_fn() -> i32 { return 41 }"),
        ]),
        main_content: r#"
            import net
            fun main() -> i32 {
                return net.root_fn()
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

#[test]
fn sibling_layout_use_from_body() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_use_from_body",
        modules: HashMap::from([
            ("net", "export fun root_fn() -> i32 { return 17 }"),
            ("net/tcp", "export fun tcp_fn() -> i32 { return 25 }"),
        ]),
        main_content: r#"
            import net
            use net.root_fn
            fun main() -> i32 {
                return root_fn()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

// Observational test: when `net.kd` exports an item with the same name as a
// sibling submodule `net/tcp.kd`, expression-position `net.tcp` resolves to the
// item in `net.kd`, not the submodule. The submodule can still be loaded (and
// its own imported symbols used via `use`), but the dotted path cannot reach
// into it because the item in the parent module shadows the submodule name.
// This test pins the current behavior so any future change is intentional.
#[test]
fn sibling_layout_item_shadows_submodule_in_expr_position() -> anyhow::Result<()> {
    ImportTestCase {
        name: "sibling_layout_item_shadows_submodule_in_expr_position",
        modules: HashMap::from([
            ("net", "export fun tcp() -> i32 { return 5 }"),
            ("net/tcp", "export fun inner() -> i32 { return 3 }"),
        ]),
        main_content: r#"
            import net
            import net.tcp
            fun main() -> i32 {
                return net.tcp()
            }
        "#,
        expected_min_top_levels: 3,
        should_fail: false,
    }
    .run()
}

// `use` visibility: a bare `use` is module-local (private), `export use`
// re-exports the name. These tests pin both directions so the internal
// bindings of stdlib/user modules do not silently leak to importers.

#[test]
fn private_use_is_not_visible_to_importers() -> anyhow::Result<()> {
    ImportTestCase {
        name: "private_use_is_not_visible_to_importers",
        modules: HashMap::from([
            ("provider", "export fun hidden() -> i32 { return 1 }"),
            (
                "middle",
                r#"
                import provider
                use provider.hidden
            "#,
            ),
        ]),
        main_content: r#"
            import middle
            fun main() -> i32 {
                return middle.hidden()
            }
        "#,
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn export_use_is_visible_to_importers() -> anyhow::Result<()> {
    ImportTestCase {
        name: "export_use_is_visible_to_importers",
        modules: HashMap::from([
            ("provider", "export fun exposed() -> i32 { return 2 }"),
            (
                "middle",
                r#"
                import provider
                export use provider.exposed
            "#,
            ),
        ]),
        main_content: r#"
            import middle
            fun main() -> i32 {
                return middle.exposed()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn private_use_is_still_visible_locally() -> anyhow::Result<()> {
    ImportTestCase {
        name: "private_use_is_still_visible_locally",
        modules: HashMap::from([
            ("provider", "export fun local_use() -> i32 { return 3 }"),
            (
                "middle",
                r#"
                import provider
                use provider.local_use

                export fun wrap() -> i32 {
                    return local_use()
                }
            "#,
            ),
        ]),
        main_content: r#"
            import middle
            fun main() -> i32 {
                return middle.wrap()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

// Generic function defined in module `m` whose body references a symbol brought
// in via a private `use`. When instantiated at a call site in another module,
// monomorphization re-enters `m`'s context — so the private `use` binding must
// still resolve. This guards against regressing the TODO removed from
// `analyze_use` (the old code marked every `use` as Public to keep this path
// working; the new code must rely on same-module private lookup instead).
#[test]
fn import_exported_top_level_const() -> anyhow::Result<()> {
    ImportTestCase {
        name: "exported_top_level_const",
        modules: HashMap::from([(
            "constants",
            r#"
                export const PAGE_SIZE: u64 = 4096
                export const LEAF: u16 = 2
            "#,
        )]),
        main_content: r#"
            import constants
            fun main() -> i32 {
                let page: u64 = constants.PAGE_SIZE
                let kind: u16 = constants.LEAF
                if page != constants.PAGE_SIZE {
                    return 0
                }
                return kind as i32
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_top_level_const_via_use() -> anyhow::Result<()> {
    ImportTestCase {
        name: "top_level_const_via_use",
        modules: HashMap::from([("constants", "export const ANSWER: i32 = 42")]),
        main_content: r#"
            import constants
            use constants.ANSWER
            fun main() -> i32 {
                return ANSWER
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_top_level_const_arithmetic_in_module() -> anyhow::Result<()> {
    ImportTestCase {
        name: "top_level_const_arithmetic_in_module",
        modules: HashMap::from([(
            "layout",
            r#"
                export const BASE: u32 = 2
                export const LEN: u32 = BASE + 2
            "#,
        )]),
        main_content: r#"
            import layout
            fun main() -> i32 {
                return layout.LEN as i32
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_top_level_const_in_array_repeat_count() -> anyhow::Result<()> {
    ImportTestCase {
        name: "top_level_const_in_array_repeat_count",
        modules: HashMap::from([(
            "layout",
            r#"
                export const BASE: u32 = 2
                export const LEN: u32 = BASE + 2
            "#,
        )]),
        main_content: r#"
            import layout
            fun main() -> i32 {
                let xs = [0; layout.LEN]
                return xs[3]
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_private_top_level_const_in_array_repeat_fails() -> anyhow::Result<()> {
    ImportTestCase {
        name: "private_top_level_const_in_array_repeat",
        modules: HashMap::from([("secrets", "const HIDDEN: u32 = 4")]),
        main_content: r#"
            import secrets
            fun main() -> i32 {
                let _ = [0; secrets.HIDDEN]
                return 0
            }
        "#,
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn import_private_top_level_const_fails() -> anyhow::Result<()> {
    ImportTestCase {
        name: "private_top_level_const",
        modules: HashMap::from([("secrets", "const HIDDEN: i32 = 99")]),
        main_content: r#"
            import secrets
            fun main() -> i32 {
                return secrets.HIDDEN
            }
        "#,
        expected_min_top_levels: 0,
        should_fail: true,
    }
    .run()
}

#[test]
fn export_use_reexports_top_level_const() -> anyhow::Result<()> {
    ImportTestCase {
        name: "export_use_top_level_const",
        modules: HashMap::from([
            ("provider", "export const FLAG: u16 = 7"),
            (
                "middle",
                r#"
                import provider
                export use provider.FLAG
            "#,
            ),
        ]),
        main_content: r#"
            import middle
            fun main() -> i32 {
                let flag: u16 = middle.FLAG
                return flag as i32
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn private_use_is_reachable_from_generic_body() -> anyhow::Result<()> {
    ImportTestCase {
        name: "private_use_is_reachable_from_generic_body",
        modules: HashMap::from([
            (
                "helpers",
                "export fun identity_i32(x: i32) -> i32 { return x }",
            ),
            (
                "lib",
                r#"
                import helpers
                use helpers.identity_i32

                export fun wrap<T>(x: T) -> i32 {
                    return identity_i32(11)
                }
            "#,
            ),
        ]),
        main_content: r#"
            import lib
            fun main() -> i32 {
                return lib.wrap<i32>(0)
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}
