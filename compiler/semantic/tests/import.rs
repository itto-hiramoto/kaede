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
        );

        analyzer.analyze(ast, false, false)
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
        modules: HashMap::from([("utils", "pub fn helper(): i32 { return 42 }")]),
        main_content: r#"
            import utils
            fn main(): i32 {
                return utils.helper()
            }
        "#,
        expected_min_top_levels: 2,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_struct_and_impl() -> anyhow::Result<()> {
    ImportTestCase {
        name: "struct_and_impl",
        modules: HashMap::from([(
            "math",
            r#"
                pub struct Point {
                    x: i32,
                    y: i32
                }

                impl Point {
                    pub fn new(x: i32, y: i32): Point {
                        return Point { x: x, y: y }
                    }

                    pub fn add(self, other: Point): Point {
                        return Point { x: self.x + other.x, y: self.y + other.y }
                    }

                    pub fn get_x(self): i32 {
                        return self.x
                    }

                    pub fn get_y(self): i32 {
                        return self.y
                    }
                }
            "#,
        )]),
        main_content: r#"
            import math
            fn main(): i32 {
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
fn import_nested_module() -> anyhow::Result<()> {
    ImportTestCase {
        name: "nested_module",
        modules: HashMap::from([(
            "nested/deep/module",
            "pub fn deep_function(): i32 { return 999 }",
        )]),
        main_content: r#"
            import nested.deep.module
            fn main(): i32 {
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
            ("utils", "pub fn util_func(): i32 { return 1 }"),
            ("math", "pub fn math_func(): i32 { return 2 }"),
        ]),
        main_content: r#"
            import utils
            import math
            fn main(): i32 {
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
        modules: HashMap::from([("utils", "pub fn helper(): i32 { return 42 }")]),
        main_content: r#"
            import utils
            import utils  // Duplicate import should be handled gracefully
            fn main(): i32 {
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
            ("math", "pub fn add(a: i32, b: i32): i32 { return a + b }"),
            (
                "utils",
                r#"
                import math
                pub fn double_add(a: i32, b: i32): i32 {
                    return math.add(a, b) * 2
                }
            "#,
            ),
        ]),
        main_content: r#"
            import utils
            fn main(): i32 {
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
                pub struct Container<T> {
                    value: T
                }

                impl<T> Container<T> {
                    pub fn new(value: T): Container<T> {
                        return Container<T> { value: value }
                    }

                    pub fn get(self): T {
                        return self.value
                    }
                }
            "#,
        )]),
        main_content: r#"
            import container
            fn main(): i32 {
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
                pub enum Color {
                    Red,
                    Green,
                    Blue
                }

                pub enum Opt<T> {
                    Some(T),
                    None
                }

                pub fn get_red(): Color {
                    return Color::Red
                }
            "#,
        )]),
        main_content: r#"
            import types
            fn main(): i32 {
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
                pub struct Circle {
                    radius: i32
                }

                pub fn get_radius(c: Circle): i32 {
                    return c.radius
                }
            "#,
        )]),
        main_content: r#"
            import graphics.shapes
            use graphics.shapes.Circle
            use graphics.shapes.get_radius

            fn main(): i32 {
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
            fn main(): i32 {
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
        main_content: "import empty\nfn main(): i32 { return 0 }",
        expected_min_top_levels: 1,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_module_with_only_private_items() -> anyhow::Result<()> {
    ImportTestCase {
        name: "module_with_only_private_items",
        modules: HashMap::from([("private", "fn private_func(): i32 { return 42 }")]),
        main_content: "import private\nfn main(): i32 { return 0 }",
        expected_min_top_levels: 1,
        should_fail: false,
    }
    .run()
}

#[test]
fn import_private_items() -> anyhow::Result<()> {
    ImportTestCase {
        name: "import_private_items",
        modules: HashMap::from([("private", "fn private_func(): i32 { return 42 }")]),
        main_content: "import private\nfn main(): i32 { return private.private_func() }",
        expected_min_top_levels: 1,
        should_fail: true,
    }
    .run()
}
