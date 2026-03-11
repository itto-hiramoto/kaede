//! Testing under the assumption that `lli` is installed!

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::{fs, path::Path, process::Command};

fn compile(
    file_paths: &[&Path],
    root_dir: &Path,
    output_path: &Path,
) -> anyhow::Result<std::process::Output> {
    let mut args = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<String>>();

    args.push("-o".to_string());
    args.push(output_path.to_string_lossy().to_string());

    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    Ok(Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .output()?)
}

fn compile_project(
    file_paths: &[&Path],
    root_dir: &Path,
) -> anyhow::Result<(assert_fs::NamedTempFile, std::process::Output)> {
    let exe = assert_fs::NamedTempFile::new("a.out")?;
    let output = compile(file_paths, root_dir, exe.path())?;

    assert!(
        output.status.success(),
        "kaede compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    Ok((exe, output))
}

fn run_compiled_binary(expect: i32, exe_path: &Path) -> anyhow::Result<()> {
    Command::new(exe_path).assert().code(predicate::eq(expect));
    Ok(())
}

fn test(expect: i32, file_paths: &[&Path], root_dir: &Path) -> anyhow::Result<()> {
    let (exe, _) = compile_project(file_paths, root_dir)?;

    run_compiled_binary(expect, exe.path())?;

    Ok(())
}

fn write_rust_import_crate(root_dir: &Path, crate_name: &str, lib_src: &str) -> anyhow::Result<()> {
    let rust_dir = root_dir.join("rust");
    fs::create_dir_all(rust_dir.join("src"))?;
    fs::write(
        rust_dir.join("Cargo.toml"),
        format!("[package]\nname = \"{crate_name}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n"),
    )?;
    fs::write(rust_dir.join("src").join("lib.rs"), lib_src)?;

    Ok(())
}

#[test]
fn import_functions() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str("pub fn f(): i32 { return 48 }")?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str("pub fn f(): i32 { return 10 }")?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        import m2
        fn main(): i32 {
            return m1.f() + m2.f()
        }"#,
    )?;

    test(58, &[module1.path(), module2.path(), main.path()], &tempdir)
}

#[test]
fn import_i32_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"impl i32 {
            pub fn add(self, other: i32): i32 {
                return self + other
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            return 48.add(10)
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_i32_method_returning_imported_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Wrap {
            value: i32,
        }

        impl i32 {
            pub fn to_wrap(self): Wrap {
                return Wrap { value: self }
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let w = 58.to_wrap()
            return w.value
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_struct_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub struct Apple {
            size: i32,
        }
        impl Apple {
            pub fn new(size: i32): Apple {
                return Apple { size: size }
            }

            pub fn is_orange(self): bool {
                return false
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"pub struct Ichigo {
            size: i32,
        }
        impl Ichigo {
            pub fn get_size(self): i32 {
                return self.size
            }
        }
        "#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        import m2
        fn main(): i32 {
            let apple = m1.Apple::new(48);
            let ichigo = m2.Ichigo { size: 10 }
            if !apple.is_orange() {
                return apple.size + ichigo.get_size()
            }
            return 123
        }"#,
    )?;

    test(58, &[module1.path(), module2.path(), main.path()], &tempdir)
}

#[test]
fn import_struct_methods_with_arg_of_self_type() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }
        impl Apple {
            pub fn new(size: i32): Apple {
                return Apple { size: size }
            }

            pub fn equals(self, other: Apple): bool {
                return self.size == other.size
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Apple::new(48);
            let ichigo = m.Apple::new(10);
            if !apple.equals(ichigo) {
                return apple.size + ichigo.size
            }
            return 123
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_struct_methods_with_name_conflict() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m1.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        struct Apple {
            size: i32,
        }
        fn main(): i32 {
            let apple1 = m1.Apple { size: 48 };
            let apple2 = Apple { size: 10 }
            return apple1.size + apple2.size
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn imported_typed_member() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        struct Fruit {
            apple: m.Apple,
        }
        fn main(): i32 {
            let fruit = Fruit { apple: m.Apple { size: 58 } }
            return fruit.apple.size
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub enum Fruit {
            Apple(i32),
            Orange,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(58);

            match apple {
                m.Fruit::Apple(value) => {
                    return value
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_enum_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub enum Fruit {
            Apple(i32),
            Orange,
        }
        impl Fruit {
            pub fn new(s: i32): mut Fruit {
                return Fruit::Apple(s)
            }
            pub fn get(self): i32 {
                return match self {
                    Fruit::Apple(s) => s,
                    Fruit::Orange => 123
                }
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::new(58)
            return apple.get()
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_complex_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(m.Apple { size: 48 });
            let ichigo = m.Fruit::Ichigo(10);

            match apple {
                m.Fruit::Apple(a) => {
                    match ichigo {
                        m.Fruit::Ichigo(i) => {
                            return a.size + i
                        },
                        _ => return 123,
                    }
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn enum_with_imported_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }
        impl Apple {
            pub fn get_size(self): i32 {
                return self.size
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        enum Fruit {
            Apple(m.Apple),
            Ichigo(i32),
        }

        fn main(): i32 {
            let apple = Fruit::Apple(m.Apple { size: 48 });
            let ichigo = Fruit::Ichigo(10);

            match apple {
                Fruit::Apple(a) => {
                    match ichigo {
                        Fruit::Ichigo(i) => {
                            return a.get_size() + i
                        },
                        _ => return 123,
                    }
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_enum_and_call_variant_method() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        impl Apple {
            pub fn get_size(self): i32 {
                return self.size
            }
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(m.Apple { size: 58 });

            match apple {
                m.Fruit::Apple(a) => {
                    return a.get_size()
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn nested_import() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let m1 = tempdir.child("m1.kd");
    m1.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let m2 = tempdir.child("m2.kd");
    m2.write_str(
        r#"import m1
        pub fn get_value(fruit: m1.Fruit): i32 {
            return match fruit {
                m1.Fruit::Apple(a) => a.size,
                m1.Fruit::Ichigo(n) => n,
            }
        }
        pub fn f(): i32 {
            let fruit = m1.Fruit::Apple(m1.Apple { size: 48 });
            return get_value(fruit)
        }
        pub fn g(): i32 {
            let fruit = m1.Fruit::Ichigo(10);
            return get_value(fruit)
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m2
        fn main(): i32 {
            return m2.f() + m2.g()
        }"#,
    )?;

    test(58, &[m1.path(), m2.path(), main.path()], &tempdir)
}

#[test]
fn import_generic_struct_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module
        .write_str(
            r#"pub struct Apple<T> {
            height: T,
            width: T,
        }

        impl<T> Apple<T> {
            pub fn new(height: T, width: T): mut Apple<T> {
                return Apple<T> { height: height, width: width }
            }

            pub fn set_height(mut self, height: T) {
                self.height = height
            }

            pub fn set_width(mut self, width: T) {
                self.width = width
            }

            pub fn get_height(self): i32 {
                return self.height
            }

            pub fn get_width(self): i32 {
                return self.width
            }
        }"#,
        )
        .unwrap();

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let mut apple = m.Apple<i32>::new(123, 456)
            apple.set_height(48)
            apple.set_width(10)
            return apple.get_height() + apple.get_width()
        }"#,
    )
    .unwrap();

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_generic_enum_methods_with_arg_of_self_type() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module
        .write_str(
            r#"pub enum Apple<T> {
            Ringo(i32),
            Budo,
        }

        impl<T> Apple<T> {
            pub fn new_ringo(size: T): mut Apple<T> {
                return Apple<T>::Ringo(size)
            }

            pub fn add(self, other: Apple<T>): T {
                let self_size = match self {
                    Apple::Ringo(s) => s,
                    Apple::Budo => 123,
                }

                return match other {
                    Apple::Ringo(s) => self_size + s,
                    Apple::Budo => self_size + 123,
                }
            }
        }"#,
        )
        .unwrap();

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Apple<i32>::new_ringo(48)
            return apple.add(m.Apple<i32>::new_ringo(10))
        }"#,
    )
    .unwrap();

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_generic_methods_with_arg_of_self_type() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module
        .write_str(
            r#"pub struct Apple<T> {
            height: T,
            width: T,
        }

        impl<T> Apple<T> {
            pub fn new(height: T, width: T): mut Apple<T> {
                return Apple<T> { height: height, width: width }
            }

            pub fn get(self): T {
                return self.height + self.width
            }

            pub fn equals(self, other: Apple<T>): bool {
                return self.height == other.height && self.width == other.width
            }
        }"#,
        )
        .unwrap();

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Apple<i32>::new(48, 10)
            if apple.equals(m.Apple<i32>::new(48, 10)) {
                return apple.get()
            }
            return 123
        }"#,
    )
    .unwrap();

    test(58, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_function_with_arg_of_external_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        pub fn get_size(apple: Apple): i32 {
            return apple.size
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import m1
        fn main(): i32 {
            return m1.get_size(m1.Apple { size: 58 })
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_function_with_arg_of_external_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub enum Apple {
            Ringo(i32),
            Budo,
        }

        pub fn get_size(apple: Apple): i32 {
            return match apple {
                Apple::Ringo(s) => s,
                Apple::Budo => 123,
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import m1
        fn main(): i32 {
            return m1.get_size(m1.Apple::Ringo(58))
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn use_declaration() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub fn get_58(): i32 {
            return 58
        }

        pub struct Apple {
            size: i32,
        }

        impl Apple {
            pub fn new(size: i32): Apple {
                return Apple { size: size }
            }

            pub fn get_size(self): i32 {
                return self.size
            }
        }

        pub enum Fruit {
            Ringo(Apple)
        }

        impl Fruit {
            pub fn get_size(self): i32 {
                return match self {
                    Fruit::Ringo(a) => a.get_size()
                }
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import m1
        use m1.Apple
        use m1.get_58
        use m1.Fruit
        fn main(): i32 {
            let apple = Apple::new(58)
            let fruit = Fruit::Ringo(apple)
            let size = fruit.get_size()
            if size == get_58() && size == apple.get_size() {
                return size
            }
            return 123
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn use_declaration_with_generic_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub struct Apple<T> {
            size: T,
        }

        impl<T> Apple<T> {
            pub fn new(size: T): Apple<T> {
                return Apple<T> { size: size }
            }

            pub fn get_size(self): T {
                return self.size
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import m1
        use m1.Apple
        fn main(): i32 {
            let apple = Apple<i32>::new(58)
            return apple.get_size()
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn use_declaration_with_generic_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub enum Apple<T> {
            Ringo(T),
            Budo,
        }

        impl<T> Apple<T> {
            pub fn new_ringo(size: T): Apple<T> {
                return Apple<T>::Ringo(size)
            }

            pub fn get_size(self): T {
                return match self {
                    Apple::Ringo(s) => s,
                    Apple::Budo => 123,
                }
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import m1
        use m1.Apple
        fn main(): i32 {
            let apple = Apple<i32>::new_ringo(58)
            return apple.get_size()
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_module_in_directory() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("dir/m1.kd");
    module1.write_str(
        "pub struct Apple { size: i32 }
        pub enum Fruit { Ringo(Apple), Ichigo(i32) }
        pub fn get_58(): i32 { return 58 }",
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"import dir.m1
        use dir.m1.Fruit
        fn main(): i32 {
            let apple = Fruit::Ringo(dir.m1.Apple { size: 58 });
            let n = match apple {
                Fruit::Ringo(a) => a.size,
                Fruit::Ichigo(n) => n,
            }
            if n == dir.m1.get_58() {
                return n
            }
            return 123
        }"#,
    )?;

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_hierarchical_module() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("dir/dir2/m1.kd");
    module1.write_str(
        "pub struct Apple {
            size: i32,
        }",
    )?;

    let module2 = tempdir.child("dir/dir2/m2.kd");
    module2.write_str(
        "import m1
        pub enum Fruit { Ringo(m1.Apple), Ichigo(i32) }
        pub fn get_48(): i32 { return 48 }
        pub fn get_10(): i32 { return 10 }",
    )?;

    let module3 = tempdir.child("dir/dir2/m3.kd");
    module3.write_str(
        "import m2
        pub fn get_58(): i32 { return m2.get_48() + m2.get_10() }",
    )?;

    let module4 = tempdir.child("dir/m.kd");
    module4.write_str(
        r#"import dir2.m3
        pub fn f(): i32 {
            return dir2.m3.get_58()
        }"#,
    )?;

    let module5 = tempdir.child("m2.kd");
    module5.write_str(
        r#"import dir.dir2.m1
        import dir.dir2.m2
        import dir.dir2.m3
        import dir.m
        use dir.dir2.m2.Fruit
        fn main(): i32 {
            let apple = Fruit::Ringo(dir.dir2.m1.Apple { size: 58 });
            let n = match apple {
                Fruit::Ringo(a) => a.size,
                Fruit::Ichigo(n) => n,
            }
            if n == dir.dir2.m3.get_58() {
                return dir.m.f()
            }
            return 123
        }"#,
    )?;

    test(
        58,
        &[
            module1.path(),
            module2.path(),
            module3.path(),
            module4.path(),
            module5.path(),
        ],
        &tempdir,
    )
}

#[test]
fn import_same_name_module_with_same_name_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("dir/m.kd");
    module1
        .write_str(
            r#"pub struct Apple {
            size: i32,
        }

        impl Apple {
            pub fn new_ringo(size: i32): Apple {
                return Apple { size: 58 }
            }
            pub fn get_size(self): i32 {
                return self.size
            }
        }"#,
        )
        .unwrap();

    let module2 = tempdir.child("m.kd");
    module2
        .write_str(
            r#"import dir.m
        struct Apple {
            width: i32,
            height: i32,
        }

        impl Apple {
            fn get(self): i32 {
                return self.width + self.height
            }
        }

        fn main(): i32 {
            let apple = dir.m.Apple::new_ringo(58)
            let apple2 = Apple { width: 48, height: 10 }

            if apple.get_size() == apple2.get() {
                return apple.size
            }
            return 123
        }"#,
        )
        .unwrap();

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_generic_struct_from_module_in_directory() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("dir/m.kd");
    module1
        .write_str(
            r#"pub struct Apple<T> {
            size: T,
        }

        impl<T> Apple<T> {
            pub fn new(size: T): Apple<T> {
                return Apple<T> { size: size }
            }

            pub fn get_size(self): T {
                return self.size
            }
        }"#,
        )
        .unwrap();

    let module2 = tempdir.child("m.kd");
    module2
        .write_str(
            r#"import dir.m
        fn main(): i32 {
            let apple = dir.m.Apple<i32>::new(58)
            return apple.get_size()
        }"#,
        )
        .unwrap();

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_function_with_non_public_generic_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1
        .write_str(
            r#"struct Apple<T> {
            size: i32,
        }

        impl<T> Apple<T> {
            fn new(size: T): Apple {
                return Apple { size: size }
            }
        }

        pub fn get_58(): i32 {
            return 58
        }"#,
        )
        .unwrap();

    let module2 = tempdir.child("m2.kd");
    module2
        .write_str(
            r#"import m1
        fn main(): i32 {
            return m1.get_58()
        }"#,
        )
        .unwrap();

    test(58, &[module1.path(), module2.path()], &tempdir)
}

#[test]
fn import_generic_struct_and_impl_with_use_declaration() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("dir/m.kd");
    module1
        .write_str(
            r#"pub enum Apple<T> {
                Ringo(T),
                Mango,
            }
            impl<T> Apple<T> {
                pub fn get_size(self): T {
                    return match self {
                        Apple::Ringo(s) => s,
                        Apple::Mango => 123,
                    }
                }
            }"#,
        )
        .unwrap();

    let module2 = tempdir.child("m2.kd");
    module2
        .write_str(
            r#"import dir.m
            use dir.m.Apple

            pub struct Test<T> {
                age: Apple<T>,
            }

            impl<T> Test<T> {
                pub fn new(): Test<T> {
                    let apple = Apple<T>::Ringo(58)
                    return Test<T> { age: apple }
                }

                pub fn get_age(self): Apple<T> {
                    return self.age
                }
            }"#,
        )
        .unwrap();

    let module3 = tempdir.child("m3.kd");
    module3
        .write_str(
            r#"import m2
            fn main(): i32 {
                let test = m2.Test<i32>::new()
                let age = test.get_age()
                return age.get_size()
            }"#,
        )
        .unwrap();

    test(
        58,
        &[module1.path(), module2.path(), module3.path()],
        &tempdir,
    )
}

#[test]
fn import_function_returning_external_struct_with_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    // m2.kd - defines a public struct with methods
    let m2 = tempdir.child("m2.kd");
    m2.write_str(
        r#"pub struct Person {
            age: i32
        }

        impl Person {
            pub fn get_age(self): i32 {
                return self.age
            }
        }"#,
    )?;

    // m1.kd - imports m2, uses Person in public function signature
    let m1 = tempdir.child("m1.kd");
    m1.write_str(
        r#"import m2
        use m2.Person

        pub fn create_person(): Person {
            return Person { age: 58 }
        }"#,
    )?;

    // test.kd - imports m1, calls function that returns Person, then calls method on it
    let test_m = tempdir.child("test.kd");
    test_m.write_str(
        r#"import m1

        fn main(): i32 {
            let p = m1.create_person()
            return p.get_age()
        }"#,
    )?;

    test(58, &[m2.path(), m1.path(), test_m.path()], &tempdir)
}

#[test]
fn import_generic_symbol_multiply_defined_linkonce_odr() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    // m2.kd - defines a generic enum with implementation
    let m2 = tempdir.child("m2.kd");
    m2.write_str(
        r#"pub enum Person<T> {
            Bob(T),
            Alice,
        }

        impl<T> Person<T> {
            pub fn get_age(self): T {
                return match self {
                    Person::Bob(n) => n,
                    Person::Alice => 3,
                }
            }
        }"#,
    )?;

    // m1.kd - imports m2, uses Person<i32> which generates generic instantiation
    let m1 = tempdir.child("m1.kd");
    m1.write_str(
        r#"import m2
        use m2.Person

        pub fn create_person(): Person<i32> {
            return Person<i32>::Bob(58)
        }"#,
    )?;

    // test.kd - imports m1, also uses Person<i32> which could generate the same symbols
    let test_m = tempdir.child("test.kd");
    test_m.write_str(
        r#"import m1

        fn main(): i32 {
            let p = m1.create_person()
            return p.get_age()
        }"#,
    )?;

    test(58, &[m2.path(), m1.path(), test_m.path()], &tempdir)
}

#[test]
fn import_extern_c() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let extern_c = tempdir.child("extern_c.kd");
    extern_c.write_str(r#"pub extern "C" fn puts(s: *i8): i32"#)?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"import extern_c
        fn main(): i32 {
            return extern_c.puts("Hello, world!".as_ptr())
        }"#,
    )?;

    test(14, &[extern_c.path(), module.path()], &tempdir)
}

#[test]
fn import_and_use_with_wildcard() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        impl Apple {
            pub fn get_size(self): i32 {
                return self.size
            }
        }

        pub fn f(): Apple {
            return Apple { size: 10 }
        }

        pub fn g(a: Apple): i32 {
            return a.get_size()
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        use m.*
        fn main(): i32 {
            let apple = Apple { size: 20 }
            return f().get_size() + g(apple)
        }"#,
    )?;

    test(30, &[module.path(), main.path()], &tempdir)
}

#[test]
fn call_imported_function_with_local_variable() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub fn f(n: i32): i32 {
            return n
        }"#,
    )?;

    let main = tempdir.child("test.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let n = 10
            return m.f(n)
        }"#,
    )?;

    test(10, &[module.path(), main.path()], &tempdir)
}

#[test]
fn import_rust_functions_with_primitive_params_and_returns() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "primitive_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn add_i8(a: i8, b: i8) -> i8 { a + b }
pub fn add_u8(a: u8, b: u8) -> u8 { a + b }
pub fn add_i16(a: i16, b: i16) -> i16 { a + b }
pub fn add_u16(a: u16, b: u16) -> u16 { a + b }
pub fn add_i32(a: i32, b: i32) -> i32 { a + b }
pub fn add_u32(a: u32, b: u32) -> u32 { a + b }
pub fn add_i64(a: i64, b: i64) -> i64 { a + b }
pub fn add_u64(a: u64, b: u64) -> u64 { a + b }
pub fn invert(v: bool) -> bool { !v }
pub fn touch() {}"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::primitive_probe

        fn main(): i32 {
            rust::primitive_probe::touch()

            if rust::primitive_probe::add_i8(20 as i8, 22 as i8) != (42 as i8) {
                return 1
            }
            if rust::primitive_probe::add_u8(20 as u8, 22 as u8) != (42 as u8) {
                return 2
            }
            if rust::primitive_probe::add_i16(20 as i16, 22 as i16) != (42 as i16) {
                return 3
            }
            if rust::primitive_probe::add_u16(20 as u16, 22 as u16) != (42 as u16) {
                return 4
            }
            if rust::primitive_probe::add_i32(20, 22) != 42 {
                return 5
            }
            if rust::primitive_probe::add_u32(20 as u32, 22 as u32) != (42 as u32) {
                return 6
            }
            if rust::primitive_probe::add_i64(20 as i64, 22 as i64) != (42 as i64) {
                return 7
            }
            if rust::primitive_probe::add_u64(20 as u64, 22 as u64) != (42 as u64) {
                return 8
            }
            if !rust::primitive_probe::invert(false) {
                return 9
            }

            return 58
        }"#,
    )?;

    test(58, &[main.path()], &tempdir)
}

#[test]
fn import_rust_function_with_str_param() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "str_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn strlen(s: &str) -> i32 { s.len() as i32 }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::str_probe

        fn main(): i32 {
            return rust::str_probe::strlen("hello")
        }"#,
    )?;

    test(5, &[main.path()], &tempdir)
}

#[test]
fn import_rust_function_with_multiple_str_params() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "multi_str_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn total_len(a: &str, b: &str) -> i32 { (a.len() + b.len()) as i32 }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::multi_str_probe

        fn main(): i32 {
            return rust::multi_str_probe::total_len("kae", "de")
        }"#,
    )?;

    test(5, &[main.path()], &tempdir)
}

#[test]
fn import_rust_function_with_str_and_primitives() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "mixed_str_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn starts_with_len(s: &str, n: i32) -> bool { s.len() == n as usize }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::mixed_str_probe

        fn main(): i32 {
            if rust::mixed_str_probe::starts_with_len("kaede", 5) {
                return 58
            }
            return 0
        }"#,
    )?;

    test(58, &[main.path()], &tempdir)
}

#[test]
fn import_rust_skips_str_return_but_keeps_supported_ones() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "skip_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn strlen(v: &str) -> i32 { v.len() as i32 }
pub fn ok_i64(v: i64) -> i64 { v }
pub fn ng_char(v: char) -> char { v }
pub fn ng_str(v: &str) -> &str { v }
pub fn ng_ptr(v: *const i8) -> *const i8 { v }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::skip_probe

        fn main(): i32 {
            return rust::skip_probe::ok_i64(53 as i64) as i32 + rust::skip_probe::strlen("kaede")
        }"#,
    )?;

    let (exe, output) = compile_project(&[main.path()], tempdir.path())?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains("warning: skipped rust function in `skip_probe`: ng_char"));
    assert!(stderr.contains("unsupported primitive type `char`"));
    assert!(stderr.contains("warning: skipped rust function in `skip_probe`: ng_str"));
    assert!(stderr
        .contains("unsupported return type: borrowed reference type `&str` is not supported yet"));
    assert!(stderr.contains("warning: skipped rust function in `skip_probe`: ng_ptr"));
    assert!(stderr.contains("unsupported raw pointer type `*const i8`"));

    run_compiled_binary(58, exe.path())
}

#[test]
fn import_rust_skips_mut_str_param_but_keeps_supported_ones() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;
    let crate_name = "mut_str_probe";

    write_rust_import_crate(
        tempdir.path(),
        crate_name,
        r#"pub fn ok(s: &str) -> i32 { s.len() as i32 }
pub fn normalize(s: &mut str) -> i32 { s.len() as i32 }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import rust::mut_str_probe

        fn main(): i32 {
            return rust::mut_str_probe::ok("kaede")
        }"#,
    )?;

    let (exe, output) = compile_project(&[main.path()], tempdir.path())?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains("warning: skipped rust function in `mut_str_probe`: normalize"));
    assert!(stderr.contains("unsupported mutable borrowed string type `&mut str`"));

    run_compiled_binary(5, exe.path())
}
