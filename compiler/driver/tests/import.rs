//! Testing under the assumption that `lli` is installed!

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::{path::Path, process::Command};

fn test(expect: i32, file_paths: &[&Path], root_dir: &Path) -> anyhow::Result<()> {
    let exe = assert_fs::NamedTempFile::new("a.out")?;

    let mut args = file_paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<String>>();

    args.push("-o".to_string());
    args.push(exe.path().to_string_lossy().to_string());

    args.push("--root-dir".to_string());
    args.push(root_dir.to_string_lossy().to_string());

    let compile_output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    Command::new(format!("{}", exe.path().to_string_lossy()))
        .assert()
        .code(predicate::eq(expect));

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
fn import_generic_struct_methods() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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

    test(58, &[module.path(), main.path()], &tempdir).unwrap();
}

#[test]
fn import_generic_enum_methods_with_arg_of_self_type() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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

    test(58, &[module.path(), main.path()], &tempdir).unwrap();
}

#[test]
fn import_generic_methods_with_arg_of_self_type() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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

    test(58, &[module.path(), main.path()], &tempdir).unwrap();
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
fn import_same_name_module_with_same_name_struct() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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
            let apple = m.Apple::new_ringo(58)
            let apple2 = Apple { width: 48, height: 10 }

            if apple.get_size() == apple2.get() {
                return apple.size
            }
            return 123
        }"#,
        )
        .unwrap();

    test(58, &[module1.path(), module2.path()], &tempdir).unwrap();
}

#[test]
fn import_generic_struct_from_module_in_directory() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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
            let apple = m.Apple<i32>::new(58)
            return apple.get_size()
        }"#,
        )
        .unwrap();

    test(58, &[module1.path(), module2.path()], &tempdir).unwrap();
}

#[test]
fn import_function_with_non_public_generic_struct() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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

    test(58, &[module1.path(), module2.path()], &tempdir).unwrap();
}

#[test]
fn import_generic_struct_and_impl_with_use_declaration() {
    let tempdir = assert_fs::TempDir::new().unwrap();

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
            use m.Apple

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
    .unwrap();
}
