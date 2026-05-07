use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use anyhow::{anyhow, Context as _};
use kaede_common::lib_extension;
use kaede_ir::{module_path::ModulePath, qualified_symbol::QualifiedSymbol, ty as ir_type};
use kaede_symbol::Symbol;
use rustdoc_types::{
    Crate, FunctionSignature, Id, Item, ItemEnum, ItemKind, ItemSummary, Type, Visibility,
};
use toml::Value as TomlValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TyPosition {
    Param,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ShimTyPosition {
    Param,
    Return,
}

#[derive(Debug, Clone)]
pub struct RustImportedFn {
    pub kaede_name: Symbol,
    pub export_name: Symbol,
    pub params: Vec<(Symbol, Rc<ir_type::Ty>)>,
    pub return_ty: Rc<ir_type::Ty>,
}

#[derive(Debug, Clone)]
pub struct RustImportOutput {
    pub module_path: ModulePath,
    pub functions: Vec<RustImportedFn>,
    pub dylib_path: Option<PathBuf>,
    pub skipped: Vec<String>,
}

/// Converts arbitrary names from rustdoc/Cargo metadata into Rust identifier-safe text.
///
/// Cargo package names may contain `-` (e.g. `my-crate`), and rustdoc can also surface names
/// that are not directly usable as Rust identifiers in generated shim code.
/// The shim uses these names for generated Rust function/parameter symbols, so we normalize
/// non `[A-Za-z0-9_]` characters to `_`.
fn sanitize_symbol(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    out
}

fn fundamental_kind_from_rustdoc_primitive(name: &str) -> Option<ir_type::FundamentalTypeKind> {
    match name {
        "i8" => Some(ir_type::FundamentalTypeKind::I8),
        "u8" => Some(ir_type::FundamentalTypeKind::U8),
        "i16" => Some(ir_type::FundamentalTypeKind::I16),
        "u16" => Some(ir_type::FundamentalTypeKind::U16),
        "i32" => Some(ir_type::FundamentalTypeKind::I32),
        "u32" => Some(ir_type::FundamentalTypeKind::U32),
        "i64" => Some(ir_type::FundamentalTypeKind::I64),
        "u64" => Some(ir_type::FundamentalTypeKind::U64),
        "f32" => Some(ir_type::FundamentalTypeKind::F32),
        "f64" => Some(ir_type::FundamentalTypeKind::F64),
        "bool" => Some(ir_type::FundamentalTypeKind::Bool),
        _ => None,
    }
}

fn rustdoc_ty_description(ty: &Type) -> String {
    match ty {
        Type::Primitive(name) => name.clone(),
        Type::Tuple(elems) if elems.is_empty() => "()".to_string(),
        Type::Tuple(_) => "tuple".to_string(),
        Type::BorrowedRef {
            is_mutable, type_, ..
        } => {
            let prefix = if *is_mutable { "&mut " } else { "&" };
            format!("{prefix}{}", rustdoc_ty_description(type_))
        }
        Type::RawPointer { is_mutable, type_ } => {
            let prefix = if *is_mutable { "*mut " } else { "*const " };
            format!("{prefix}{}", rustdoc_ty_description(type_))
        }
        Type::Slice(_) => "[_]".to_string(),
        Type::Array { type_, len } => format!("[{}; {len}]", rustdoc_ty_description(type_)),
        Type::ResolvedPath(path) => path.path.clone(),
        _ => "unknown".to_string(),
    }
}

fn ty_is_empty_tuple(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(elems) if elems.is_empty())
}

fn kaede_string_qualified_symbol() -> QualifiedSymbol {
    QualifiedSymbol::new(
        ModulePath::new(vec![
            Symbol::from("std".to_string()),
            Symbol::from("string".to_string()),
        ]),
        Symbol::from("String".to_string()),
    )
}

fn kaede_string_ir_ty() -> Rc<ir_type::Ty> {
    Rc::new(ir_type::wrap_in_ref(
        Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::UserDefined(ir_type::UserDefinedType::new(
                ir_type::UserDefinedTypeKind::Placeholder(kaede_string_qualified_symbol()),
            ))
            .into(),
            mutability: ir_type::Mutability::Not,
        }),
        ir_type::Mutability::Not,
    ))
}

fn kaede_str_ir_ty() -> Rc<ir_type::Ty> {
    Rc::new(ir_type::wrap_in_ref(
        Rc::new(ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::Str,
            ir_type::Mutability::Not,
        )),
        ir_type::Mutability::Not,
    ))
}

fn is_kaede_str_ty(ty: &ir_type::Ty) -> bool {
    ty.is_str()
}

fn is_kaede_string_ty(ty: &ir_type::Ty) -> bool {
    matches!(
        ty.kind.as_ref(),
        ir_type::TyKind::Reference(rty)
            if matches!(
                rty.get_base_type().kind.as_ref(),
                ir_type::TyKind::UserDefined(udt)
                    if udt.qualified_symbol() == kaede_string_qualified_symbol()
            )
    )
}

fn is_kaede_char_ty(ty: &ir_type::Ty) -> bool {
    matches!(
        ty.kind.as_ref(),
        ir_type::TyKind::Fundamental(fty) if fty.kind == ir_type::FundamentalTypeKind::Char
    )
}

fn resolved_path_segments<'a>(
    ty: &Type,
    paths: Option<&'a HashMap<Id, ItemSummary>>,
) -> Option<&'a [String]> {
    let Type::ResolvedPath(path) = ty else {
        return None;
    };
    let summary = paths?.get(&path.id)?;
    Some(summary.path.as_slice())
}

fn is_rust_string_resolved_path(ty: &Type, paths: Option<&HashMap<Id, ItemSummary>>) -> bool {
    let Some(segments) = resolved_path_segments(ty, paths) else {
        return false;
    };
    segments == ["alloc", "string", "String"] || segments == ["std", "string", "String"]
}

fn parse_borrowed_ref_ty(ty: &Type, position: TyPosition) -> Result<Rc<ir_type::Ty>, String> {
    let Type::BorrowedRef {
        is_mutable, type_, ..
    } = ty
    else {
        return Err(format!(
            "unsupported borrowed reference type `{}`",
            rustdoc_ty_description(ty)
        ));
    };

    if matches!(type_.as_ref(), Type::Primitive(p) if p == "str") {
        if *is_mutable {
            return Err("unsupported mutable borrowed string type `&mut str`".to_string());
        }
        return match position {
            TyPosition::Param => Ok(kaede_str_ir_ty()),
            TyPosition::Return => Ok(kaede_string_ir_ty()),
        };
    }

    Err(format!(
        "unsupported borrowed reference type `{}`",
        rustdoc_ty_description(ty)
    ))
}

fn parse_supported_ty(
    ty: &Type,
    position: TyPosition,
    paths: Option<&HashMap<Id, ItemSummary>>,
) -> Result<Rc<ir_type::Ty>, String> {
    match ty {
        Type::Primitive(name) if name == "char" => Ok(Rc::new(ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::Char,
            ir_type::Mutability::Not,
        ))),
        Type::Primitive(name) => fundamental_kind_from_rustdoc_primitive(name)
            .map(|kind| Rc::new(ir_type::make_fundamental_type(kind, ir_type::Mutability::Not)))
            .ok_or_else(|| format!("unsupported primitive type `{name}`")),
        Type::Tuple(elems) if elems.is_empty() => Ok(Rc::new(ir_type::Ty::new_unit())),
        Type::BorrowedRef { .. } => parse_borrowed_ref_ty(ty, position),
        Type::ResolvedPath(_) if is_rust_string_resolved_path(ty, paths) => {
            Ok(kaede_string_ir_ty())
        }
        Type::RawPointer { .. } => Err(format!(
            "unsupported raw pointer type `{}`",
            rustdoc_ty_description(ty)
        )),
        _ => Err(format!(
            "unsupported non-primitive type `{}`",
            rustdoc_ty_description(ty)
        )),
    }
}

fn project_root_from_root_dir(root_dir: &Path, rust_subdir: &Path) -> anyhow::Result<PathBuf> {
    let direct = root_dir.join(rust_subdir).join("Cargo.toml");
    if direct.exists() {
        return Ok(root_dir.to_path_buf());
    }

    let parent = root_dir
        .parent()
        .ok_or_else(|| anyhow!("failed to resolve project root from {}", root_dir.display()))?;
    let parent_manifest = parent.join(rust_subdir).join("Cargo.toml");
    if parent_manifest.exists() {
        return Ok(parent.to_path_buf());
    }

    Err(anyhow!(
        "{}/Cargo.toml not found (searched '{}' and '{}')",
        rust_subdir.display(),
        direct.display(),
        parent_manifest.display()
    ))
}

fn run_command(mut cmd: Command, name: &str) -> anyhow::Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("failed to run `{name}`"))?;
    if status.success() {
        return Ok(());
    }

    Err(anyhow!("`{name}` failed with status {}", status))
}

fn read_package_name(manifest_path: &Path) -> anyhow::Result<String> {
    let content = fs::read_to_string(manifest_path)
        .with_context(|| format!("failed to read {}", manifest_path.display()))?;
    let toml = content.parse::<TomlValue>().with_context(|| {
        format!(
            "failed to parse Cargo.toml at {}",
            manifest_path.to_string_lossy()
        )
    })?;

    toml.get("package")
        .and_then(|v| v.get("name"))
        .and_then(TomlValue::as_str)
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("package.name not found in {}", manifest_path.display()))
}

fn collect_candidate_ids(krate: &Crate, crate_name: &str) -> Vec<Id> {
    let mut ids: Vec<Id> = krate
        .paths
        .iter()
        .filter_map(|(id, summary)| {
            let is_root_exposed = summary.path.len() == 2
                && summary
                    .path
                    .first()
                    .is_some_and(|p| p == crate_name || p == &crate_name.replace('-', "_"));
            if matches!(summary.kind, ItemKind::Function) && is_root_exposed {
                Some(*id)
            } else {
                None
            }
        })
        .collect();

    if !ids.is_empty() {
        ids.sort_by_key(|id| id.0);
        ids.dedup();
        return ids;
    }

    if let Some(root_item) = krate.index.get(&krate.root) {
        if let ItemEnum::Module(module) = &root_item.inner {
            return module.items.clone();
        }
    }
    Vec::new()
}

fn parse_supported_params(
    inputs: &[(String, Type)],
    paths: Option<&HashMap<Id, ItemSummary>>,
) -> Result<Vec<(Symbol, Rc<ir_type::Ty>)>, String> {
    let mut params = Vec::with_capacity(inputs.len());

    for (idx, (raw_name, ty)) in inputs.iter().enumerate() {
        let sanitized = sanitize_symbol(raw_name);
        let param_name = if sanitized.is_empty() {
            format!("arg{idx}")
        } else {
            sanitized
        };
        let param_ty = parse_supported_ty(ty, TyPosition::Param, paths)
            .map_err(|reason| format!("unsupported parameter type at #{idx}: {reason}"))?;
        params.push((Symbol::from(param_name), param_ty));
    }

    Ok(params)
}

fn parse_public_function_item(
    item: &Item,
    crate_sanitized: &str,
    paths: Option<&HashMap<Id, ItemSummary>>,
) -> Option<Result<RustImportedFn, String>> {
    if !matches!(item.visibility, Visibility::Public) {
        return None;
    }

    let ItemEnum::Function(function) = &item.inner else {
        return None;
    };
    let name = item.name.as_deref()?;

    let FunctionSignature { inputs, output, .. } = &function.sig;

    let params = match parse_supported_params(inputs, paths) {
        Ok(params) => params,
        Err(reason) => return Some(Err(format!("{name}: {reason}"))),
    };
    let return_ty = match output.as_ref() {
        None => Rc::new(ir_type::Ty::new_unit()),
        Some(ty) if ty_is_empty_tuple(ty) => Rc::new(ir_type::Ty::new_unit()),
        Some(ty) => match parse_supported_ty(ty, TyPosition::Return, paths) {
            Ok(t) => t,
            Err(reason) => {
                return Some(Err(format!("{name}: unsupported return type: {reason}")))
            }
        },
    };

    Some(Ok(RustImportedFn {
        kaede_name: Symbol::from(name.to_string()),
        export_name: Symbol::from(format!(
            "kaede_rust_shim_{}_{}",
            crate_sanitized,
            sanitize_symbol(name)
        )),
        params,
        return_ty,
    }))
}

fn parse_root_public_functions(
    rustdoc_json: &Path,
    crate_name: &str,
) -> anyhow::Result<(Vec<RustImportedFn>, Vec<String>)> {
    let content = fs::read_to_string(rustdoc_json)
        .with_context(|| format!("failed to read rustdoc json: {}", rustdoc_json.display()))?;
    let krate: Crate = serde_json::from_str(&content)
        .with_context(|| format!("failed to parse rustdoc json: {}", rustdoc_json.display()))?;

    let candidate_ids = collect_candidate_ids(&krate, crate_name);

    let mut functions = Vec::new();
    let mut skipped = Vec::new();
    let crate_sanitized = sanitize_symbol(crate_name);

    for item_id in candidate_ids {
        let Some(item) = krate.index.get(&item_id) else {
            continue;
        };

        match parse_public_function_item(item, &crate_sanitized, Some(&krate.paths)) {
            Some(Ok(parsed)) => functions.push(parsed),
            Some(Err(reason)) => skipped.push(reason),
            None => {}
        }
    }

    Ok((functions, skipped))
}

fn rust_ty_to_shim_rs(ty: &ir_type::Ty, position: ShimTyPosition) -> anyhow::Result<&'static str> {
    if is_kaede_str_ty(ty) {
        return match position {
            ShimTyPosition::Param => Ok("*const KaedeStr"),
            ShimTyPosition::Return => Err(anyhow!("unsupported shim type: {}", ty.kind)),
        };
    }

    if is_kaede_string_ty(ty) {
        return match position {
            ShimTyPosition::Param => Ok("*const KaedeString"),
            ShimTyPosition::Return => Ok("*mut KaedeString"),
        };
    }

    match ty.kind.as_ref() {
        ir_type::TyKind::Fundamental(fty) => match fty.kind {
            ir_type::FundamentalTypeKind::I8 => Ok("i8"),
            ir_type::FundamentalTypeKind::U8 => Ok("u8"),
            ir_type::FundamentalTypeKind::I16 => Ok("i16"),
            ir_type::FundamentalTypeKind::U16 => Ok("u16"),
            ir_type::FundamentalTypeKind::I32 => Ok("i32"),
            ir_type::FundamentalTypeKind::U32 => Ok("u32"),
            ir_type::FundamentalTypeKind::I64 => Ok("i64"),
            ir_type::FundamentalTypeKind::U64 => Ok("u64"),
            ir_type::FundamentalTypeKind::F32 => Ok("f32"),
            ir_type::FundamentalTypeKind::F64 => Ok("f64"),
            ir_type::FundamentalTypeKind::Bool => Ok("bool"),
            ir_type::FundamentalTypeKind::Char => Ok("u32"),
            ir_type::FundamentalTypeKind::Str => Err(anyhow!("unsupported shim type: {}", ty.kind)),
        },
        ir_type::TyKind::Unit => Ok("()"),
        _ => Err(anyhow!("unsupported shim type: {}", ty.kind)),
    }
}

fn shim_arg_expr(name: &str, ty: &ir_type::Ty) -> anyhow::Result<String> {
    if is_kaede_str_ty(ty) {
        return Ok(format!("unsafe {{ kaede_str_as_rust_str({name}) }}"));
    }

    if is_kaede_string_ty(ty) {
        return Ok(format!("unsafe {{ kaede_string_as_rust_string({name}) }}"));
    }

    if is_kaede_char_ty(ty) {
        return Ok(format!(
            "std::char::from_u32({name}).expect(\"invalid Kaede char\")"
        ));
    }

    rust_ty_to_shim_rs(ty, ShimTyPosition::Param)?;
    Ok(name.to_string())
}

fn shim_needs_kaede_str_helpers(functions: &[RustImportedFn]) -> bool {
    functions
        .iter()
        .flat_map(|func| func.params.iter())
        .any(|(_, ty)| is_kaede_str_ty(ty))
}

fn shim_needs_kaede_string_param_helpers(functions: &[RustImportedFn]) -> bool {
    functions
        .iter()
        .flat_map(|func| func.params.iter())
        .any(|(_, ty)| is_kaede_string_ty(ty))
}

fn shim_needs_kaede_string_return_helpers(functions: &[RustImportedFn]) -> bool {
    functions
        .iter()
        .any(|func| is_kaede_string_ty(&func.return_ty))
}

fn generate_kaede_str_helpers() -> &'static str {
    r#"#[repr(C)]
pub struct KaedeStr {
    ptr: *const u8,
    len: u64,
}

unsafe fn kaede_str_as_rust_str<'a>(value: *const KaedeStr) -> &'a str {
    let value = unsafe { &*value };
    if value.len == 0 {
        return "";
    }
    let bytes = unsafe { std::slice::from_raw_parts(value.ptr, value.len as usize) };
    unsafe { std::str::from_utf8_unchecked(bytes) }
}

"#
}

fn generate_kaede_string_types() -> &'static str {
    r#"#[repr(C)]
pub struct KaedeVectorU8 {
    ptr: *mut u8,
    len: u64,
    capacity: u64,
}

#[repr(C)]
pub struct KaedeString {
    bytes: *mut KaedeVectorU8,
}

"#
}

fn generate_kaede_string_param_helper() -> &'static str {
    r#"unsafe fn kaede_string_as_rust_string(value: *const KaedeString) -> String {
    let value = unsafe { &*value };
    let bytes = unsafe { &*value.bytes };
    if bytes.len == 0 {
        return String::new();
    }
    let slice = unsafe { std::slice::from_raw_parts(bytes.ptr, bytes.len as usize) };
    unsafe { String::from_utf8_unchecked(slice.to_vec()) }
}

"#
}

fn generate_kaede_string_return_helper() -> &'static str {
    r#"unsafe extern "C" {
    fn kaede_mem_alloc(size: usize) -> *mut std::ffi::c_void;
}

fn rust_str_into_kaede_string(value: impl AsRef<str>) -> *mut KaedeString {
    let bytes = value.as_ref().as_bytes();
    let len = bytes.len();
    let ptr = unsafe { kaede_mem_alloc(len) }.cast::<u8>();
    if len > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len);
        }
    }

    let string_ptr =
        unsafe { kaede_mem_alloc(std::mem::size_of::<KaedeString>()) }.cast::<KaedeString>();
    let bytes_ptr =
        unsafe { kaede_mem_alloc(std::mem::size_of::<KaedeVectorU8>()) }.cast::<KaedeVectorU8>();
    unsafe {
        bytes_ptr.write(KaedeVectorU8 {
            ptr,
            len: len as u64,
            capacity: len as u64,
        });
        string_ptr.write(KaedeString { bytes: bytes_ptr });
    }

    string_ptr
}

"#
}

fn generate_kaede_string_helpers(functions: &[RustImportedFn]) -> String {
    let needs_param_helper = shim_needs_kaede_string_param_helpers(functions);
    let needs_return_helper = shim_needs_kaede_string_return_helpers(functions);
    if !needs_param_helper && !needs_return_helper {
        return String::new();
    }

    let mut helpers = String::from(generate_kaede_string_types());
    if needs_param_helper {
        helpers.push_str(generate_kaede_string_param_helper());
    }
    if needs_return_helper {
        helpers.push_str(generate_kaede_string_return_helper());
    }
    helpers
}

fn generate_shim_build_script() -> &'static str {
    r#"use std::{env, fs, path::PathBuf};

fn kaede_dir() -> PathBuf {
    if let Some(dir) = env::var_os("KAEDE_DIR") {
        return PathBuf::from(dir);
    }

    if let Some(home) = env::var_os("HOME") {
        return PathBuf::from(home).join(".kaede");
    }

    panic!("KAEDE_DIR or HOME must be set to build Kaede Rust shims");
}

fn third_party_lib_dirs(kaede_dir: &PathBuf) -> Vec<PathBuf> {
    let mut lib_dirs = match fs::read_dir(kaede_dir.join("third_party")) {
        Ok(entries) => entries
            .filter_map(Result::ok)
            .map(|entry| entry.path().join("lib"))
            .filter(|path| path.is_dir())
            .collect::<Vec<_>>(),
        Err(_) => Vec::new(),
    };
    lib_dirs.sort();
    lib_dirs
}

fn main() {
    println!("cargo:rerun-if-env-changed=KAEDE_DIR");

    let kaede_dir = kaede_dir();
    let mut lib_dirs = vec![kaede_dir.join("lib")];
    lib_dirs.extend(third_party_lib_dirs(&kaede_dir));

    println!("cargo:rustc-link-lib=dylib=kd");

    for lib_dir in &lib_dirs {
        println!("cargo:rustc-link-search=native={}", lib_dir.display());
    }

    if cfg!(any(target_os = "macos", target_os = "linux")) {
        for lib_dir in &lib_dirs {
            println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_dir.display());
        }
    }
}
"#
}

fn generate_shim_crate(
    project_root: &Path,
    rust_manifest: &Path,
    package_name: &str,
    crate_name: &str,
    functions: &[RustImportedFn],
) -> anyhow::Result<Option<PathBuf>> {
    if functions.is_empty() {
        return Ok(None);
    }

    let crate_sanitized = sanitize_symbol(crate_name);
    let shim_name = format!("kaede_rust_shim_{crate_sanitized}");
    let shim_dir = project_root
        .join("build")
        .join("kaede_rust_shim")
        .join(crate_name);
    let src_dir = shim_dir.join("src");
    fs::create_dir_all(&src_dir)
        .with_context(|| format!("failed to create {}", src_dir.display()))?;

    let cargo_dir = shim_dir.join(".cargo");
    if cargo_dir.exists() {
        fs::remove_dir_all(&cargo_dir)
            .with_context(|| format!("failed to remove {}", cargo_dir.display()))?;
    }
    fs::write(shim_dir.join("build.rs"), generate_shim_build_script())?;

    let rust_dir = rust_manifest
        .parent()
        .ok_or_else(|| anyhow!("invalid rust manifest path: {}", rust_manifest.display()))?;
    let rust_dir = rust_dir
        .canonicalize()
        .with_context(|| format!("failed to canonicalize {}", rust_dir.display()))?;
    let cargo_toml = format!(
        "[package]\nname = \"{shim_name}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[workspace]\n\n[lib]\ncrate-type = [\"cdylib\"]\n\n[dependencies]\nkaede_user_crate = {{ package = \"{package_name}\", path = \"{}\" }}\n",
        rust_dir.display()
    );
    fs::write(shim_dir.join("Cargo.toml"), cargo_toml)?;

    let mut lib_rs = String::new();
    if shim_needs_kaede_str_helpers(functions) {
        lib_rs.push_str(generate_kaede_str_helpers());
    }
    lib_rs.push_str(&generate_kaede_string_helpers(functions));

    for func in functions {
        let param_decls = func
            .params
            .iter()
            .map(|(name, ty)| {
                Ok(format!(
                    "{}: {}",
                    sanitize_symbol(name.as_str()),
                    rust_ty_to_shim_rs(ty, ShimTyPosition::Param)?
                ))
            })
            .collect::<anyhow::Result<Vec<_>>>()?
            .join(", ");
        let return_ty = rust_ty_to_shim_rs(&func.return_ty, ShimTyPosition::Return)?;
        let args = func
            .params
            .iter()
            .map(|(name, ty)| {
                let sanitized = sanitize_symbol(name.as_str());
                shim_arg_expr(&sanitized, ty)
            })
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .collect::<Vec<_>>()
            .join(", ");

        lib_rs.push_str("#[unsafe(no_mangle)]\n");
        if return_ty == "()" {
            lib_rs.push_str(&format!(
                "pub extern \"C\" fn {}({}) {{\n    kaede_user_crate::{}({});\n}}\n\n",
                func.export_name.as_str(),
                param_decls,
                func.kaede_name.as_str(),
                args
            ));
        } else if is_kaede_string_ty(&func.return_ty) {
            lib_rs.push_str(&format!(
                "pub extern \"C\" fn {}({}) -> {} {{\n    rust_str_into_kaede_string(kaede_user_crate::{}({}))\n}}\n\n",
                func.export_name.as_str(),
                param_decls,
                return_ty,
                func.kaede_name.as_str(),
                args
            ));
        } else if is_kaede_char_ty(&func.return_ty) {
            lib_rs.push_str(&format!(
                "pub extern \"C\" fn {}({}) -> {} {{\n    kaede_user_crate::{}({}) as u32\n}}\n\n",
                func.export_name.as_str(),
                param_decls,
                return_ty,
                func.kaede_name.as_str(),
                args
            ));
        } else {
            lib_rs.push_str(&format!(
                "pub extern \"C\" fn {}({}) -> {} {{\n    kaede_user_crate::{}({})\n}}\n\n",
                func.export_name.as_str(),
                param_decls,
                return_ty,
                func.kaede_name.as_str(),
                args
            ));
        }
    }
    fs::write(src_dir.join("lib.rs"), lib_rs)?;

    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--manifest-path")
        .arg(shim_dir.join("Cargo.toml"));
    run_command(cmd, "cargo build (shim)")?;

    let dylib = shim_dir
        .join("target")
        .join("debug")
        .join(format!("lib{shim_name}.{}", lib_extension()));
    if !dylib.exists() {
        return Err(anyhow!(
            "shim dylib not found after build: {}",
            dylib.display()
        ));
    }

    Ok(Some(dylib))
}

pub fn resolve_rust_import(
    root_dir: &Path,
    rust_subdir: &Path,
    crate_name: &str,
) -> anyhow::Result<RustImportOutput> {
    let project_root = project_root_from_root_dir(root_dir, rust_subdir)?;
    let rust_manifest = project_root.join(rust_subdir).join("Cargo.toml");
    let package_name = read_package_name(&rust_manifest)?;
    if package_name != crate_name {
        return Err(anyhow!(
            "import rust::{crate_name} does not match {}/Cargo.toml package name `{package_name}`",
            rust_subdir.display()
        ));
    }

    let rustdoc_json = rustdoc_json::Builder::default()
        .toolchain("nightly-2026-03-17")
        .manifest_path(&rust_manifest)
        .silent(true)
        .build()
        .with_context(|| {
            format!(
                "cargo +nightly rustdoc failed for {}",
                rust_manifest.display()
            )
        })?;
    let (functions, skipped) = parse_root_public_functions(&rustdoc_json, crate_name)?;
    let dylib_path = generate_shim_crate(
        &project_root,
        &rust_manifest,
        &package_name,
        crate_name,
        &functions,
    )?;

    Ok(RustImportOutput {
        module_path: ModulePath::new(vec![
            Symbol::from("rust".to_string()),
            Symbol::from(crate_name.to_string()),
        ]),
        functions,
        dylib_path,
        skipped,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Value};

    fn fundamental(kind: ir_type::FundamentalTypeKind) -> Rc<ir_type::Ty> {
        Rc::new(ir_type::make_fundamental_type(
            kind,
            ir_type::Mutability::Not,
        ))
    }

    fn ty(value: Value) -> Type {
        serde_json::from_value(value).expect("failed to deserialize rustdoc_types::Type")
    }

    fn rust_string_resolved_path_ty() -> Type {
        ty(json!({
            "resolved_path": {
                "path": "String",
                "id": 1,
                "args": null
            }
        }))
    }

    fn rust_string_paths() -> HashMap<Id, ItemSummary> {
        let mut paths = HashMap::new();
        paths.insert(
            Id(1),
            ItemSummary {
                crate_id: 0,
                path: vec![
                    "alloc".to_string(),
                    "string".to_string(),
                    "String".to_string(),
                ],
                kind: ItemKind::Struct,
            },
        );
        paths
    }

    fn rust_imported_fn(
        params: Vec<(Symbol, Rc<ir_type::Ty>)>,
        return_ty: Rc<ir_type::Ty>,
    ) -> RustImportedFn {
        RustImportedFn {
            kaede_name: Symbol::from("probe".to_string()),
            export_name: Symbol::from("shim_probe".to_string()),
            params,
            return_ty,
        }
    }

    #[test]
    fn parse_supported_ty_accepts_supported_primitives() {
        let cases = [
            ("i8", ir_type::FundamentalTypeKind::I8),
            ("u8", ir_type::FundamentalTypeKind::U8),
            ("i16", ir_type::FundamentalTypeKind::I16),
            ("u16", ir_type::FundamentalTypeKind::U16),
            ("i32", ir_type::FundamentalTypeKind::I32),
            ("u32", ir_type::FundamentalTypeKind::U32),
            ("i64", ir_type::FundamentalTypeKind::I64),
            ("u64", ir_type::FundamentalTypeKind::U64),
            ("bool", ir_type::FundamentalTypeKind::Bool),
        ];

        for (primitive, expected) in cases {
            let parsed = parse_supported_ty(
                &ty(json!({ "primitive": primitive })),
                TyPosition::Param,
                None,
            )
            .unwrap();
            assert_eq!(parsed, fundamental(expected), "primitive `{primitive}`");
        }
    }

    #[test]
    fn parse_supported_ty_accepts_unit_shapes() {
        assert_eq!(
            parse_supported_ty(&ty(json!({ "tuple": [] })), TyPosition::Param, None).unwrap(),
            Rc::new(ir_type::Ty::new_unit())
        );
    }

    #[test]
    fn parse_supported_ty_accepts_shared_borrowed_str_parameter() {
        let parsed = parse_supported_ty(
            &ty(json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "str" }
                }
            })),
            TyPosition::Param,
            None,
        )
        .unwrap();

        assert!(parsed.is_str());
        assert_eq!(parsed, kaede_str_ir_ty());
    }

    #[test]
    fn parse_supported_ty_accepts_char_and_owned_string_shapes() {
        let char_ty =
            parse_supported_ty(&ty(json!({ "primitive": "char" })), TyPosition::Param, None)
                .unwrap();
        assert_eq!(char_ty, fundamental(ir_type::FundamentalTypeKind::Char));

        let paths = rust_string_paths();
        let string_param = parse_supported_ty(
            &rust_string_resolved_path_ty(),
            TyPosition::Param,
            Some(&paths),
        )
        .unwrap();
        assert!(is_kaede_string_ty(&string_param));

        let str_return = parse_supported_ty(
            &ty(json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "str" }
                }
            })),
            TyPosition::Return,
            None,
        )
        .unwrap();
        assert!(is_kaede_string_ty(&str_return));
    }

    #[test]
    fn parse_supported_ty_accepts_rust_string_return() {
        let paths = rust_string_paths();
        let parsed = parse_supported_ty(
            &rust_string_resolved_path_ty(),
            TyPosition::Return,
            Some(&paths),
        )
        .unwrap();

        assert!(is_kaede_string_ty(&parsed));
        assert_eq!(parsed, kaede_string_ir_ty());
    }

    #[test]
    fn parse_supported_ty_rejects_unsupported_shapes_with_reason() {
        let mut_str_err = parse_supported_ty(
            &ty(json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": true,
                    "type": { "primitive": "str" }
                }
            })),
            TyPosition::Param,
            None,
        )
        .unwrap_err();
        assert!(mut_str_err.contains("mutable borrowed string type `&mut str`"));

        let borrowed_ref_err = parse_supported_ty(
            &ty(json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "i32" }
                }
            })),
            TyPosition::Param,
            None,
        )
        .unwrap_err();
        assert!(borrowed_ref_err.contains("borrowed reference type `&i32`"));

        let raw_ptr_err = parse_supported_ty(
            &ty(json!({
                "raw_pointer": {
                    "is_mutable": false,
                    "type": { "primitive": "i8" }
                }
            })),
            TyPosition::Param,
            None,
        )
        .unwrap_err();
        assert!(raw_ptr_err.contains("raw pointer type `*const i8`"));

        let string_paths = rust_string_paths();
        let string_param = parse_supported_ty(
            &rust_string_resolved_path_ty(),
            TyPosition::Param,
            Some(&string_paths),
        )
        .unwrap();
        assert!(is_kaede_string_ty(&string_param));
    }

    #[test]
    fn rust_ty_to_shim_rs_accepts_supported_types() {
        let cases = [
            (fundamental(ir_type::FundamentalTypeKind::I8), "i8"),
            (fundamental(ir_type::FundamentalTypeKind::U8), "u8"),
            (fundamental(ir_type::FundamentalTypeKind::I16), "i16"),
            (fundamental(ir_type::FundamentalTypeKind::U16), "u16"),
            (fundamental(ir_type::FundamentalTypeKind::I32), "i32"),
            (fundamental(ir_type::FundamentalTypeKind::U32), "u32"),
            (fundamental(ir_type::FundamentalTypeKind::I64), "i64"),
            (fundamental(ir_type::FundamentalTypeKind::U64), "u64"),
            (fundamental(ir_type::FundamentalTypeKind::Bool), "bool"),
            (fundamental(ir_type::FundamentalTypeKind::Char), "u32"),
            (Rc::new(ir_type::Ty::new_unit()), "()"),
        ];

        for (ty, expected) in cases {
            assert_eq!(
                rust_ty_to_shim_rs(&ty, ShimTyPosition::Param).unwrap(),
                expected
            );
        }

        assert_eq!(
            rust_ty_to_shim_rs(&kaede_str_ir_ty(), ShimTyPosition::Param).unwrap(),
            "*const KaedeStr"
        );
        assert_eq!(
            rust_ty_to_shim_rs(&kaede_string_ir_ty(), ShimTyPosition::Return).unwrap(),
            "*mut KaedeString"
        );
        assert_eq!(
            rust_ty_to_shim_rs(&kaede_string_ir_ty(), ShimTyPosition::Param).unwrap(),
            "*const KaedeString"
        );
    }

    #[test]
    fn rust_ty_to_shim_rs_rejects_unsupported_types() {
        assert!(rust_ty_to_shim_rs(
            &fundamental(ir_type::FundamentalTypeKind::Str),
            ShimTyPosition::Param,
        )
        .is_err());
        assert!(rust_ty_to_shim_rs(
            &ir_type::Ty::wrap_in_pointer(fundamental(ir_type::FundamentalTypeKind::I8,)),
            ShimTyPosition::Param,
        )
        .is_err());
        assert!(rust_ty_to_shim_rs(&kaede_str_ir_ty(), ShimTyPosition::Return).is_err());
    }

    #[test]
    fn shim_arg_expr_converts_kaede_str_param() {
        assert_eq!(
            shim_arg_expr("value", &kaede_str_ir_ty()).unwrap(),
            "unsafe { kaede_str_as_rust_str(value) }"
        );
        assert_eq!(
            shim_arg_expr("value", &kaede_string_ir_ty()).unwrap(),
            "unsafe { kaede_string_as_rust_string(value) }"
        );
        assert_eq!(
            shim_arg_expr("value", &fundamental(ir_type::FundamentalTypeKind::Char)).unwrap(),
            "std::char::from_u32(value).expect(\"invalid Kaede char\")"
        );
        assert_eq!(
            shim_arg_expr("value", &fundamental(ir_type::FundamentalTypeKind::I32)).unwrap(),
            "value"
        );
    }

    #[test]
    fn generate_kaede_string_helpers_only_emits_needed_helpers() {
        let return_only = [rust_imported_fn(
            vec![(Symbol::from("input".to_string()), kaede_str_ir_ty())],
            kaede_string_ir_ty(),
        )];
        let return_helpers = generate_kaede_string_helpers(&return_only);
        assert!(return_helpers.contains("pub struct KaedeString"));
        assert!(!return_helpers.contains("kaede_string_as_rust_string"));
        assert!(return_helpers.contains("rust_str_into_kaede_string"));
        assert!(return_helpers.contains("kaede_mem_alloc"));

        let param_only = [rust_imported_fn(
            vec![(Symbol::from("input".to_string()), kaede_string_ir_ty())],
            fundamental(ir_type::FundamentalTypeKind::I32),
        )];
        let param_helpers = generate_kaede_string_helpers(&param_only);
        assert!(param_helpers.contains("pub struct KaedeString"));
        assert!(param_helpers.contains("kaede_string_as_rust_string"));
        assert!(!param_helpers.contains("rust_str_into_kaede_string"));
        assert!(!param_helpers.contains("kaede_mem_alloc"));

        let both = [rust_imported_fn(
            vec![(Symbol::from("input".to_string()), kaede_string_ir_ty())],
            kaede_string_ir_ty(),
        )];
        let both_helpers = generate_kaede_string_helpers(&both);
        assert!(both_helpers.contains("kaede_string_as_rust_string"));
        assert!(both_helpers.contains("rust_str_into_kaede_string"));
    }
}
