use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use anyhow::{anyhow, Context as _};
use kaede_common::lib_extension;
use kaede_ir::{module_path::ModulePath, qualified_symbol::QualifiedSymbol, ty as ir_type};
use kaede_symbol::Symbol;
use serde_json::Value;
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

fn json_id_to_string(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        _ => None,
    }
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
        "bool" => Some(ir_type::FundamentalTypeKind::Bool),
        _ => None,
    }
}

fn rustdoc_ty_description(value: &Value) -> String {
    if value.is_null() {
        return "()".to_string();
    }

    if value
        .get("tuple")
        .and_then(Value::as_array)
        .is_some_and(|arr| arr.is_empty())
    {
        return "()".to_string();
    }

    if let Some(primitive) = value.get("primitive").and_then(Value::as_str) {
        return primitive.to_string();
    }

    if let Some(borrowed_ref) = value.get("borrowed_ref") {
        let is_mutable = borrowed_ref
            .get("is_mutable")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        let prefix = if is_mutable { "&mut " } else { "&" };
        let inner = borrowed_ref
            .get("type")
            .map(rustdoc_ty_description)
            .unwrap_or_else(|| "?".to_string());
        return format!("{prefix}{inner}");
    }

    if let Some(raw_pointer) = value.get("raw_pointer") {
        let is_mutable = raw_pointer
            .get("is_mutable")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        let prefix = if is_mutable { "*mut " } else { "*const " };
        let inner = raw_pointer
            .get("type")
            .map(rustdoc_ty_description)
            .unwrap_or_else(|| "?".to_string());
        return format!("{prefix}{inner}");
    }

    if value.get("slice").is_some() {
        return "[_]".to_string();
    }

    if let Some(array) = value.get("array") {
        let inner = array
            .get("type")
            .map(rustdoc_ty_description)
            .unwrap_or_else(|| "?".to_string());
        let len = array
            .get("len")
            .map(|len| len.to_string())
            .unwrap_or_else(|| "?".to_string());
        return format!("[{inner}; {len}]");
    }

    if value.get("tuple").is_some() {
        return "tuple".to_string();
    }

    if let Some(path) = value.get("resolved_path") {
        if let Some(name) = path.get("name").and_then(Value::as_str) {
            return name.to_string();
        }
        if let Some(name) = path.get("path").and_then(Value::as_str) {
            return name.to_string();
        }
    }

    "unknown".to_string()
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

fn resolved_path_segments(
    value: &Value,
    paths: Option<&serde_json::Map<String, Value>>,
) -> Option<Vec<String>> {
    let resolved_path = value.get("resolved_path")?;
    let path_id = json_id_to_string(resolved_path.get("id")?)?;
    let path_entry = paths?.get(&path_id)?;
    let path = path_entry.get("path")?.as_array()?;

    path.iter()
        .map(|segment| segment.as_str().map(ToOwned::to_owned))
        .collect()
}

fn is_rust_string_resolved_path(
    value: &Value,
    paths: Option<&serde_json::Map<String, Value>>,
) -> bool {
    let Some(segments) = resolved_path_segments(value, paths) else {
        return false;
    };

    let matches = |expected: &[&str]| -> bool {
        segments.len() == expected.len()
            && segments
                .iter()
                .zip(expected.iter())
                .all(|(actual, expected)| actual == expected)
    };

    matches(&["alloc", "string", "String"]) || matches(&["std", "string", "String"])
}

fn parse_borrowed_ref_ty(value: &Value, position: TyPosition) -> Result<Rc<ir_type::Ty>, String> {
    let borrowed_ref = value.get("borrowed_ref").ok_or_else(|| {
        format!(
            "unsupported borrowed reference type `{}`",
            rustdoc_ty_description(value)
        )
    })?;
    let is_mutable = borrowed_ref
        .get("is_mutable")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    let inner = borrowed_ref.get("type").unwrap_or(&Value::Null);

    if inner.get("primitive").and_then(Value::as_str) == Some("str") {
        if is_mutable {
            return Err("unsupported mutable borrowed string type `&mut str`".to_string());
        }

        return match position {
            TyPosition::Param => Ok(kaede_str_ir_ty()),
            TyPosition::Return => Ok(kaede_string_ir_ty()),
        };
    }

    Err(format!(
        "unsupported borrowed reference type `{}`",
        rustdoc_ty_description(value)
    ))
}

fn parse_supported_ty(
    value: &Value,
    position: TyPosition,
    paths: Option<&serde_json::Map<String, Value>>,
) -> Result<Rc<ir_type::Ty>, String> {
    if value.is_null() {
        return Ok(Rc::new(ir_type::Ty::new_unit()));
    }

    if let Some(primitive) = value.get("primitive").and_then(Value::as_str) {
        if primitive == "char" {
            return Ok(Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Char,
                ir_type::Mutability::Not,
            )));
        }

        if let Some(kind) = fundamental_kind_from_rustdoc_primitive(primitive) {
            return Ok(Rc::new(ir_type::make_fundamental_type(
                kind,
                ir_type::Mutability::Not,
            )));
        }

        return Err(format!("unsupported primitive type `{primitive}`"));
    }

    if value
        .get("tuple")
        .and_then(Value::as_array)
        .is_some_and(|arr| arr.is_empty())
    {
        return Ok(Rc::new(ir_type::Ty::new_unit()));
    }

    if value.get("borrowed_ref").is_some() {
        return parse_borrowed_ref_ty(value, position);
    }

    if value.get("resolved_path").is_some() && is_rust_string_resolved_path(value, paths) {
        return Ok(kaede_string_ir_ty());
    }

    if value.get("raw_pointer").is_some() {
        return Err(format!(
            "unsupported raw pointer type `{}`",
            rustdoc_ty_description(value)
        ));
    }

    Err(format!(
        "unsupported non-primitive type `{}`",
        rustdoc_ty_description(value)
    ))
}

fn is_public_visibility(item: &Value) -> bool {
    match item.get("visibility") {
        Some(Value::String(s)) => s == "public",
        Some(Value::Object(map)) => map.get("public").and_then(Value::as_bool).unwrap_or(false),
        _ => false,
    }
}

fn extract_fn_io(sig_container: &Value) -> Option<(&Value, &Value)> {
    if let Some(sig) = sig_container.get("sig") {
        let inputs = sig.get("inputs")?;
        let output = sig.get("output")?;
        return Some((inputs, output));
    }

    if let Some(decl) = sig_container.get("decl") {
        let inputs = decl.get("inputs")?;
        let output = decl.get("output")?;
        return Some((inputs, output));
    }

    let inputs = sig_container.get("inputs")?;
    let output = sig_container.get("output")?;
    Some((inputs, output))
}

fn project_root_from_root_dir(root_dir: &Path) -> anyhow::Result<PathBuf> {
    let direct = root_dir.join("rust").join("Cargo.toml");
    if direct.exists() {
        return Ok(root_dir.to_path_buf());
    }

    let parent = root_dir
        .parent()
        .ok_or_else(|| anyhow!("failed to resolve project root from {}", root_dir.display()))?;
    let parent_manifest = parent.join("rust").join("Cargo.toml");
    if parent_manifest.exists() {
        return Ok(parent.to_path_buf());
    }

    Err(anyhow!(
        "rust/Cargo.toml not found (searched '{}' and '{}')",
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

fn run_rustdoc_json(rust_manifest: &Path) -> anyhow::Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.args([
        "+nightly",
        "rustdoc",
        "--manifest-path",
        &rust_manifest.to_string_lossy(),
        "--lib",
        "--",
        "-Z",
        "unstable-options",
        "--output-format",
        "json",
    ]);
    run_command(cmd, "cargo +nightly rustdoc")
}

fn rustdoc_json_path(rust_dir: &Path, crate_name: &str) -> anyhow::Result<PathBuf> {
    let doc_dir = rust_dir.join("target").join("doc");
    let candidates = [
        doc_dir.join(format!("{crate_name}.json")),
        doc_dir.join(format!("{}.json", crate_name.replace('-', "_"))),
    ];

    candidates
        .into_iter()
        .find(|p| p.exists())
        .ok_or_else(|| anyhow!("rustdoc JSON not found in {}", doc_dir.display()))
}

fn infer_root_id(
    index: &serde_json::Map<String, Value>,
    crate_name: &str,
) -> anyhow::Result<String> {
    index
        .iter()
        .find_map(|(id, item)| {
            let is_module = item
                .get("kind")
                .and_then(Value::as_str)
                .is_some_and(|k| k == "module");
            let is_local_crate = item
                .get("crate_id")
                .and_then(Value::as_u64)
                .is_some_and(|cid| cid == 0);
            let is_named_like_crate = item
                .get("name")
                .and_then(Value::as_str)
                .is_some_and(|name| name == crate_name || name == crate_name.replace('-', "_"));

            if is_module && is_local_crate && is_named_like_crate {
                Some(id.clone())
            } else {
                None
            }
        })
        .or_else(|| {
            index.iter().find_map(|(id, item)| {
                let is_module = item
                    .get("kind")
                    .and_then(Value::as_str)
                    .is_some_and(|k| k == "module");
                let is_local_crate = item
                    .get("crate_id")
                    .and_then(Value::as_u64)
                    .is_some_and(|cid| cid == 0);
                if is_module && is_local_crate {
                    Some(id.clone())
                } else {
                    None
                }
            })
        })
        .ok_or_else(|| anyhow!("missing root id in rustdoc json and failed to infer it"))
}

fn collect_candidate_ids(root_item: &Value, rustdoc: &Value, crate_name: &str) -> Vec<String> {
    let root_item_ids = root_item
        .get("inner")
        .and_then(|v| v.get("module"))
        .and_then(|v| v.get("items"))
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .filter_map(|v| json_id_to_string(&v))
        .collect::<Vec<_>>();

    if let Some(paths) = rustdoc.get("paths").and_then(Value::as_object) {
        let mut ids = paths
            .iter()
            .filter_map(|(id, entry)| {
                let kind_is_fn = entry
                    .get("kind")
                    .and_then(Value::as_str)
                    .is_some_and(|k| k == "function");
                let path = entry.get("path").and_then(Value::as_array)?;
                let is_root_exposed = path.len() == 2
                    && path
                        .first()
                        .and_then(Value::as_str)
                        .is_some_and(|p| p == crate_name || p == crate_name.replace('-', "_"));

                if kind_is_fn && is_root_exposed {
                    Some(id.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        if !ids.is_empty() {
            ids.sort();
            ids.dedup();
            return ids;
        }
    }

    root_item_ids
}

fn parse_supported_params(
    inputs: &[Value],
    paths: Option<&serde_json::Map<String, Value>>,
) -> Result<Vec<(Symbol, Rc<ir_type::Ty>)>, String> {
    let mut params = Vec::with_capacity(inputs.len());

    for (idx, input) in inputs.iter().enumerate() {
        let Some(input_pair) = input.as_array() else {
            return Err(format!("parameter #{idx} format is unsupported"));
        };
        if input_pair.len() != 2 {
            return Err(format!("parameter #{idx} format is unsupported"));
        }
        let param_name = input_pair[0]
            .as_str()
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| format!("arg{idx}"));
        let param_name = {
            let sanitized = sanitize_symbol(&param_name);
            if sanitized.is_empty() {
                format!("arg{idx}")
            } else {
                sanitized
            }
        };
        let param_ty = parse_supported_ty(&input_pair[1], TyPosition::Param, paths)
            .map_err(|reason| format!("unsupported parameter type at #{idx}: {reason}"))?;
        params.push((Symbol::from(param_name), param_ty));
    }

    Ok(params)
}

fn parse_public_function_item(
    item: &Value,
    crate_sanitized: &str,
    paths: Option<&serde_json::Map<String, Value>>,
) -> Option<Result<RustImportedFn, String>> {
    if !is_public_visibility(item) {
        return None;
    }

    let is_function = item
        .get("kind")
        .and_then(Value::as_str)
        .is_some_and(|v| v == "function")
        || item.get("inner").and_then(|v| v.get("function")).is_some();
    if !is_function {
        return None;
    }

    let name = item.get("name").and_then(Value::as_str)?;
    let function_node = match item.get("inner").and_then(|v| v.get("function")) {
        Some(node) => node,
        None => return Some(Err(format!("{name}: failed to read function signature"))),
    };
    let (inputs, output) = match extract_fn_io(function_node) {
        Some(io) => io,
        None => return Some(Err(format!("{name}: failed to read function signature"))),
    };

    let inputs = match inputs.as_array() {
        Some(inputs) => inputs,
        None => return Some(Err(format!("{name}: failed to read input types"))),
    };
    let params = match parse_supported_params(inputs, paths) {
        Ok(params) => params,
        Err(reason) => return Some(Err(format!("{name}: {reason}"))),
    };
    let return_ty = match parse_supported_ty(output, TyPosition::Return, paths) {
        Ok(ty) => ty,
        Err(reason) => return Some(Err(format!("{name}: unsupported return type: {reason}"))),
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
    let v: Value = serde_json::from_str(&content)
        .with_context(|| format!("failed to parse rustdoc json: {}", rustdoc_json.display()))?;

    let index = v
        .get("index")
        .and_then(Value::as_object)
        .ok_or_else(|| anyhow!("missing index in rustdoc json"))?;
    let paths = v.get("paths").and_then(Value::as_object);

    let root_id = match v.get("root").and_then(json_id_to_string) {
        Some(root) => root,
        None => infer_root_id(index, crate_name)?,
    };

    let root_item = index
        .get(&root_id)
        .ok_or_else(|| anyhow!("root item not found in rustdoc index"))?;
    let candidate_ids = collect_candidate_ids(root_item, &v, crate_name);

    let mut functions = Vec::new();
    let mut skipped = Vec::new();
    let crate_sanitized = sanitize_symbol(crate_name);

    for item_id in candidate_ids {
        let Some(item) = index.get(&item_id) else {
            continue;
        };

        match parse_public_function_item(item, &crate_sanitized, paths) {
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

pub fn resolve_rust_import(root_dir: &Path, crate_name: &str) -> anyhow::Result<RustImportOutput> {
    let project_root = project_root_from_root_dir(root_dir)?;
    let rust_manifest = project_root.join("rust").join("Cargo.toml");
    let package_name = read_package_name(&rust_manifest)?;
    if package_name != crate_name {
        return Err(anyhow!(
            "import rust::{crate_name} does not match rust/Cargo.toml package name `{package_name}`"
        ));
    }

    run_rustdoc_json(&rust_manifest)?;
    let rustdoc_json = rustdoc_json_path(project_root.join("rust").as_path(), crate_name)?;
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
    use serde_json::json;

    fn fundamental(kind: ir_type::FundamentalTypeKind) -> Rc<ir_type::Ty> {
        Rc::new(ir_type::make_fundamental_type(
            kind,
            ir_type::Mutability::Not,
        ))
    }

    fn rust_string_resolved_path_ty() -> Value {
        json!({
            "resolved_path": {
                "path": "String",
                "id": 1,
                "args": null
            }
        })
    }

    fn rust_string_paths() -> serde_json::Map<String, Value> {
        json!({
            "1": {
                "path": ["alloc", "string", "String"]
            }
        })
        .as_object()
        .unwrap()
        .clone()
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
            let parsed =
                parse_supported_ty(&json!({ "primitive": primitive }), TyPosition::Param, None)
                    .unwrap();
            assert_eq!(parsed, fundamental(expected), "primitive `{primitive}`");
        }
    }

    #[test]
    fn parse_supported_ty_accepts_unit_shapes() {
        assert_eq!(
            parse_supported_ty(&Value::Null, TyPosition::Param, None).unwrap(),
            Rc::new(ir_type::Ty::new_unit())
        );
        assert_eq!(
            parse_supported_ty(&json!({ "tuple": [] }), TyPosition::Param, None).unwrap(),
            Rc::new(ir_type::Ty::new_unit())
        );
    }

    #[test]
    fn parse_supported_ty_accepts_shared_borrowed_str_parameter() {
        let parsed = parse_supported_ty(
            &json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "str" }
                }
            }),
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
            parse_supported_ty(&json!({ "primitive": "char" }), TyPosition::Param, None).unwrap();
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
            &json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "str" }
                }
            }),
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
            &json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": true,
                    "type": { "primitive": "str" }
                }
            }),
            TyPosition::Param,
            None,
        )
        .unwrap_err();
        assert!(mut_str_err.contains("mutable borrowed string type `&mut str`"));

        let borrowed_ref_err = parse_supported_ty(
            &json!({
                "borrowed_ref": {
                    "lifetime": null,
                    "is_mutable": false,
                    "type": { "primitive": "i32" }
                }
            }),
            TyPosition::Param,
            None,
        )
        .unwrap_err();
        assert!(borrowed_ref_err.contains("borrowed reference type `&i32`"));

        let raw_ptr_err = parse_supported_ty(
            &json!({
                "raw_pointer": {
                    "is_mutable": false,
                    "type": { "primitive": "i8" }
                }
            }),
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
