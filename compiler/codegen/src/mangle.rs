use kaede_symbol::Symbol;
use kaede_ast_type::UserDefinedType;

use crate::CompileUnitCtx;

/// Mangled by the current module name
pub fn mangle_name(cucx: &CompileUnitCtx, name: Symbol) -> String {
    format!("{}.{}", cucx.mangle_prefix().unwrap(), name)
}

pub fn mangle_method(cucx: &CompileUnitCtx, impl_for: &UserDefinedType, name: Symbol) -> String {
    format!("{}.{}", mangle_udt_name(cucx, impl_for), name)
}

pub fn mangle_static_method(
    cucx: &CompileUnitCtx,
    impl_for: &UserDefinedType,
    name: Symbol,
) -> String {
    format!("{}::{}", mangle_udt_name(cucx, impl_for), name)
}

pub fn mangle_udt_name(cucx: &CompileUnitCtx, udt: &UserDefinedType) -> Symbol {
    if let Some(generic_args) = &udt.generic_args {
        format!(
            "{}-{}",
            mangle_name(cucx, udt.name.symbol()),
            generic_args
                .types
                .iter()
                .map(|t| {
                    if let Some(ty) = cucx.tcx.lookup_generic_arg(t.kind.to_string().into()) {
                        ty.kind.to_string()
                    } else {
                        t.kind.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("_")
        )
    } else {
        mangle_name(cucx, udt.name.symbol())
    }
    .into()
}
