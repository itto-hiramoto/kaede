use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use kaede_symbol::Symbol;

use crate::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    top::{Enum, Interface, Struct},
};

/// Duplicate the type, change the mutability, and return the duplicated type
pub fn change_mutability_dup(ty: Rc<Ty>, mutability: Mutability) -> Rc<Ty> {
    let mut duped = (*ty).clone();

    change_mutability(&mut duped, mutability);

    duped.into()
}

// Container element mutability (`Pointer`/`Array`/`Slice`/`Tuple`) is derived
// from the operand at each access site, not stored — so only `Reference`,
// which wraps heap-allocated UDTs, needs to propagate.
pub fn change_mutability(ty: &mut Ty, mutability: Mutability) {
    ty.mutability = mutability;

    if let TyKind::Reference(rty) = ty.kind.as_ref() {
        let mut new_refee_ty = Ty {
            kind: rty.refee_ty.kind.clone(),
            mutability,
        };

        change_mutability(&mut new_refee_ty, mutability);

        ty.kind = TyKind::Reference(ReferenceType {
            refee_ty: new_refee_ty.into(),
        })
        .into();
    }
}

pub fn wrap_in_ref(ty: Rc<Ty>, mutability: Mutability) -> Ty {
    Ty {
        kind: TyKind::Reference(ReferenceType { refee_ty: ty }).into(),
        mutability,
    }
}

/// No mutability comparisons
pub fn is_same_type(t1: &Ty, t2: &Ty) -> bool {
    if matches!(t1.kind.as_ref(), TyKind::Var(_)) || matches!(t2.kind.as_ref(), TyKind::Var(_)) {
        return true;
    }

    if t1.kind == t2.kind {
        return true;
    }

    // True if either one is never type
    if matches!(t1.kind.as_ref(), TyKind::Never) || matches!(t2.kind.as_ref(), TyKind::Never) {
        return true;
    }

    false
}

fn is_exact_same_type(t1: &Ty, t2: &Ty) -> bool {
    match (t1.kind.as_ref(), t2.kind.as_ref()) {
        (TyKind::Fundamental(lhs), TyKind::Fundamental(rhs)) => lhs == rhs,
        (TyKind::UserDefined(lhs), TyKind::UserDefined(rhs)) => lhs == rhs,
        (TyKind::Closure(lhs), TyKind::Closure(rhs)) => {
            lhs.param_tys.len() == rhs.param_tys.len()
                && lhs
                    .param_tys
                    .iter()
                    .zip(rhs.param_tys.iter())
                    .all(|(lhs, rhs)| is_exact_same_type(lhs, rhs))
                && is_exact_same_type(&lhs.ret_ty, &rhs.ret_ty)
                && lhs.captures.len() == rhs.captures.len()
                && lhs
                    .captures
                    .iter()
                    .zip(rhs.captures.iter())
                    .all(|(lhs, rhs)| is_exact_same_type(lhs, rhs))
        }
        (TyKind::Reference(lhs), TyKind::Reference(rhs)) => {
            is_exact_same_type(&lhs.refee_ty, &rhs.refee_ty)
        }
        (TyKind::Pointer(lhs), TyKind::Pointer(rhs)) => {
            is_exact_same_type(&lhs.pointee_ty, &rhs.pointee_ty)
        }
        (TyKind::Slice(lhs), TyKind::Slice(rhs)) => is_exact_same_type(lhs, rhs),
        (TyKind::Array((lhs, lhs_size)), TyKind::Array((rhs, rhs_size))) => {
            lhs_size == rhs_size && is_exact_same_type(lhs, rhs)
        }
        (TyKind::Tuple(lhs), TyKind::Tuple(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| is_exact_same_type(lhs, rhs))
        }
        (TyKind::Var(lhs), TyKind::Var(rhs)) => lhs == rhs,
        (TyKind::Unit, TyKind::Unit) | (TyKind::Never, TyKind::Never) => true,
        _ => false,
    }
}

pub fn contains_type_var(ty: &Rc<Ty>) -> bool {
    match ty.kind.as_ref() {
        TyKind::Var(_) => true,
        TyKind::Pointer(pty) => contains_type_var(&pty.pointee_ty),
        TyKind::Reference(rty) => contains_type_var(&rty.refee_ty),
        TyKind::Slice(elem) => contains_type_var(elem),
        TyKind::Array((elem, _)) => contains_type_var(elem),
        TyKind::Tuple(elems) => elems.iter().any(contains_type_var),
        TyKind::Closure(closure) => {
            closure.param_tys.iter().any(contains_type_var)
                || contains_type_var(&closure.ret_ty)
                || closure.captures.iter().any(contains_type_var)
        }
        TyKind::UserDefined(udt) => udt
            .generic_instance
            .as_ref()
            .is_some_and(GenericInstanceInfo::contains_type_var),
        TyKind::Fundamental(_) | TyKind::Unit | TyKind::Never => false,
    }
}

/// True if `ty` mentions `interface_name` anywhere in its tree.
pub fn contains_interface(ty: &Rc<Ty>, interface_name: &QualifiedSymbol) -> bool {
    match ty.kind.as_ref() {
        TyKind::UserDefined(udt) => {
            udt.is_interface_with_name(interface_name)
                || udt.generic_instance.as_ref().is_some_and(|inst| {
                    inst.args
                        .iter()
                        .any(|a| contains_interface(a, interface_name))
                })
        }
        TyKind::Pointer(pty) => contains_interface(&pty.pointee_ty, interface_name),
        TyKind::Reference(rty) => contains_interface(&rty.refee_ty, interface_name),
        TyKind::Slice(elem) => contains_interface(elem, interface_name),
        TyKind::Array((elem, _)) => contains_interface(elem, interface_name),
        TyKind::Tuple(elems) => elems
            .iter()
            .any(|t| contains_interface(t, interface_name)),
        TyKind::Closure(c) => {
            c.param_tys
                .iter()
                .any(|t| contains_interface(t, interface_name))
                || contains_interface(&c.ret_ty, interface_name)
                || c.captures
                    .iter()
                    .any(|t| contains_interface(t, interface_name))
        }
        TyKind::Fundamental(_) | TyKind::Var(_) | TyKind::Unit | TyKind::Never => false,
    }
}

/// Replace every reference to `interface_name` inside `ty` with `impl_ty`.
/// Used when checking that an `impl T { ... }` block conforms to an
/// interface whose method signatures mention the interface itself in
/// parameter or return positions (e.g. `fun eq(self, other: Hashable)`):
/// the interface name in those positions is rewritten to `T` before the
/// signature comparison so the impl can write the concrete type directly.
pub fn substitute_interface(
    ty: &Rc<Ty>,
    interface_name: &QualifiedSymbol,
    impl_ty: &Rc<Ty>,
) -> Rc<Ty> {
    if !contains_interface(ty, interface_name) {
        return ty.clone();
    }
    match ty.kind.as_ref() {
        TyKind::UserDefined(udt) => {
            if udt.is_interface_with_name(interface_name) {
                return impl_ty.clone();
            }
            let substituted_instance = udt.generic_instance.as_ref().map(|instance| {
                GenericInstanceInfo::new(
                    instance.origin.clone(),
                    instance
                        .args
                        .iter()
                        .map(|arg| substitute_interface(arg, interface_name, impl_ty))
                        .collect(),
                )
            });
            Rc::new(Ty {
                kind: TyKind::UserDefined(UserDefinedType {
                    kind: udt.kind.clone(),
                    generic_instance: substituted_instance,
                })
                .into(),
                mutability: ty.mutability,
            })
        }
        TyKind::Pointer(pty) => Rc::new(Ty {
            kind: TyKind::Pointer(PointerType {
                pointee_ty: substitute_interface(
                    &pty.pointee_ty,
                    interface_name,
                    impl_ty,
                ),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Reference(rty) => {
            // The analyzer auto-wraps a UDT parameter type in `Reference`.
            // If the inner type is the interface we are substituting,
            // replace the whole `Reference` with `impl_ty` so that
            // conformance against impls on fundamental types (which are
            // not reference-wrapped) succeeds.
            if matches!(
                rty.refee_ty.kind.as_ref(),
                TyKind::UserDefined(udt) if udt.is_interface_with_name(interface_name)
            ) {
                return impl_ty.clone();
            }
            Rc::new(Ty {
                kind: TyKind::Reference(ReferenceType {
                    refee_ty: substitute_interface(
                        &rty.refee_ty,
                        interface_name,
                        impl_ty,
                    ),
                })
                .into(),
                mutability: ty.mutability,
            })
        }
        TyKind::Slice(elem) => Rc::new(Ty {
            kind: TyKind::Slice(substitute_interface(
                elem,
                interface_name,
                impl_ty,
            ))
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Array((elem, size)) => Rc::new(Ty {
            kind: TyKind::Array((
                substitute_interface(elem, interface_name, impl_ty),
                *size,
            ))
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Tuple(elems) => Rc::new(Ty {
            kind: TyKind::Tuple(
                elems
                    .iter()
                    .map(|t| substitute_interface(t, interface_name, impl_ty))
                    .collect(),
            )
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Closure(closure) => Rc::new(Ty {
            kind: TyKind::Closure(ClosureType {
                param_tys: closure
                    .param_tys
                    .iter()
                    .map(|t| substitute_interface(t, interface_name, impl_ty))
                    .collect(),
                ret_ty: substitute_interface(&closure.ret_ty, interface_name, impl_ty),
                captures: closure
                    .captures
                    .iter()
                    .map(|t| substitute_interface(t, interface_name, impl_ty))
                    .collect(),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Fundamental(_) | TyKind::Var(_) | TyKind::Unit | TyKind::Never => ty.clone(),
    }
}

pub fn apply_type_var_bindings(ty: &Rc<Ty>, bindings: &HashMap<VarId, Rc<Ty>>) -> Rc<Ty> {
    match ty.kind.as_ref() {
        TyKind::Var(id) => bindings.get(id).cloned().unwrap_or_else(|| ty.clone()),
        TyKind::Pointer(pty) => Rc::new(Ty {
            kind: TyKind::Pointer(PointerType {
                pointee_ty: apply_type_var_bindings(&pty.pointee_ty, bindings),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Reference(rty) => Rc::new(Ty {
            kind: TyKind::Reference(ReferenceType {
                refee_ty: apply_type_var_bindings(&rty.refee_ty, bindings),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Slice(elem) => Rc::new(Ty {
            kind: TyKind::Slice(apply_type_var_bindings(elem, bindings)).into(),
            mutability: ty.mutability,
        }),
        TyKind::Array((elem, size)) => Rc::new(Ty {
            kind: TyKind::Array((apply_type_var_bindings(elem, bindings), *size)).into(),
            mutability: ty.mutability,
        }),
        TyKind::Tuple(elems) => Rc::new(Ty {
            kind: TyKind::Tuple(
                elems
                    .iter()
                    .map(|t| apply_type_var_bindings(t, bindings))
                    .collect(),
            )
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Closure(closure) => Rc::new(Ty {
            kind: TyKind::Closure(ClosureType {
                param_tys: closure
                    .param_tys
                    .iter()
                    .map(|t| apply_type_var_bindings(t, bindings))
                    .collect(),
                ret_ty: apply_type_var_bindings(&closure.ret_ty, bindings),
                captures: closure
                    .captures
                    .iter()
                    .map(|t| apply_type_var_bindings(t, bindings))
                    .collect(),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::UserDefined(udt) => Rc::new(Ty {
            kind: TyKind::UserDefined(UserDefinedType {
                kind: udt.kind.clone(),
                generic_instance: udt.generic_instance.as_ref().map(|instance| {
                    GenericInstanceInfo::new(
                        instance.origin.clone(),
                        instance
                            .args
                            .iter()
                            .map(|arg| apply_type_var_bindings(arg, bindings))
                            .collect(),
                    )
                }),
            })
            .into(),
            mutability: ty.mutability,
        }),
        TyKind::Fundamental(_) | TyKind::Unit | TyKind::Never => ty.clone(),
    }
}

#[derive(Debug, Eq, Clone)]
pub struct Ty {
    pub kind: Rc<TyKind>,
    pub mutability: Mutability,
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        is_same_type(self, other)
    }
}

impl Ty {
    pub fn new_unit() -> Self {
        Self {
            kind: TyKind::Unit.into(),
            mutability: Mutability::Not,
        }
    }

    pub fn new_never() -> Self {
        Self {
            kind: TyKind::Never.into(),
            mutability: Mutability::Not,
        }
    }

    pub fn new_str(mutability: Mutability) -> Self {
        Self {
            kind: TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::Str,
            })
            .into(),
            mutability,
        }
    }

    /// Return true if it is a user-defined type
    pub fn is_udt(&self) -> bool {
        match self.kind.as_ref() {
            TyKind::Reference(rty) => {
                matches!(rty.get_base_type().kind.as_ref(), TyKind::UserDefined(_))
            }
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self.kind.as_ref() {
            TyKind::Reference(rty) => {
                matches!(
                    rty.get_base_type().kind.as_ref(),
                    TyKind::UserDefined(UserDefinedType {
                        kind: UserDefinedTypeKind::Enum(_),
                        ..
                    })
                )
            }
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        if let TyKind::Reference(rty) = self.kind.as_ref() {
            if let TyKind::Fundamental(fty) = rty.refee_ty.kind.as_ref() {
                return matches!(fty.kind, FundamentalTypeKind::Str);
            }
        }

        false
    }

    pub fn wrap_in_pointer(wrapped: Rc<Ty>) -> Ty {
        Ty {
            kind: TyKind::Pointer(PointerType {
                pointee_ty: wrapped,
            })
            .into(),
            mutability: Mutability::Not,
        }
    }
}

/// Represents whether a value can be changed
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Mutability {
    Not,
    Mut,
}

impl From<bool> for Mutability {
    fn from(value: bool) -> Self {
        if value {
            Mutability::Mut
        } else {
            Mutability::Not
        }
    }
}

impl From<kaede_ast_type::Mutability> for Mutability {
    fn from(value: kaede_ast_type::Mutability) -> Self {
        match value {
            kaede_ast_type::Mutability::Mut => Mutability::Mut,
            kaede_ast_type::Mutability::Not => Mutability::Not,
        }
    }
}
impl Mutability {
    /// Return `true` if self is mutable
    pub fn is_mut(self) -> bool {
        matches!(self, Self::Mut)
    }

    /// Return `true` if self is **not** mutable
    pub fn is_not(self) -> bool {
        matches!(self, Self::Not)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum FundamentalTypeKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    Str,
    Char,
}

impl std::fmt::Display for FundamentalTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FundamentalTypeKind::*;

        match self {
            I8 => write!(f, "i8"),
            U8 => write!(f, "u8"),
            I16 => write!(f, "i16"),
            U16 => write!(f, "u16"),
            I32 => write!(f, "i32"),
            U32 => write!(f, "u32"),
            I64 => write!(f, "i64"),
            U64 => write!(f, "u64"),

            F32 => write!(f, "f32"),
            F64 => write!(f, "f64"),

            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
            Char => write!(f, "char"),
        }
    }
}

impl FundamentalTypeKind {
    /// Returns true if this type is an integer type
    pub fn is_int(&self) -> bool {
        match self {
            // Integer types
            FundamentalTypeKind::I8
            | FundamentalTypeKind::U8
            | FundamentalTypeKind::I16
            | FundamentalTypeKind::U16
            | FundamentalTypeKind::I32
            | FundamentalTypeKind::U32
            | FundamentalTypeKind::I64
            | FundamentalTypeKind::U64 => true,

            // Non-integer types
            FundamentalTypeKind::F32
            | FundamentalTypeKind::F64
            | FundamentalTypeKind::Bool
            | FundamentalTypeKind::Str
            | FundamentalTypeKind::Char => false,
        }
    }

    /// Returns true if this type is a floating-point type
    pub fn is_float(&self) -> bool {
        matches!(self, FundamentalTypeKind::F32 | FundamentalTypeKind::F64)
    }

    /// Bit width of this floating-point type, or `None` for non-floats.
    pub fn float_bit_width(&self) -> Option<u32> {
        match self {
            FundamentalTypeKind::F32 => Some(32),
            FundamentalTypeKind::F64 => Some(64),
            _ => None,
        }
    }
}

pub fn make_fundamental_type(kind: FundamentalTypeKind, mutability: Mutability) -> Ty {
    Ty {
        kind: TyKind::Fundamental(FundamentalType { kind }).into(),
        mutability,
    }
}

pub type VarId = usize;

#[derive(Debug, Clone)]
pub struct GenericInstanceInfo {
    pub origin: QualifiedSymbol,
    pub args: Vec<Rc<Ty>>,
}

impl GenericInstanceInfo {
    pub fn new(origin: QualifiedSymbol, args: Vec<Rc<Ty>>) -> Self {
        Self { origin, args }
    }

    pub fn contains_type_var(&self) -> bool {
        self.args.iter().any(contains_type_var)
    }

    pub fn collect_var_ids_in_order(&self) -> Vec<VarId> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();

        for arg in &self.args {
            collect_type_var_ids(arg, &mut out, &mut seen);
        }

        out
    }
}

fn collect_type_var_ids(ty: &Rc<Ty>, out: &mut Vec<VarId>, seen: &mut HashSet<VarId>) {
    match ty.kind.as_ref() {
        TyKind::Var(id) => {
            if seen.insert(*id) {
                out.push(*id);
            }
        }
        TyKind::Pointer(pty) => collect_type_var_ids(&pty.pointee_ty, out, seen),
        TyKind::Reference(rty) => collect_type_var_ids(&rty.refee_ty, out, seen),
        TyKind::Slice(elem) => collect_type_var_ids(elem, out, seen),
        TyKind::Array((elem, _)) => collect_type_var_ids(elem, out, seen),
        TyKind::Tuple(elems) => {
            for elem in elems {
                collect_type_var_ids(elem, out, seen);
            }
        }
        TyKind::Closure(closure) => {
            for param in &closure.param_tys {
                collect_type_var_ids(param, out, seen);
            }
            collect_type_var_ids(&closure.ret_ty, out, seen);
            for capture in &closure.captures {
                collect_type_var_ids(capture, out, seen);
            }
        }
        TyKind::UserDefined(udt) => {
            if let Some(instance) = &udt.generic_instance {
                for arg in &instance.args {
                    collect_type_var_ids(arg, out, seen);
                }
            }
        }
        TyKind::Fundamental(_) | TyKind::Unit | TyKind::Never => {}
    }
}

impl PartialEq for GenericInstanceInfo {
    fn eq(&self, other: &Self) -> bool {
        self.origin == other.origin
            && self.args.len() == other.args.len()
            && self
                .args
                .iter()
                .zip(other.args.iter())
                .all(|(lhs, rhs)| is_exact_same_type(lhs, rhs))
    }
}

impl Eq for GenericInstanceInfo {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyKind {
    Fundamental(FundamentalType),

    UserDefined(UserDefinedType),

    Closure(ClosureType),

    Reference(ReferenceType),

    Pointer(PointerType),

    Slice(Rc<Ty> /* Element type */),

    Array((Rc<Ty> /* Element type */, u32 /* Size */)),

    Tuple(Vec<Rc<Ty>> /* Element types */),

    // Inferred type
    Var(VarId),

    Unit,

    /// Same as Rust's never type
    Never,
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fundamental(fty) => write!(f, "{}", fty.kind),

            Self::UserDefined(udt) => write!(f, "{udt}"),

            Self::Closure(closure) => {
                let params = closure
                    .param_tys
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fun({params}) -> {}", closure.ret_ty.kind)
            }

            Self::Reference(refee) => write!(f, "&{}", refee.refee_ty.kind),

            Self::Pointer(pty) => write!(f, "*{}", pty.pointee_ty.kind),

            Self::Slice(elem_ty) => write!(f, "[{}]", elem_ty.kind),

            Self::Array((elem_ty, size)) => write!(f, "[{}; {}]", elem_ty.kind, size),

            Self::Tuple(elem_tys) => write!(
                f,
                "({})",
                elem_tys
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Var(_) => write!(f, "_"),

            Self::Unit => write!(f, "()"),

            Self::Never => write!(f, "!"),
        }
    }
}

impl TyKind {
    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),
            Self::UserDefined(_) => todo!(),
            Self::Closure(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_signed(),

            Self::Pointer(_) => panic!("Cannot get sign information of pointer type!"),
            Self::Slice(_) => panic!("Cannot get sign information of slice type!"),
            Self::Array(_) => panic!("Cannot get sign information of array type!"),
            Self::Tuple(_) => panic!("Cannot get sign information of tuple type!"),
            Self::Unit => panic!("Cannot get sign information of unit type!"),
            Self::Never => panic!("Cannot get sign information of never type!"),

            Self::Var(_) => unreachable!(),
        }
    }

    pub fn is_int_or_bool(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_int_or_char_or_bool(),
            Self::UserDefined(_) => todo!(),
            Self::Closure(_) => false,
            Self::Reference(ty) => ty.refee_ty.kind.is_int_or_bool(),

            Self::Array(_)
            | Self::Slice(_)
            | Self::Tuple(_)
            | Self::Pointer(_)
            | Self::Unit
            | Self::Never => false,

            Self::Var(_) => unreachable!(),
        }
    }

    pub fn can_be_arithmetic_operand(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.can_be_arithmetic_operand(),
            Self::Reference(ty) => ty.refee_ty.kind.can_be_arithmetic_operand(),

            Self::UserDefined(_)
            | Self::Closure(_)
            | Self::Array(_)
            | Self::Slice(_)
            | Self::Tuple(_)
            | Self::Pointer(_)
            | Self::Unit
            | Self::Never => false,

            Self::Var(_) => unreachable!(),
        }
    }

    pub fn is_float(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.kind.is_float(),
            Self::Reference(ty) => ty.refee_ty.kind.is_float(),

            Self::UserDefined(_)
            | Self::Closure(_)
            | Self::Array(_)
            | Self::Slice(_)
            | Self::Tuple(_)
            | Self::Pointer(_)
            | Self::Unit
            | Self::Never => false,

            Self::Var(_) => false,
        }
    }

    /// Bit width of this floating-point type, or `None` for non-floats.
    pub fn float_bit_width(&self) -> Option<u32> {
        match self {
            Self::Fundamental(fty) => fty.kind.float_bit_width(),
            Self::Reference(ty) => ty.refee_ty.kind.float_bit_width(),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FundamentalType {
    pub kind: FundamentalTypeKind,
}

#[derive(Debug, Clone)]
pub struct ClosureType {
    pub param_tys: Vec<Rc<Ty>>,
    pub ret_ty: Rc<Ty>,
    pub captures: Vec<Rc<Ty>>,
}

impl PartialEq for ClosureType {
    fn eq(&self, other: &Self) -> bool {
        self.param_tys
            .iter()
            .zip(other.param_tys.iter())
            .all(|(l, r)| is_same_type(l, r))
            && is_same_type(&self.ret_ty, &other.ret_ty)
            && self
                .captures
                .iter()
                .zip(other.captures.iter())
                .all(|(l, r)| is_same_type(l, r))
    }
}

impl Eq for ClosureType {}

impl FundamentalType {
    pub fn create_llvm_str_type(context: &Context) -> BasicTypeEnum<'_> {
        let str_ty = context.ptr_type(AddressSpace::default());
        let len_ty = context.i64_type();
        // { *u8, i64 }
        context
            .struct_type(&[str_ty.into(), len_ty.into()], true)
            .into()
    }

    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 => context.i8_type().as_basic_type_enum(),
            U8 => context.i8_type().as_basic_type_enum(),
            I16 => context.i16_type().as_basic_type_enum(),
            U16 => context.i16_type().as_basic_type_enum(),
            I32 => context.i32_type().as_basic_type_enum(),
            U32 => context.i32_type().as_basic_type_enum(),
            I64 => context.i64_type().as_basic_type_enum(),
            U64 => context.i64_type().as_basic_type_enum(),
            F32 => context.f32_type().as_basic_type_enum(),
            F64 => context.f64_type().as_basic_type_enum(),
            Char => context.i32_type().as_basic_type_enum(),
            Bool => context.bool_type().as_basic_type_enum(),
            Str => Self::create_llvm_str_type(context),
        }
    }

    pub fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | I16 | I32 | I64 => true,
            U8 | U16 | U32 | U64 => false,
            F32 | F64 => true,
            Bool => false,
            Str => false,
            Char => false,
        }
    }

    pub fn is_int_or_char_or_bool(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Bool | Char => true,
            F32 | F64 => false,
            Str => false,
        }
    }

    pub fn can_be_arithmetic_operand(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Char => true,
            F32 | F64 => true,
            Bool => true,
            Str => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PointerType {
    pub pointee_ty: Rc<Ty>,
}

impl PointerType {
    pub fn get_base_type(&self) -> Rc<Ty> {
        if let TyKind::Pointer(pty) = self.pointee_ty.kind.as_ref() {
            return pty.get_base_type();
        }

        self.pointee_ty.clone()
    }
}

impl PartialEq for PointerType {
    fn eq(&self, other: &Self) -> bool {
        is_same_type(&self.pointee_ty, &other.pointee_ty)
    }
}

impl Eq for PointerType {}

#[derive(Debug, Clone)]
pub enum UserDefinedTypeKind {
    Struct(Rc<Struct>),
    Enum(Rc<Enum>),
    Interface(Rc<Interface>),
    // For circular dependency
    Placeholder(QualifiedSymbol),
}

#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub kind: UserDefinedTypeKind,
    pub generic_instance: Option<GenericInstanceInfo>,
}

impl UserDefinedType {
    pub fn new(kind: UserDefinedTypeKind) -> Self {
        Self {
            kind,
            generic_instance: None,
        }
    }

    pub fn with_generic_instance(
        kind: UserDefinedTypeKind,
        generic_instance: GenericInstanceInfo,
    ) -> Self {
        Self {
            kind,
            generic_instance: Some(generic_instance),
        }
    }

    pub fn module_path(&self) -> ModulePath {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.module_path().clone(),
            UserDefinedTypeKind::Enum(e) => e.name.module_path().clone(),
            UserDefinedTypeKind::Interface(i) => i.name.module_path().clone(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.module_path().clone(),
        }
    }

    pub fn name(&self) -> Symbol {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.symbol(),
            UserDefinedTypeKind::Enum(e) => e.name.symbol(),
            UserDefinedTypeKind::Interface(i) => i.name.symbol(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.symbol(),
        }
    }

    pub fn qualified_symbol(&self) -> QualifiedSymbol {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.clone(),
            UserDefinedTypeKind::Enum(e) => e.name.clone(),
            UserDefinedTypeKind::Interface(i) => i.name.clone(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.clone(),
        }
    }

    pub fn is_interface(&self) -> bool {
        matches!(self.kind, UserDefinedTypeKind::Interface(_))
    }

    /// True if this UDT is the interface (or its placeholder) whose
    /// qualified name equals `name`.
    pub fn is_interface_with_name(&self, name: &QualifiedSymbol) -> bool {
        match &self.kind {
            UserDefinedTypeKind::Interface(i) => &i.name == name,
            UserDefinedTypeKind::Placeholder(qsym) => qsym == name,
            _ => false,
        }
    }
}

impl Display for UserDefinedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl PartialEq for UserDefinedType {
    fn eq(&self, other: &Self) -> bool {
        self.qualified_symbol() == other.qualified_symbol()
            && self.generic_instance == other.generic_instance
    }
}

impl Eq for UserDefinedType {}

#[derive(Debug, Eq, Clone)]
pub struct ReferenceType {
    pub refee_ty: Rc<Ty>,
}

impl PartialEq for ReferenceType {
    // No mutability comparisons
    fn eq(&self, other: &Self) -> bool {
        *self.refee_ty.kind == *other.refee_ty.kind
    }
}

impl ReferenceType {
    pub fn new(refee_ty: Rc<Ty>) -> Self {
        Self { refee_ty }
    }

    /// &i32 -> i32
    ///
    /// &&i32 -> i32
    pub fn get_base_type(&self) -> Rc<Ty> {
        match self.refee_ty.kind.as_ref() {
            TyKind::Reference(rty) => rty.get_base_type(),

            _ => self.refee_ty.clone(),
        }
    }
}
