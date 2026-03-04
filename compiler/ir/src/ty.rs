use std::{collections::HashMap, fmt::Display, rc::Rc};

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use kaede_symbol::Symbol;

use crate::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    top::{Enum, Struct},
};

/// Duplicate the type, change the mutability, and return the duplicated type
pub fn change_mutability_dup(ty: Rc<Ty>, mutability: Mutability) -> Rc<Ty> {
    let mut duped = (*ty).clone();

    change_mutability(&mut duped, mutability);

    duped.into()
}

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
        TyKind::Fundamental(_) | TyKind::UserDefined(_) | TyKind::Unit | TyKind::Never => false,
    }
}

pub fn collect_type_var_bindings(
    pattern: &Rc<Ty>,
    resolved: &Rc<Ty>,
    out: &mut HashMap<VarId, Rc<Ty>>,
) {
    match (pattern.kind.as_ref(), resolved.kind.as_ref()) {
        (TyKind::Var(id), _) => {
            out.insert(*id, resolved.clone());
        }
        (TyKind::Pointer(p1), TyKind::Pointer(p2)) => {
            collect_type_var_bindings(&p1.pointee_ty, &p2.pointee_ty, out);
        }
        (TyKind::Reference(r1), TyKind::Reference(r2)) => {
            collect_type_var_bindings(&r1.refee_ty, &r2.refee_ty, out);
        }
        (TyKind::Slice(e1), TyKind::Slice(e2)) => {
            collect_type_var_bindings(e1, e2, out);
        }
        (TyKind::Array((e1, _)), TyKind::Array((e2, _))) => {
            collect_type_var_bindings(e1, e2, out);
        }
        (TyKind::Tuple(ts1), TyKind::Tuple(ts2)) => {
            for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                collect_type_var_bindings(t1, t2, out);
            }
        }
        (TyKind::Closure(c1), TyKind::Closure(c2)) => {
            for (p1, p2) in c1.param_tys.iter().zip(c2.param_tys.iter()) {
                collect_type_var_bindings(p1, p2, out);
            }
            collect_type_var_bindings(&c1.ret_ty, &c2.ret_ty, out);
            for (cap1, cap2) in c1.captures.iter().zip(c2.captures.iter()) {
                collect_type_var_bindings(cap1, cap2, out);
            }
        }
        _ => {}
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
        TyKind::Fundamental(_) | TyKind::UserDefined(_) | TyKind::Unit | TyKind::Never => {
            ty.clone()
        }
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
            FundamentalTypeKind::Bool | FundamentalTypeKind::Str | FundamentalTypeKind::Char => {
                false
            }
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
                write!(f, "fn({params}) -> {}", closure.ret_ty.kind)
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
        // { *i8, i64 }
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
            Char => context.i8_type().as_basic_type_enum(),
            Bool => context.bool_type().as_basic_type_enum(),
            Str => Self::create_llvm_str_type(context),
        }
    }

    pub fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | I16 | I32 | I64 => true,
            U8 | U16 | U32 | U64 => false,
            Bool => false,
            Str => false,
            Char => true,
        }
    }

    pub fn is_int_or_char_or_bool(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Bool | Char => true,
            Str => false,
        }
    }

    pub fn can_be_arithmetic_operand(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Char => true,
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
    // For circular dependency
    Placeholder(QualifiedSymbol),
}

#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub kind: UserDefinedTypeKind,
}

impl UserDefinedType {
    pub fn new(kind: UserDefinedTypeKind) -> Self {
        Self { kind }
    }

    pub fn module_path(&self) -> ModulePath {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.module_path().clone(),
            UserDefinedTypeKind::Enum(e) => e.name.module_path().clone(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.module_path().clone(),
        }
    }

    pub fn name(&self) -> Symbol {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.symbol(),
            UserDefinedTypeKind::Enum(e) => e.name.symbol(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.symbol(),
        }
    }

    pub fn qualified_symbol(&self) -> QualifiedSymbol {
        match &self.kind {
            UserDefinedTypeKind::Struct(s) => s.name.clone(),
            UserDefinedTypeKind::Enum(e) => e.name.clone(),
            UserDefinedTypeKind::Placeholder(qsym) => qsym.clone(),
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
