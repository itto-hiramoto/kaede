use std::{fmt::Display, rc::Rc};

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use kaede_span::Span;
use kaede_symbol::Ident;

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
            span: ty.span,
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
        span: ty.span,
        kind: TyKind::Reference(ReferenceType { refee_ty: ty }).into(),
        mutability,
    }
}

/// No mutability comparisons
pub fn is_same_type(t1: &Ty, t2: &Ty) -> bool {
    if t1.kind == t2.kind {
        return true;
    }

    // True if either one is never type
    if matches!(t1.kind.as_ref(), TyKind::Never) || matches!(t2.kind.as_ref(), TyKind::Never) {
        return true;
    }

    false
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    pub kind: Rc<TyKind>,
    pub mutability: Mutability,
    pub span: Span,
}

impl Ty {
    pub fn new_unit(span: Span) -> Self {
        Self {
            kind: TyKind::Unit.into(),
            mutability: Mutability::Not,
            span,
        }
    }

    pub fn new_never(span: Span) -> Self {
        Self {
            kind: TyKind::Never.into(),
            mutability: Mutability::Not,
            span,
        }
    }

    pub fn new_str(mutability: Mutability, span: Span) -> Self {
        Self {
            kind: TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::Str,
            })
            .into(),
            mutability,
            span,
        }
    }

    pub fn new_var(span: Span) -> Self {
        Self {
            kind: TyKind::Var.into(),
            mutability: Mutability::Not,
            span,
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

    pub fn is_str(&self) -> bool {
        if let TyKind::Reference(rty) = self.kind.as_ref() {
            if let TyKind::Fundamental(fty) = rty.refee_ty.kind.as_ref() {
                return matches!(fty.kind, FundamentalTypeKind::Str);
            }
        }

        false
    }

    pub fn is_generic(&self) -> bool {
        matches!(self.kind.as_ref(), TyKind::Generic(_))
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
    Char,
    Bool,
    Str,
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
            Char => write!(f, "char"),
            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
        }
    }
}

pub fn make_fundamental_type(kind: FundamentalTypeKind, mutability: Mutability, span: Span) -> Ty {
    Ty {
        kind: TyKind::Fundamental(FundamentalType { kind }).into(),
        mutability,
        span,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyKind {
    Fundamental(FundamentalType),

    UserDefined(UserDefinedType),

    Generic(GenericType),

    Closure(ClosureType),

    Reference(ReferenceType),

    Pointer(PointerType),

    Array((Rc<Ty> /* Element type */, u32 /* Size */)),

    Tuple(Vec<Rc<Ty>> /* Element types */),

    Unit,

    /// Same as Rust's never type
    Never,

    /// Inferred type
    Var,

    /// External type, e.g. `std.io.println`
    /// If the type appearing in the expression is an external type,
    /// it is basically handled as binary operators rather than here,
    /// so this is not used. (except for generic arguments, etc., where the type can be anticipated)
    External(Ty, Vec<Ident>),
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fundamental(fty) => write!(f, "{}", fty.kind),

            Self::UserDefined(udt) => write!(f, "{}", udt.name.symbol().as_str()),

            Self::Generic(gty) => write!(f, "{}", gty.name.as_str()),

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

            Self::Unit => write!(f, "()"),

            Self::Never => write!(f, "!"),

            Self::Var => write!(f, "_"),

            Self::External(ty, access_chain) => write!(
                f,
                "{}.{}",
                access_chain
                    .iter()
                    .map(|i| i.as_str())
                    .collect::<Vec<_>>()
                    .join("."),
                ty.kind
            ),
        }
    }
}

impl TyKind {
    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),
            Self::UserDefined(_) => todo!(),
            Self::Generic(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_signed(),
            Self::External(t, _) => t.kind.is_signed(),
            Self::Closure(_) => todo!(),

            Self::Pointer(_) => panic!("Cannot get sign information of pointer type!"),
            Self::Array(_) => panic!("Cannot get sign information of array type!"),
            Self::Tuple(_) => panic!("Cannot get sign information of tuple type!"),
            Self::Unit => panic!("Cannot get sign information of unit type!"),
            Self::Never => panic!("Cannot get sign information of never type!"),
            Self::Var => panic!("Cannot get sign information of inferred type!"),
        }
    }

    pub fn is_int_or_bool(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_int_or_bool(),
            Self::UserDefined(_) => todo!(),
            Self::Generic(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_int_or_bool(),
            Self::External(t, _) => t.kind.is_int_or_bool(),
            Self::Closure(_) => false,

            Self::Array(_)
            | Self::Tuple(_)
            | Self::Pointer(_)
            | Self::Unit
            | Self::Never
            | Self::Var => false,
        }
    }

    pub fn is_inferred(&self) -> bool {
        matches!(self, Self::Var)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FundamentalType {
    pub kind: FundamentalTypeKind,
}

impl FundamentalType {
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
            Str => {
                let str_ty = context.ptr_type(AddressSpace::default());
                let len_ty = context.i64_type();
                // { *i8, i64 }
                context
                    .struct_type(&[str_ty.into(), len_ty.into()], true)
                    .into()
            }
        }
    }

    pub fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | I16 | I32 | I64 => true,
            U8 | U16 | U32 | U64 => false,
            Char => true,
            Bool => false,
            Str => false,
        }
    }

    pub fn is_int_or_bool(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Bool | Char => true,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericArgs {
    pub types: Vec<Rc<Ty>>,
    pub span: Span,
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

#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub name: Ident,
    pub generic_args: Option<GenericArgs>,
}

impl UserDefinedType {
    pub fn new(name: Ident, generic_args: Option<GenericArgs>) -> Self {
        Self { name, generic_args }
    }
}

impl Display for UserDefinedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.generic_args.is_none() {
            write!(f, "{}", self.name.symbol().as_str())
        } else {
            write!(
                f,
                "{}<{}>",
                self.name.symbol().as_str(),
                self.generic_args
                    .as_ref()
                    .unwrap()
                    .types
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

impl PartialEq for UserDefinedType {
    fn eq(&self, other: &Self) -> bool {
        self.name.symbol() == other.name.symbol()
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

#[derive(Debug, Clone)]
pub struct GenericType {
    pub name: Ident,
}

impl PartialEq for GenericType {
    fn eq(&self, other: &Self) -> bool {
        self.name.symbol() == other.name.symbol()
    }
}

impl Eq for GenericType {}
