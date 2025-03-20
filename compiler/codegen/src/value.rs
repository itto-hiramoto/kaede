use std::rc::Rc;

use inkwell::values::BasicValueEnum;
use kaede_ast_type::{Mutability, Ty, TyKind};

/// The type information in llvm's value is **insufficient**, so wrap it with our own type
///
/// For example, there is **no** sign information in the integer value of llvm
#[derive(Debug, Clone)]
pub struct Value<'ctx> {
    /// None if void
    val: Option<BasicValueEnum<'ctx>>,
    /// None if void type
    ty: Option<Rc<Ty>>,
}

impl<'ctx> Value<'ctx> {
    pub fn new(val: BasicValueEnum<'ctx>, ty: Rc<Ty>) -> Self {
        Self {
            val: Some(val),
            ty: Some(ty),
        }
    }

    pub fn new_never() -> Self {
        Self {
            val: None,
            ty: Some(
                Ty {
                    kind: TyKind::Never.into(),
                    mutability: Mutability::Not,
                }
                .into(),
            ),
        }
    }

    pub fn new_unit() -> Self {
        Self {
            val: None,
            ty: Some(
                Ty {
                    kind: TyKind::Unit.into(),
                    mutability: Mutability::Not,
                }
                .into(),
            ),
        }
    }

    pub fn is_never_ty(&self) -> bool {
        matches!(self.get_type().kind.as_ref(), TyKind::Never)
    }

    /// Create a `Value` representing a void value of type void
    pub fn new_void() -> Self {
        Self {
            val: None,
            ty: None,
        }
    }

    /// If the value is `void`, this function **panics**
    pub fn get_value(&self) -> BasicValueEnum<'ctx> {
        match &self.val {
            Some(val) => *val,
            None => panic!("Value type is {}", self.get_type().kind),
        }
    }

    /// If the type is `void`, this function **panics**
    pub fn get_type(&self) -> Rc<Ty> {
        self.ty.as_ref().unwrap().clone()
    }
}

pub fn has_signed(vals: &[&Value]) -> bool {
    for val in vals {
        if val.get_type().kind.is_signed() {
            return true;
        }
    }

    false
}
