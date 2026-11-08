use std::{convert::Infallible, rc::Rc};

use kaede_ir::{
    self as ir, ty as ir_type,
    visit::{walk_fn_decl, MutVisitor},
};

/// Substitutes the generic parameters of `origin` with `args` throughout a declaration.
pub(crate) struct GenericSubstituter<'a> {
    origin: &'a ir::qualified_symbol::QualifiedSymbol,
    args: &'a [Rc<ir_type::Ty>],
}

impl<'a> GenericSubstituter<'a> {
    pub(crate) fn for_generic_params(
        origin: &'a ir::qualified_symbol::QualifiedSymbol,
        args: &'a [Rc<ir_type::Ty>],
    ) -> Self {
        Self { origin, args }
    }

    pub(crate) fn apply_fn_decl(mut self, decl: &mut ir::top::FnDecl) {
        let Ok(()) = walk_fn_decl(&mut self, decl);
    }
}

impl MutVisitor for GenericSubstituter<'_> {
    type Error = Infallible;

    fn visit_ty(&mut self, ty: &mut Rc<ir_type::Ty>) -> Result<(), Self::Error> {
        *ty = ir_type::substitute_generic_params(ty, self.origin, self.args);
        Ok(())
    }
}
