use std::rc::Rc;

use kaede_ast as ast;
use kaede_ir::ty as ir_type;
use kaede_symbol_table::GenericArgumentTable;

use crate::{error::SemanticError, SemanticAnalyzer};

impl SemanticAnalyzer {
    pub fn with_generic_arguments<T>(
        &mut self,
        generic_params: &ast::top::GenericParams,
        generic_args: &[Rc<ir_type::Ty>],
        f: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        // Check the length of the generic arguments
        if generic_params.names.len() != generic_args.len() {
            return Err(SemanticError::GenericArgumentLengthMismatch {
                expected: generic_params.names.len(),
                actual: generic_args.len(),
                span: generic_params.span,
            }
            .into());
        }

        let mut generic_argument_table = GenericArgumentTable::new();

        generic_argument_table.map.extend(
            generic_params
                .names
                .iter()
                .zip(generic_args.iter())
                .map(|(ident, ty)| (ident.symbol(), ty.clone())),
        );

        let current_module_path = self.current_module_path().clone();

        self.modules
            .get_mut(&current_module_path)
            .unwrap()
            .push_generic_argument_table(generic_argument_table);

        let result = f(self);

        // Remove the generic arguments
        self.modules
            .get_mut(&current_module_path)
            .unwrap()
            .pop_generic_argument_table();

        result
    }
}
