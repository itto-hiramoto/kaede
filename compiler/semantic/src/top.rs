use kaede_symbol::Symbol;

use crate::{
    symbol_table::{GenericFuncInfo, GenericInfo, SymbolTableValue},
    SemanticAnalyzer,
};

use kaede_ast as ast;
use kaede_ir as ir;

impl SemanticAnalyzer {
    /// Returns `None` if a top-level item is a generic.
    pub fn analyze_top_level(
        &mut self,
        top_level: ast::top::TopLevel,
    ) -> anyhow::Result<Option<ir::top::TopLevel>> {
        use ast::top::TopLevelKind;

        match top_level.kind {
            TopLevelKind::Fn(node) => self.analyze_fn(node),
            TopLevelKind::Struct(_) => unimplemented!(),
            TopLevelKind::Import(_) => unimplemented!(),
            TopLevelKind::Impl(_) => unimplemented!(),
            TopLevelKind::Enum(_) => unimplemented!(),
            TopLevelKind::Extern(_) => unimplemented!(),
            TopLevelKind::Use(_) => unimplemented!(),

            _ => unreachable!(),
        }
    }

    /// Returns `None` if a function is generic.
    fn analyze_fn(&mut self, node: ast::top::Fn) -> anyhow::Result<Option<ir::top::TopLevel>> {
        assert_eq!(node.decl.self_, None);

        let mangled_name = if node.decl.name.as_str() == "main" {
            // Suppress mangling of main function.
            Symbol::from(String::from("kdmain"))
        } else {
            self.mangle_fn_name(node.decl.name.as_str())
        };

        // If the function is generic, register it in the symbol table and return early.
        if node.decl.generic_params.is_some() {
            let span = node.span;

            self.get_root_symbol_table().insert(
                mangled_name,
                SymbolTableValue::Generic(GenericInfo::Func(GenericFuncInfo { ast: node })),
                span,
            )?;

            // Generic functions are not generated immediately, but are generated when they are used.
            return Ok(None);
        }

        let fn_decl = ir::top::FnDecl {
            name: mangled_name,
            is_var_args: node.decl.params.is_var_args,
            params: node
                .decl
                .params
                .v
                .into_iter()
                .map(|p| ir::top::Param {
                    name: p.name.symbol(),
                    ty: p.ty,
                })
                .collect(),
            return_ty: node.decl.return_ty,
        };

        let fn_ = ir::top::Fn {
            decl: fn_decl,
            body: self.analyze_block(&node.body)?,
        };

        Ok(Some(ir::top::TopLevel::Fn(fn_)))
    }
}
