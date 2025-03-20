use std::rc::Rc;

use inkwell::values::BasicValue;
use kaede_ast::expr::ExprKind;
use kaede_ast::stmt::{
    Assign, AssignKind, Block, Let, LetKind, NormalLet, Stmt, StmtKind, TupleUnpack,
};
use kaede_ast_type::{
    change_mutability_dup, create_inferred_tuple, is_same_type, Mutability, Ty, TyKind,
};
use kaede_span::Span;
use kaede_symbol::Ident;

use crate::expr::build_tuple_indexing;
use crate::tcx::{SymbolTable, SymbolTableValue};
use crate::value::Value;
use crate::{error::CodegenError, expr::build_expression, get_loaded_pointer, CompileUnitCtx};

pub fn build_block(cucx: &mut CompileUnitCtx, block: &Block) -> anyhow::Result<()> {
    cucx.tcx.push_symbol_table(SymbolTable::new());

    for stmt in &block.body {
        build_statement(cucx, stmt)?;
    }

    cucx.tcx.pop_symbol_table();

    Ok(())
}

pub fn build_statement(cucx: &mut CompileUnitCtx, node: &Stmt) -> anyhow::Result<()> {
    let mut builder = StmtBuilder::new(cucx);

    builder.build(node)?;

    Ok(())
}

pub fn build_normal_let(
    cucx: &mut CompileUnitCtx,
    name: &Ident,
    mutability: Mutability,
    init: Option<Value<'_>>,
    specified_ty: Rc<Ty>,
    span: Span,
) -> anyhow::Result<()> {
    if let Some(init) = init {
        if matches!(init.get_type().kind.as_ref(), TyKind::Reference(_))
            && mutability.is_mut()
            && init.get_type().mutability.is_not()
        {
            return Err(CodegenError::CannotAssignImmutableToMutable { span }.into());
        }

        let var_ty = change_mutability_dup(init.get_type(), mutability);

        let alloca = if specified_ty.kind.is_inferred() {
            // No type information was available, so infer from an initializer
            let alloca = cucx.create_entry_block_alloca(name.as_str(), &var_ty, span)?;

            cucx.tcx.insert_symbol_to_current_scope(
                name.symbol(),
                SymbolTableValue::Variable((alloca, var_ty)),
                span,
            )?;

            alloca
        } else {
            // Type information is available

            // Check if an initializer type and type match
            if !is_same_type(&specified_ty, &init.get_type()) {
                return Err(CodegenError::MismatchedTypes {
                    types: (
                        specified_ty.kind.to_string(),
                        init.get_type().kind.to_string(),
                    ),
                    span,
                }
                .into());
            }

            let alloca = cucx.create_entry_block_alloca(name.as_str(), &var_ty, span)?;

            cucx.tcx.insert_symbol_to_current_scope(
                name.symbol(),
                SymbolTableValue::Variable((alloca, var_ty)),
                span,
            )?;

            alloca
        };

        // Initialization
        cucx.builder.build_store(alloca, init.get_value())?;

        return Ok(());
    }

    todo!()
}

struct StmtBuilder<'a, 'ctx> {
    cucx: &'a mut CompileUnitCtx<'ctx>,
}

impl<'a, 'ctx> StmtBuilder<'a, 'ctx> {
    fn new(cucx: &'a mut CompileUnitCtx<'ctx>) -> Self {
        Self { cucx }
    }

    /// Generate statement code
    fn build(&mut self, stmt: &Stmt) -> anyhow::Result<()> {
        match &stmt.kind {
            StmtKind::Expr(e) => {
                if matches!(e.kind, ExprKind::If(_) | ExprKind::Match(_)) {
                    self.cucx.is_ifmatch_stmt = true;
                    build_expression(self.cucx, e)?;
                    self.cucx.is_ifmatch_stmt = false;
                } else {
                    build_expression(self.cucx, e)?;
                }
            }

            StmtKind::Let(node) => self.let_(node)?,

            StmtKind::Assign(node) => self.assign(node)?,
        }

        Ok(())
    }

    fn assign(&mut self, node: &Assign) -> anyhow::Result<()> {
        let left = build_expression(self.cucx, &node.lhs)?;

        if left.get_type().mutability.is_not() {
            return Err(CodegenError::CannotAssignTwiceToImutable { span: node.span }.into());
        }

        let ptr_to_left =
            match get_loaded_pointer(&left.get_value().as_instruction_value().unwrap()) {
                Some(p) => p,
                None => {
                    return Err(CodegenError::InvalidLeftOfAssignment { span: node.span }.into())
                }
            };

        let right = build_expression(self.cucx, &node.rhs)?;

        if !is_same_type(&left.get_type(), &right.get_type()) {
            return Err(CodegenError::MismatchedTypes {
                types: (
                    left.get_type().kind.to_string(),
                    right.get_type().kind.to_string(),
                ),
                span: node.span,
            }
            .into());
        }

        match node.kind {
            AssignKind::Simple => self
                .cucx
                .builder
                .build_store(ptr_to_left, right.get_value())?,
        };

        Ok(())
    }

    fn let_(&mut self, node: &Let) -> anyhow::Result<()> {
        match &node.kind {
            LetKind::NormalLet(node) => self.normal_let(node),
            LetKind::TupleUnpack(node) => self.tuple_unpacking(node),
        }
    }

    fn build_normal_let(
        &mut self,
        name: &Ident,
        mutability: Mutability,
        init: Option<Value<'ctx>>,
        specified_ty: Rc<Ty>,
        span: Span,
    ) -> anyhow::Result<()> {
        build_normal_let(self.cucx, name, mutability, init, specified_ty, span)
    }

    fn normal_let(&mut self, node: &NormalLet) -> anyhow::Result<()> {
        let value = match &node.init {
            Some(e) => Some(build_expression(self.cucx, e)?),
            None => None,
        };

        self.build_normal_let(
            &node.name,
            node.mutability,
            value,
            node.ty.clone(),
            node.span,
        )
    }

    fn tuple_unpacking(&mut self, node: &TupleUnpack) -> anyhow::Result<()> {
        let tuple = build_expression(self.cucx, &node.init)?;

        let tuple_ref_ty = tuple.get_type();

        let (tuple_len, tuple_mutability) =
            if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
                match rty.refee_ty.kind.as_ref() {
                    TyKind::Tuple(tuple_ty) => (tuple_ty.len(), tuple_ref_ty.mutability),

                    kind => {
                        return Err(CodegenError::MismatchedTypes {
                            types: (
                                create_inferred_tuple(node.names.len()).to_string(),
                                kind.to_string(),
                            ),
                            span: node.span,
                        }
                        .into());
                    }
                }
            } else {
                return Err(CodegenError::MismatchedTypes {
                    types: (
                        create_inferred_tuple(node.names.len()).to_string(),
                        tuple_ref_ty.kind.to_string(),
                    ),
                    span: node.span,
                }
                .into());
            };

        if node.names.len() != tuple_len {
            return Err(CodegenError::NumberOfTupleFieldsDoesNotMatch {
                lens: (node.names.len(), tuple_len),
                span: node.span,
            }
            .into());
        }

        // Unpacking
        for (index, name_and_mutability) in node.names.iter().enumerate() {
            let (name, mutability) = match name_and_mutability {
                Some(x) => (&x.0, x.1),

                // Ignore field
                None => continue,
            };

            if mutability.is_mut() && tuple_mutability.is_not() {
                todo!("Error")
            }

            self.unpack_one_tuple_field(&tuple, index as u32, name, mutability, node.span)?;
        }

        Ok(())
    }

    fn unpack_one_tuple_field(
        &mut self,
        tuple: &Value<'ctx>,
        index: u32,
        unpacked_name: &Ident,
        unpacked_mutability: Mutability,
        span: Span,
    ) -> anyhow::Result<()> {
        assert!(matches!(
            tuple.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let tuple_ref_ty = tuple.get_type();

        let tuple_ty = if let TyKind::Reference(rty) = tuple_ref_ty.kind.as_ref() {
            rty
        } else {
            unreachable!();
        };

        let unpacked_value = build_tuple_indexing(
            self.cucx,
            tuple.get_value().into_pointer_value(),
            index,
            &tuple_ty.refee_ty,
            span,
        )?;

        self.build_normal_let(
            unpacked_name,
            unpacked_mutability,
            Some(unpacked_value),
            Ty::new_inferred(unpacked_mutability).into(),
            span,
        )?;

        Ok(())
    }
}
