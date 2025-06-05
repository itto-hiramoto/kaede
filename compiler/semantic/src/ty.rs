use std::rc::Rc;

use kaede_ast_type as ast_type;
use kaede_ir_type::{self as ir_type, ModulePath};
use kaede_span::Span;

use crate::{error::SemanticError, SemanticAnalyzer};

impl SemanticAnalyzer {
    pub fn analyze_type(&mut self, ty: &ast_type::Ty) -> anyhow::Result<Rc<ir_type::Ty>> {
        match ty.kind.as_ref() {
            ast_type::TyKind::Fundamental(fty) => Ok(self.analyze_fundamental_type(fty).into()),

            ast_type::TyKind::UserDefined(udt) => Ok(self.analyze_user_defined_type(udt).into()),

            ast_type::TyKind::Reference(rty) => Ok(self.analyze_reference_type(rty)?.into()),
            ast_type::TyKind::Pointer(pty) => Ok(self.analyze_pointer_type(pty)?.into()),

            ast_type::TyKind::Array((ety, length)) => {
                Ok(self.analyze_array_type(ety, *length)?.into())
            }
            ast_type::TyKind::Tuple(etys) => Ok(self
                .analyze_tuple_type(
                    etys.iter()
                        .map(AsRef::as_ref)
                        .collect::<Vec<_>>()
                        .as_slice(),
                )?
                .into()),

            ast_type::TyKind::Generic(gty) => self.analyze_generic_type(gty, ty.span),
            ast_type::TyKind::External(ety) => self.analyze_external_type(ety),

            ast_type::TyKind::Unit => Ok(ir_type::Ty::new_unit().into()),
            ast_type::TyKind::Never => Ok(ir_type::Ty::new_never().into()),
            ast_type::TyKind::Inferred => todo!(),
        }
    }

    fn analyze_external_type(
        &mut self,
        ety: &ast_type::ExternalType,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let external_module_path = ModulePath::new(
            ety.get_module_names_recursively()
                .iter()
                .map(|n| n.symbol())
                .collect(),
        );

        let base_ty = ety.get_base_type();

        // Replace the current module path with the external module path
        self.with_external_module(external_module_path, |analyzer| {
            analyzer.analyze_type(&base_ty)
        })
    }

    fn analyze_generic_type(
        &self,
        gty: &ast_type::GenericType,
        span: Span,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let name = gty.name.symbol();

        match self.lookup_generic_argument(name) {
            Some(ty) => Ok(ty),
            None => Err(SemanticError::Undeclared { name, span }.into()),
        }
    }

    fn analyze_fundamental_type(&self, fty: &ast_type::FundamentalType) -> ir_type::Ty {
        let make = |kind| ir_type::make_fundamental_type(kind, ir_type::Mutability::Not);

        match fty.kind {
            ast_type::FundamentalTypeKind::Str => make(ir_type::FundamentalTypeKind::Str),
            ast_type::FundamentalTypeKind::Bool => make(ir_type::FundamentalTypeKind::Bool),
            ast_type::FundamentalTypeKind::I8 => make(ir_type::FundamentalTypeKind::I8),
            ast_type::FundamentalTypeKind::U8 => make(ir_type::FundamentalTypeKind::U8),
            ast_type::FundamentalTypeKind::I32 => make(ir_type::FundamentalTypeKind::I32),
            ast_type::FundamentalTypeKind::U32 => make(ir_type::FundamentalTypeKind::U32),
            ast_type::FundamentalTypeKind::I64 => make(ir_type::FundamentalTypeKind::I64),
            ast_type::FundamentalTypeKind::U64 => make(ir_type::FundamentalTypeKind::U64),
        }
    }

    fn analyze_user_defined_type(&self, udt: &ast_type::UserDefinedType) -> ir_type::Ty {
        ir_type::Ty {
            kind: ir_type::TyKind::UserDefined(ir_type::UserDefinedType {
                name: ir_type::QualifiedSymbol::new(
                    self.current_module_path().clone(),
                    udt.name.symbol(),
                ),
            })
            .into(),
            mutability: ir_type::Mutability::Not,
        }
    }

    fn analyze_reference_type(
        &mut self,
        rty: &ast_type::ReferenceType,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Reference(ir_type::ReferenceType {
                refee_ty: self.analyze_type(&rty.refee_ty)?.into(),
            })
            .into(),
            mutability: ir_type::Mutability::Not,
        })
    }

    fn analyze_pointer_type(&mut self, pty: &ast_type::PointerType) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                pointee_ty: self.analyze_type(&pty.pointee_ty)?.into(),
            })
            .into(),
            mutability: ir_type::Mutability::Not,
        })
    }

    fn analyze_array_type(
        &mut self,
        ety: &ast_type::Ty,
        length: u32,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Array((self.analyze_type(&ety)?.into(), length)).into(),
            mutability: ir_type::Mutability::Not,
        })
    }

    fn analyze_tuple_type(&mut self, etys: &[&ast_type::Ty]) -> anyhow::Result<ir_type::Ty> {
        let etys = etys
            .iter()
            .map(|ety| self.analyze_type(ety))
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Tuple(etys.into_iter().map(|ty| ty.into()).collect()).into(),
            mutability: ir_type::Mutability::Not,
        })
    }
}
