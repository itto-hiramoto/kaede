use std::rc::Rc;

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::{module_path::ModulePath, top::TopLevel, ty as ir_type};
use kaede_span::Span;

use crate::{
    error::SemanticError,
    symbol_table::{GenericKind, SymbolTableValueKind},
    SemanticAnalyzer, TopLevelAnalysisResult,
};

impl SemanticAnalyzer {
    pub fn analyze_type(&mut self, ty: &ast_type::Ty) -> anyhow::Result<Rc<ir_type::Ty>> {
        match ty.kind.as_ref() {
            ast_type::TyKind::Fundamental(fty) => Ok(self.analyze_fundamental_type(fty).into()),

            ast_type::TyKind::UserDefined(udt) => self.analyze_user_defined_type(udt),

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
        self.with_module(external_module_path, |analyzer| {
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

    fn create_generic_struct(
        &mut self,
        ast: &ast::top::Struct,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        todo!()
    }

    fn create_generic_enum(
        &mut self,
        ast: &ast::top::Enum,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        todo!()
    }

    fn create_generic_type(
        &mut self,
        udt: &ast_type::UserDefinedType,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        assert!(udt.generic_args.is_some());

        let generic_args = udt
            .generic_args
            .as_ref()
            .unwrap()
            .types
            .iter()
            .map(|ty| self.analyze_type(ty))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let symbol = self
            .lookup_symbol(udt.name.symbol())
            .ok_or(SemanticError::Undeclared {
                name: udt.name.symbol(),
                span: udt.name.span(),
            })?;

        let borrowed_symbol = symbol.borrow();

        let generic_info = match &*&borrowed_symbol.kind {
            SymbolTableValueKind::Generic(generic_info) => generic_info,
            _ => unreachable!(),
        };

        let module_path = generic_info.module_path.clone();

        self.with_module(module_path, |analyzer| {
            // Create the user defined type with the generic arguments
            let (ty, impl_info) = match &generic_info.kind {
                GenericKind::Struct(info) => (
                    analyzer.create_generic_struct(&info.ast, &generic_args)?,
                    info.impl_info.as_ref(),
                ),
                GenericKind::Enum(info) => (
                    analyzer.create_generic_enum(&info.ast, &generic_args)?,
                    info.impl_info.as_ref(),
                ),
                _ => unreachable!(),
            };

            // Create methods
            let mut borrowed_mut_symbol = symbol.borrow_mut();

            let generic_info = match &mut *&mut borrowed_mut_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            if impl_info.is_some() {
                let impl_info = match &mut generic_info.kind {
                    GenericKind::Struct(info) => info.impl_info.as_mut().unwrap(),
                    GenericKind::Enum(info) => info.impl_info.as_mut().unwrap(),
                    _ => unreachable!(),
                };

                let impl_ir =
                    impl_info.to_impl_ir_with_actual_types(analyzer, generic_args.clone())?;

                if let TopLevelAnalysisResult::TopLevel(TopLevel::Impl(impl_ir)) = impl_ir {
                    analyzer.generated_generic_impl_table.push(impl_ir.clone());
                }
            }

            Ok(ty)
        })
    }

    fn analyze_user_defined_type(
        &mut self,
        udt: &ast_type::UserDefinedType,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        // If this is a generic type, the type is generated here.
        if udt.generic_args.is_some() {
            return self.create_generic_type(&udt);
        }

        if let Some(generic_arg) = self.lookup_generic_argument(udt.name.symbol()) {
            return Ok(generic_arg);
        }

        let symbol = self
            .lookup_symbol(udt.name.symbol())
            .ok_or(SemanticError::Undeclared {
                name: udt.name.symbol(),
                span: udt.name.span(),
            })?;

        let borrowed_symbol = symbol.borrow();

        let udt_kind = match &*&borrowed_symbol.kind {
            SymbolTableValueKind::Struct(st) => ir_type::UserDefinedTypeKind::Struct(st.clone()),
            SymbolTableValueKind::Enum(en) => ir_type::UserDefinedTypeKind::Enum(en.clone()),
            _ => unreachable!(),
        };

        Ok(Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::UserDefined(ir_type::UserDefinedType::new(udt_kind)).into(),
            mutability: ir_type::Mutability::Not,
        }))
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
