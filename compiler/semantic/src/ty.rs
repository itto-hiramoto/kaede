use std::rc::Rc;

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir as ir;
use kaede_ir::ty as ir_type;
use kaede_span::Span;
use kaede_symbol::Ident;

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

            ast_type::TyKind::Unit => Ok(ir_type::Ty::new_unit().into()),
            ast_type::TyKind::Never => Ok(ir_type::Ty::new_never().into()),
            ast_type::TyKind::Inferred => todo!(),
        }
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

    fn verify_generic_argument_length(
        &self,
        params: &ast::top::GenericParams,
        args: &[Rc<ir_type::Ty>],
        span: Span,
    ) -> anyhow::Result<()> {
        if params.names.len() != args.len() {
            return Err(SemanticError::GenericArgumentLengthMismatch {
                expected: params.names.len(),
                actual: args.len(),
                span,
            }
            .into());
        }

        Ok(())
    }

    fn create_generic_struct(
        &mut self,
        ast: &ast::top::Struct,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let generic_params = ast.generic_params.as_ref().unwrap();

        self.verify_generic_argument_length(generic_params, generic_args, generic_params.span)?;

        let mut ast = ast.clone();
        ast.generic_params = None;
        let generated_generic_key =
            self.create_generated_generic_key(ast.name.symbol(), generic_args);
        ast.name = Ident::new(generated_generic_key, ast.name.span());

        let symbol = self.with_generic_arguments(generic_params, generic_args, |analyzer| {
            // If the symbol is already generated, return it
            if let Some(symbol) = analyzer.lookup_symbol(generated_generic_key) {
                return Ok(symbol);
            }

            let top_level = analyzer.analyze_struct(ast)?;

            if let TopLevelAnalysisResult::TopLevel(top_level) = top_level {
                assert!(matches!(top_level, ir::top::TopLevel::Struct(_)));
                analyzer.generated_generics.push(top_level);
            } else {
                unreachable!()
            }

            Ok(analyzer.lookup_symbol(generated_generic_key).unwrap())
        })?;

        let symbol_value = symbol.borrow();

        let udt_ir = match &symbol_value.kind {
            SymbolTableValueKind::Struct(st) => {
                ir_type::TyKind::UserDefined(ir_type::UserDefinedType {
                    kind: ir_type::UserDefinedTypeKind::Struct(st.clone()),
                })
            }
            _ => unreachable!(),
        };

        Ok(Rc::new(ir_type::Ty {
            kind: udt_ir.into(),
            mutability: ir_type::Mutability::Not,
        }))
    }

    fn create_generic_enum(
        &mut self,
        ast: &ast::top::Enum,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let generic_params = ast.generic_params.as_ref().unwrap();

        self.verify_generic_argument_length(generic_params, generic_args, generic_params.span)?;

        let mut ast = ast.clone();
        ast.generic_params = None;
        let generated_generic_key =
            self.create_generated_generic_key(ast.name.symbol(), generic_args);
        ast.name = Ident::new(generated_generic_key, ast.name.span());

        let name_span = ast.name.span();

        self.with_generic_arguments(generic_params, generic_args, |analyzer| {
            let top_level = analyzer.analyze_enum(ast)?;

            if let TopLevelAnalysisResult::TopLevel(top_level) = top_level {
                assert!(matches!(top_level, ir::top::TopLevel::Enum(_)));
                analyzer.generated_generics.push(top_level);
            } else {
                unreachable!()
            }

            Ok(())
        })?;

        let symbol =
            self.lookup_symbol(generated_generic_key)
                .ok_or(SemanticError::Undeclared {
                    name: generated_generic_key,
                    span: name_span,
                })?;

        let symbol_value = symbol.borrow();

        let udt_ir = match &symbol_value.kind {
            SymbolTableValueKind::Enum(en) => {
                ir_type::TyKind::UserDefined(ir_type::UserDefinedType {
                    kind: ir_type::UserDefinedTypeKind::Enum(en.clone()),
                })
            }
            _ => unreachable!(),
        };

        Ok(Rc::new(ir_type::Ty {
            kind: udt_ir.into(),
            mutability: ir_type::Mutability::Not,
        }))
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

        // To avoid borrow checker error
        drop(borrowed_symbol);

        self.with_module(module_path, |analyzer| {
            let mut borrowed_mut_symbol = symbol.borrow_mut();

            let generic_info = match &mut *&mut borrowed_mut_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

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
            if impl_info.is_some() {
                let impl_info = match &mut generic_info.kind {
                    GenericKind::Struct(info) => info.impl_info.as_mut().unwrap(),
                    GenericKind::Enum(info) => info.impl_info.as_mut().unwrap(),
                    _ => unreachable!(),
                };

                // If the generic arguments have already been generated, return the type
                if impl_info.generateds.contains(&generic_args) {
                    return Ok(ty);
                } else {
                    impl_info.generateds.push(generic_args.clone());
                }

                let generic_params = impl_info.impl_.generic_params.as_ref().unwrap().clone();

                // Check if the length of the generic arguments is the same as the number of generic parameters
                if generic_params.names.len() != generic_args.len() {
                    return Err(SemanticError::GenericArgumentLengthMismatch {
                        expected: generic_params.names.len(),
                        actual: generic_args.len(),
                        span: generic_params.span,
                    }
                    .into());
                }

                let mut impl_ = impl_info.impl_.clone();
                impl_.generic_params = None;

                // To avoid borrow checker error
                drop(borrowed_mut_symbol);

                let impl_ir = analyzer.with_generic_arguments(
                    &generic_params,
                    &generic_args,
                    |analyzer| analyzer.analyze_impl(impl_),
                )?;

                if let TopLevelAnalysisResult::TopLevel(top_level) = impl_ir {
                    assert!(matches!(top_level, ir::top::TopLevel::Impl(_)));
                    analyzer.generated_generics.push(top_level);
                }
            }

            Ok(ty)
        })
    }

    pub fn analyze_user_defined_type(
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

            SymbolTableValueKind::Generic(generic_info) => {
                // Generic arguments are not provided.
                return Err(SemanticError::GenericArgumentLengthMismatch {
                    expected: generic_info.get_generic_argument_length(),
                    actual: 0,
                    span: udt.name.span(),
                }
                .into());
            }
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
