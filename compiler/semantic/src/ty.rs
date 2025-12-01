use std::rc::Rc;

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::ty as ir_type;
use kaede_ir::{self as ir, qualified_symbol::QualifiedSymbol};
use kaede_span::Span;
use kaede_symbol::Ident;

use crate::context::AnalyzeCommand;
use crate::{error::SemanticError, SemanticAnalyzer, TopLevelAnalysisResult};
use kaede_symbol_table::{GenericKind, SymbolTableValueKind};

struct GenericTypeOps<T> {
    get_generic_params: fn(&T) -> &ast::top::GenericParams,
    get_name: fn(&T) -> Ident,
    clone_and_clear: fn(&T, Ident) -> T,
    analyze: fn(&mut SemanticAnalyzer, T) -> anyhow::Result<TopLevelAnalysisResult>,
    extract_type: fn(&SymbolTableValueKind) -> ir_type::UserDefinedTypeKind,
}

impl SemanticAnalyzer {
    pub fn analyze_type(&mut self, ty: &ast_type::Ty) -> anyhow::Result<Rc<ir_type::Ty>> {
        match ty.kind.as_ref() {
            ast_type::TyKind::Fundamental(fty) => Ok(self
                .analyze_fundamental_type(fty, ty.mutability.into())
                .into()),

            ast_type::TyKind::UserDefined(udt) => {
                self.analyze_user_defined_type(udt, ty.mutability.into())
            }

            ast_type::TyKind::Reference(rty) => Ok(self
                .analyze_reference_type(rty, ty.mutability.into())?
                .into()),
            ast_type::TyKind::Pointer(pty) => {
                Ok(self.analyze_pointer_type(pty, ty.mutability.into())?.into())
            }

            ast_type::TyKind::Array((ety, length)) => Ok(self
                .analyze_array_type(ety, *length, ty.mutability.into())?
                .into()),
            ast_type::TyKind::Tuple(etys) => Ok(self
                .analyze_tuple_type(
                    etys.iter()
                        .map(AsRef::as_ref)
                        .collect::<Vec<_>>()
                        .as_slice(),
                    ty.mutability.into(),
                )?
                .into()),

            ast_type::TyKind::Generic(gty) => self.analyze_generic_type(gty, ty.span),

            ast_type::TyKind::External(ty, access_chain) => {
                let (_, module_path) = self.create_module_path_from_access_chain(
                    access_chain
                        .iter()
                        .map(|i| i.symbol())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    ty.span,
                )?;

                self.with_module(module_path, |analyzer| analyzer.analyze_type(ty))
            }

            ast_type::TyKind::Unit => Ok(ir_type::Ty::new_unit().into()),
            ast_type::TyKind::Never => Ok(ir_type::Ty::new_never().into()),
            ast_type::TyKind::Var => Ok(self.infer_context.fresh()),
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

    fn analyze_fundamental_type(
        &self,
        fty: &ast_type::FundamentalType,
        mutability: ir_type::Mutability,
    ) -> ir_type::Ty {
        let make = |kind| ir_type::make_fundamental_type(kind, mutability);

        match fty.kind {
            ast_type::FundamentalTypeKind::Str => make(ir_type::FundamentalTypeKind::Str),
            ast_type::FundamentalTypeKind::Bool => make(ir_type::FundamentalTypeKind::Bool),
            ast_type::FundamentalTypeKind::I8 => make(ir_type::FundamentalTypeKind::I8),
            ast_type::FundamentalTypeKind::U8 => make(ir_type::FundamentalTypeKind::U8),
            ast_type::FundamentalTypeKind::I32 => make(ir_type::FundamentalTypeKind::I32),
            ast_type::FundamentalTypeKind::U32 => make(ir_type::FundamentalTypeKind::U32),
            ast_type::FundamentalTypeKind::I64 => make(ir_type::FundamentalTypeKind::I64),
            ast_type::FundamentalTypeKind::U64 => make(ir_type::FundamentalTypeKind::U64),
            ast_type::FundamentalTypeKind::Char => make(ir_type::FundamentalTypeKind::Char),
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

    fn instantiate_generic_type<T>(
        &mut self,
        ast: &T,
        generic_args: &[Rc<ir_type::Ty>],
        ops: GenericTypeOps<T>,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        let generic_params = (ops.get_generic_params)(ast);

        self.verify_generic_argument_length(generic_params, generic_args, generic_params.span)?;

        let generated_generic_key =
            self.create_generated_generic_key((ops.get_name)(ast).symbol(), generic_args);
        let modified_ast = (ops.clone_and_clear)(
            ast,
            Ident::new(generated_generic_key, (ops.get_name)(ast).span()),
        );

        if self.generating_generics.contains(&generated_generic_key) {
            // Circular dependency detected - return a placeholder type
            return Ok(Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::UserDefined(ir_type::UserDefinedType {
                    kind: ir_type::UserDefinedTypeKind::Placeholder(QualifiedSymbol::new(
                        self.current_module_path().clone(),
                        generated_generic_key,
                    )),
                })
                .into(),
                mutability: ir_type::Mutability::Not,
            }));
        }

        self.generating_generics.insert(generated_generic_key);

        let symbol = self.with_generic_arguments(generic_params, generic_args, |analyzer| {
            // If the symbol is already generated, return it
            if let Some(symbol) = analyzer.lookup_symbol(generated_generic_key) {
                return Ok(symbol);
            }

            let top_level = (ops.analyze)(analyzer, modified_ast)?;

            if let TopLevelAnalysisResult::TopLevel(top_level) = top_level {
                analyzer.generated_generics.push(top_level);
            } else {
                unreachable!()
            }

            Ok(analyzer.lookup_symbol(generated_generic_key).unwrap())
        })?;

        let symbol_value = symbol.borrow();
        let udt_ir = ir_type::TyKind::UserDefined(ir_type::UserDefinedType {
            kind: (ops.extract_type)(&symbol_value.kind),
        });

        Ok(Rc::new(ir_type::Ty {
            kind: udt_ir.into(),
            mutability: ir_type::Mutability::Not,
        }))
    }

    fn create_generic_struct(
        &mut self,
        ast: &ast::top::Struct,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        self.instantiate_generic_type(
            ast,
            generic_args,
            GenericTypeOps {
                get_generic_params: |ast| ast.generic_params.as_ref().unwrap(),
                get_name: |ast| ast.name,
                clone_and_clear: |ast, new_name| {
                    let mut cloned = ast.clone();
                    cloned.generic_params = None;
                    cloned.name = new_name;
                    cloned
                },
                analyze: |analyzer, ast| analyzer.analyze_struct(ast),
                extract_type: |kind| match kind {
                    SymbolTableValueKind::Struct(st) => {
                        ir_type::UserDefinedTypeKind::Struct(st.clone())
                    }
                    _ => unreachable!(),
                },
            },
        )
    }

    fn create_generic_enum(
        &mut self,
        ast: &ast::top::Enum,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        self.instantiate_generic_type(
            ast,
            generic_args,
            GenericTypeOps {
                get_generic_params: |ast| ast.generic_params.as_ref().unwrap(),
                get_name: |ast| ast.name,
                clone_and_clear: |ast, new_name| {
                    let mut cloned = ast.clone();
                    cloned.generic_params = None;
                    cloned.name = new_name;
                    cloned
                },
                analyze: |analyzer, ast| analyzer.analyze_enum(ast),
                extract_type: |kind| match kind {
                    SymbolTableValueKind::Enum(en) => {
                        ir_type::UserDefinedTypeKind::Enum(en.clone())
                    }
                    _ => unreachable!(),
                },
            },
        )
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

        let generic_info = match &borrowed_symbol.kind {
            SymbolTableValueKind::Generic(generic_info) => generic_info,
            _ => unreachable!(),
        };

        let module_path = generic_info.module_path.clone();

        self.with_module(module_path, |analyzer| {
            let generic_info = match &borrowed_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            // Create the user defined type with the generic arguments
            let impl_info = match &generic_info.kind {
                GenericKind::Struct(info) => info.impl_info.as_ref(),
                GenericKind::Enum(info) => info.impl_info.as_ref(),
                _ => unreachable!(),
            };

            // Create methods
            if impl_info.is_some() {
                // To avoid borrow checker error
                drop(borrowed_symbol);

                let mut borrowed_mut_symbol = symbol.borrow_mut();

                let generic_info = match &mut borrowed_mut_symbol.kind {
                    SymbolTableValueKind::Generic(generic_info) => generic_info,
                    _ => unreachable!(),
                };

                let impl_info = match &mut generic_info.kind {
                    GenericKind::Struct(info) => info.impl_info.as_mut().unwrap(),
                    GenericKind::Enum(info) => info.impl_info.as_mut().unwrap(),
                    _ => unreachable!(),
                };

                // Check if the impl have already been generated
                if !impl_info.generateds.contains(&generic_args) {
                    impl_info.generateds.push(generic_args.clone());

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

                    let mut methods = vec![];

                    // Since the generic function is generated multiple times,
                    // we need to set link_once to true to avoid errors
                    for method in impl_.items.iter() {
                        if let ast::top::TopLevelKind::Fn(fn_) = &method.kind {
                            let mut fn_decl = fn_.decl.clone();
                            fn_decl.link_once = true;

                            methods.push(ast::top::TopLevel {
                                kind: ast::top::TopLevelKind::Fn(ast::top::Fn {
                                    decl: fn_decl,
                                    body: fn_.body.clone(),
                                    span: fn_.span,
                                }),
                                span: fn_.span,
                            });
                        }
                    }

                    impl_.items = Rc::new(methods);

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
            }

            let borrowed_symbol = symbol.borrow();

            let generic_info = match &borrowed_symbol.kind {
                SymbolTableValueKind::Generic(generic_info) => generic_info,
                _ => unreachable!(),
            };

            // Create the user defined type with the generic arguments
            let (struct_ast, enum_ast) = match &generic_info.kind {
                GenericKind::Struct(info) => (Some(info.ast.clone()), None),
                GenericKind::Enum(info) => (None, Some(info.ast.clone())),
                _ => unreachable!(),
            };

            // To avoid borrow checker error
            drop(borrowed_symbol);

            if let Some(struct_ast) = struct_ast {
                analyzer.create_generic_struct(&struct_ast, &generic_args)
            } else if let Some(enum_ast) = enum_ast {
                analyzer.create_generic_enum(&enum_ast, &generic_args)
            } else {
                unreachable!()
            }
        })
    }

    pub fn analyze_user_defined_type(
        &mut self,
        udt: &ast_type::UserDefinedType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<Rc<ir_type::Ty>> {
        // If this is a generic type, the type is generated here.
        if udt.generic_args.is_some() {
            // Generic methods must be defined, so NoCommand
            return self.with_analyze_command(AnalyzeCommand::NoCommand, |analyzer| {
                analyzer.create_generic_type(udt)
            });
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

        let udt_kind = match &borrowed_symbol.kind {
            SymbolTableValueKind::Struct(st) => ir_type::UserDefinedTypeKind::Struct(st.clone()),
            SymbolTableValueKind::Enum(en) => ir_type::UserDefinedTypeKind::Enum(en.clone()),
            SymbolTableValueKind::Placeholder(placeholder) => {
                ir_type::UserDefinedTypeKind::Placeholder(placeholder.clone())
            }

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
            mutability,
        }))
    }

    fn analyze_reference_type(
        &mut self,
        rty: &ast_type::ReferenceType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Reference(ir_type::ReferenceType {
                refee_ty: self.analyze_type(&rty.refee_ty)?,
            })
            .into(),
            mutability,
        })
    }

    fn analyze_pointer_type(
        &mut self,
        pty: &ast_type::PointerType,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                pointee_ty: self.analyze_type(&pty.pointee_ty)?,
            })
            .into(),
            mutability,
        })
    }

    fn analyze_array_type(
        &mut self,
        ety: &ast_type::Ty,
        length: u32,
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Array((self.analyze_type(ety)?, length)).into(),
            mutability,
        })
    }

    fn analyze_tuple_type(
        &mut self,
        etys: &[&ast_type::Ty],
        mutability: ir_type::Mutability,
    ) -> anyhow::Result<ir_type::Ty> {
        let etys = etys
            .iter()
            .map(|ety| self.analyze_type(ety))
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(ir_type::Ty {
            kind: ir_type::TyKind::Tuple(etys.into_iter().collect()).into(),
            mutability,
        })
    }
}
