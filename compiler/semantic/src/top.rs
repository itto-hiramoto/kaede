use std::rc::Rc;

use crate::{
    symbol_table::{
        GenericEnumInfo, GenericFuncInfo, GenericImplInfo, GenericInfo, GenericKind,
        GenericStructInfo, SymbolTable, SymbolTableValue, SymbolTableValueKind, VariableInfo,
    },
    SemanticAnalyzer, SemanticError,
};

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::{self as ir, qualified_symbol::QualifiedSymbol};
use kaede_symbol::Ident;

/// If a top-level is generic, there is no IR that can be generated immediately, so this enum is used.
#[derive(Debug, Clone)]
pub enum TopLevelAnalysisResult {
    GenericTopLevel,
    TopLevel(ir::top::TopLevel),
}

impl SemanticAnalyzer {
    pub fn analyze_top_level(
        &mut self,
        top_level: ast::top::TopLevel,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        use ast::top::TopLevelKind;

        match top_level.kind {
            TopLevelKind::Fn(node) => self.analyze_fn(node),
            TopLevelKind::Struct(node) => self.analyze_struct(node),
            TopLevelKind::Enum(node) => self.analyze_enum(node),
            TopLevelKind::Impl(node) => self.analyze_impl(node),
            TopLevelKind::Extern(node) => self.analyze_extern(node),
            TopLevelKind::Import(_) => unimplemented!(),
            TopLevelKind::Use(_) => unimplemented!(),

            _ => unreachable!(),
        }
    }

    pub fn analyze_extern(
        &mut self,
        node: ast::top::Extern,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let fn_decl = self.analyze_fn_decl(node.fn_decl)?;

        let name = fn_decl.name.symbol();

        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Function(Rc::new(ir::top::Fn {
                decl: fn_decl.clone(),
                body: None,
            })),
            self,
        );

        self.insert_symbol_to_root_scope(name, symbol_table_value, node.span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Extern(
            Rc::new(ir::top::Extern {
                lang_linkage: node.lang_linkage.map(|s| s.syb),
                fn_decl,
            }),
        )))
    }

    pub fn analyze_impl(&mut self, node: ast::top::Impl) -> anyhow::Result<TopLevelAnalysisResult> {
        let span = node.span;

        if node.generic_params.is_some() {
            let base_ty = if let ast_type::TyKind::Reference(rty) = node.ty.kind.as_ref() {
                rty.get_base_type()
            } else {
                todo!("Error")
            };

            if let ast_type::TyKind::UserDefined(udt) = base_ty.kind.as_ref() {
                let symbol_kind = self.lookup_symbol(udt.name.symbol()).ok_or_else(|| {
                    SemanticError::Undeclared {
                        name: udt.name.symbol(),
                        span: node.span,
                    }
                })?;

                match &mut *&mut symbol_kind.borrow_mut().kind {
                    SymbolTableValueKind::Generic(ref mut generic_info) => {
                        match &mut generic_info.kind {
                            GenericKind::Struct(info) => {
                                info.impl_info = Some(GenericImplInfo::new(node, span));
                            }
                            GenericKind::Enum(info) => {
                                info.impl_info = Some(GenericImplInfo::new(node, span));
                            }
                            _ => todo!("Error"),
                        }
                    }

                    _ => todo!("Error"),
                }

                // Generic impls are not created immediately, but are created when they are used.
                return Ok(TopLevelAnalysisResult::GenericTopLevel);
            } else {
                todo!("Error")
            }
        }

        let mut methods = vec![];

        let ast_ty = Rc::new(node.ty);

        for item in node.items.iter() {
            match &item.kind {
                ast::top::TopLevelKind::Fn(fn_) => {
                    methods.push(self.analyze_method(ast_ty.clone(), fn_.clone())?)
                }
                _ => todo!("Error"),
            }
        }

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Impl(
            Rc::new(ir::top::Impl { methods }),
        )))
    }

    fn analyze_method(
        &mut self,
        ty: Rc<ast_type::Ty>,
        mut node: ast::top::Fn,
    ) -> anyhow::Result<Rc<ir::top::Fn>> {
        // If the method isn't static, insert self to the front of the parameters
        if let Some(mutability) = node.decl.self_ {
            node.decl.params.v.insert(
                0,
                ast::top::Param {
                    name: Ident::new("self".to_owned().into(), node.span),
                    ty: ast_type::change_mutability_dup(ty.clone(), mutability),
                },
            );
        }

        let parent_ty = self.analyze_type(&ty)?;

        let parent_name = if let ir::ty::TyKind::Reference(ty) = parent_ty.kind.as_ref() {
            if let ir::ty::TyKind::UserDefined(udt) = ty.get_base_type().kind.as_ref() {
                udt.name()
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        };

        node.decl.name = Ident::new(
            self.create_method_key(
                parent_name,
                node.decl.name.symbol(),
                node.decl.self_.is_none(),
            ),
            node.decl.name.span(),
        );

        node.decl.self_ = None;
        self.analyze_fn_internal(node)
    }

    fn analyze_fn(&mut self, node: ast::top::Fn) -> anyhow::Result<TopLevelAnalysisResult> {
        assert_eq!(node.decl.self_, None);

        let name = node.decl.name.symbol();

        // If the function is generic, register it in the symbol table and return early.
        if node.decl.generic_params.is_some() {
            let span = node.span;

            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::new(
                    GenericKind::Func(GenericFuncInfo { ast: node }),
                    self.current_module_path().clone(),
                )),
                self,
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, span)?;

            // Generic functions are not generated immediately, but are generated when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Fn(
            self.analyze_fn_internal(node)?,
        )))
    }

    pub fn analyze_fn_internal(&mut self, node: ast::top::Fn) -> anyhow::Result<Rc<ir::top::Fn>> {
        assert_eq!(node.decl.self_, None);

        let fn_decl = self.analyze_fn_decl(node.decl)?;

        // Create a new symbol table for the function parameters.
        {
            let mut symbol_table = SymbolTable::new();
            for param in fn_decl.params.iter() {
                let symbol_table_value = SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo {
                        ty: param.ty.clone(),
                    }),
                    self,
                );

                symbol_table.insert(param.name.symbol(), symbol_table_value, node.span)?;
            }
            self.push_scope(symbol_table);
        }

        let name = fn_decl.name.symbol();

        let fn_ = Rc::new(ir::top::Fn {
            decl: fn_decl,
            body: Some(self.analyze_block(&node.body)?),
        });

        // Pop the function symbol table.
        self.pop_scope();

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Function(fn_.clone()), self);

        self.insert_symbol_to_root_scope(name, symbol_table_value, node.span)?;

        Ok(fn_)
    }

    fn analyze_fn_decl(&mut self, node: ast::top::FnDecl) -> anyhow::Result<ir::top::FnDecl> {
        let name = node.name.symbol();

        let params = node
            .params
            .v
            .into_iter()
            .map(|p| -> anyhow::Result<ir::top::Param> {
                Ok(ir::top::Param {
                    name: p.name,
                    ty: self.analyze_type(&p.ty)?.into(),
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(ir::top::FnDecl {
            name: QualifiedSymbol::new(self.current_module_path().clone(), name),
            is_var_args: node.params.is_var_args,
            params,
            return_ty: match &node.return_ty {
                None => None,
                Some(ty) => Some(self.analyze_type(ty)?.into()),
            },
        })
    }

    pub fn analyze_struct(
        &mut self,
        node: ast::top::Struct,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        let name = node.name.symbol();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::new(
                    GenericKind::Struct(GenericStructInfo::new(node)),
                    self.current_module_path().clone(),
                )),
                self,
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, span)?;

            // Generic structs are not created immediately, but are created when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        let fields = node
            .fields
            .into_iter()
            .map(|field| -> anyhow::Result<ir::top::StructField> {
                Ok(ir::top::StructField {
                    name: field.name.symbol(),
                    ty: self.analyze_type(&field.ty)?.into(),
                    offset: field.offset,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let ir = Rc::new(ir::top::Struct {
            name: QualifiedSymbol::new(self.current_module_path().clone(), name),
            fields,
        });

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Struct(ir.clone()), self);

        self.insert_symbol_to_root_scope(name, symbol_table_value, span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Struct(
            ir,
        )))
    }

    pub fn analyze_enum(&mut self, node: ast::top::Enum) -> anyhow::Result<TopLevelAnalysisResult> {
        let name = node.name.symbol();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::new(
                    GenericKind::Enum(GenericEnumInfo::new(node)),
                    self.current_module_path().clone(),
                )),
                self,
            );

            self.insert_symbol_to_root_scope(name, symbol_table_value, span)?;

            // Generics are not created immediately, but are created when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        let variants = node
            .variants
            .into_iter()
            .map(|variant| -> anyhow::Result<ir::top::EnumVariant> {
                Ok(ir::top::EnumVariant {
                    name: variant.name.symbol(),
                    ty: match variant.ty {
                        None => None,
                        Some(ty) => Some(self.analyze_type(&ty)?),
                    },
                    offset: variant.offset,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let ir = Rc::new(ir::top::Enum {
            name: QualifiedSymbol::new(self.current_module_path().clone(), name),
            variants,
        });

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Enum(ir.clone()), self);

        self.insert_symbol_to_root_scope(name, symbol_table_value, span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Enum(
            ir,
        )))
    }
}
