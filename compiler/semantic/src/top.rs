use std::rc::Rc;

use kaede_symbol::Symbol;

use crate::{
    symbol_table::{
        GenericEnumInfo, GenericFuncInfo, GenericInfo, GenericStructInfo, SymbolTable,
        SymbolTableValue, SymbolTableValueKind, VariableInfo,
    },
    SemanticAnalyzer,
};

use kaede_ast as ast;
use kaede_ir as ir;

/// If a top-level is generic, there is no IR that can be generated immediately, so this enum is used.
#[derive(Debug)]
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
            TopLevelKind::Import(_) => unimplemented!(),
            TopLevelKind::Impl(_) => unimplemented!(),
            TopLevelKind::Extern(_) => unimplemented!(),
            TopLevelKind::Use(_) => unimplemented!(),

            _ => unreachable!(),
        }
    }

    fn analyze_fn(&mut self, node: ast::top::Fn) -> anyhow::Result<TopLevelAnalysisResult> {
        assert_eq!(node.decl.self_, None);

        let mangled_name = if node.decl.name.as_str() == "main" {
            // Suppress mangling of main function.
            Symbol::from(String::from("kdmain"))
        } else {
            self.mangle_fn_name(node.decl.name.as_str())
        };

        self.analyze_fn_with_mangled_name(node, mangled_name)
    }

    pub fn analyze_fn_with_mangled_name(
        &mut self,
        node: ast::top::Fn,
        mangled_name: Symbol,
    ) -> anyhow::Result<TopLevelAnalysisResult> {
        assert_eq!(node.decl.self_, None);

        // If the function is generic, register it in the symbol table and return early.
        if node.decl.generic_params.is_some() {
            let span = node.span;

            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::Func(GenericFuncInfo { ast: node })),
                self,
            );

            self.get_root_symbol_table()
                .insert(mangled_name, symbol_table_value, span)?;

            // Generic functions are not generated immediately, but are generated when they are used.
            return Ok(TopLevelAnalysisResult::GenericTopLevel);
        }

        let params = node
            .decl
            .params
            .v
            .into_iter()
            .map(|p| -> anyhow::Result<ir::top::Param> {
                Ok(ir::top::Param {
                    name: p.name.symbol(),
                    ty: self.analyze_type(&p.ty)?.into(),
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        // Create a new symbol table for the function parameters.
        {
            let mut symbol_table = SymbolTable::new();
            for param in params.iter() {
                let symbol_table_value = SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo {
                        ty: param.ty.clone(),
                    }),
                    self,
                );

                symbol_table.insert(param.name, symbol_table_value, node.span)?;
            }
            self.symbol_tables.push(symbol_table);
        }

        let fn_decl = ir::top::FnDecl {
            name: mangled_name,
            is_var_args: node.decl.params.is_var_args,
            params,
            return_ty: match &node.decl.return_ty {
                None => None,
                Some(ty) => Some(self.analyze_type(ty)?.into()),
            },
        };

        let fn_ = Rc::new(ir::top::Fn {
            decl: fn_decl,
            body: self.analyze_block(&node.body)?,
        });

        // Pop the function symbol table.
        self.symbol_tables.pop();

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Function(fn_.clone()), self);

        self.get_root_symbol_table()
            .insert(mangled_name, symbol_table_value, node.span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Fn(fn_)))
    }

    fn analyze_struct(&mut self, node: ast::top::Struct) -> anyhow::Result<TopLevelAnalysisResult> {
        let mangled_name = self.mangle_struct_name(node.name.as_str()).into();
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::Struct(GenericStructInfo::new(node))),
                self,
            );

            self.get_root_symbol_table()
                .insert(mangled_name, symbol_table_value, span)?;

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
            name: mangled_name,
            fields,
        });

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Struct(ir.clone()), self);

        self.get_root_symbol_table()
            .insert(mangled_name, symbol_table_value, node.span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Struct(
            ir,
        )))
    }

    fn analyze_enum(&mut self, node: ast::top::Enum) -> anyhow::Result<TopLevelAnalysisResult> {
        let mangled_name = self.mangle_enum_name(node.name.as_str());
        let span = node.span;

        // For generic
        if node.generic_params.is_some() {
            let symbol_table_value = SymbolTableValue::new(
                SymbolTableValueKind::Generic(GenericInfo::Enum(GenericEnumInfo::new(node))),
                self,
            );

            self.get_current_symbol_table()
                .insert(mangled_name, symbol_table_value, span)?;

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
            name: mangled_name,
            variants,
        });

        let symbol_table_value =
            SymbolTableValue::new(SymbolTableValueKind::Enum(ir.clone()), self);

        self.get_current_symbol_table()
            .insert(mangled_name, symbol_table_value, span)?;

        Ok(TopLevelAnalysisResult::TopLevel(ir::top::TopLevel::Enum(
            ir,
        )))
    }
}
