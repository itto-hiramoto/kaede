use std::{cell::RefCell, collections::HashMap, rc::Rc};

use kaede_ir_type as ir_type;

use kaede_ir_type::ModulePath;
use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{error::SemanticError, SemanticAnalyzer};

use kaede_ast as ast;
use kaede_ir as ir;

// #[derive(Debug)]
// pub struct FunctionInfo<'ctx> {
//     pub value: FunctionValue<'ctx>,
//     pub return_type: ReturnType,
//     pub param_types: Vec<Rc<Ty>>,
// }

//
// #[derive(Debug, Clone)]
// pub struct EnumVariantInfo {
//     pub name: Ident,
//     pub _vis: Visibility,
//     pub offset: u32,
//     pub ty: Option<Rc<Ty>>,
// }

// #[derive(Debug, Clone)]
// pub struct EnumInfo<'ctx> {
//     pub ty: StructType<'ctx>,
//     pub mangled_name: Symbol,
//     pub name: Symbol, // Non-mangled
//     pub variants: HashMap<Symbol, EnumVariantInfo>,
//     pub is_external: Option<Vec<Ident>>,
// }

// #[derive(Debug, Clone)]
// pub struct AlreadyGeneratedGenericImpl {
//     pub table: Vec<GenericArgs>,
// }

// impl AlreadyGeneratedGenericImpl {
//     pub fn new() -> Self {
//         Self { table: Vec::new() }
//     }

//     pub fn insert(&mut self, args: GenericArgs) {
//         self.table.push(args);
//     }

//     pub fn contains(&self, args: &GenericArgs) -> bool {
//         let types = &args.types;

//         for args in &self.table {
//             if types == &args.types {
//                 return true;
//             }
//         }

//         false
//     }
// }

#[derive(Debug, Clone)]
pub struct GenericImplInfo {
    pub impl_: ast::top::Impl,
    pub visibility: ast::top::Visibility,
    pub span: Span,
    // pub already_generated: AlreadyGeneratedGenericImpl,
}

impl GenericImplInfo {
    pub fn new(impl_: ast::top::Impl, visibility: ast::top::Visibility, span: Span) -> Self {
        Self {
            impl_,
            visibility,
            span,
            // already_generated: AlreadyGeneratedGenericImpl::new(),
        }
    }
}

#[derive(Debug)]
pub struct GenericStructInfo {
    pub ast: ast::top::Struct,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericStructInfo {
    pub fn new(ast: ast::top::Struct) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub struct GenericEnumInfo {
    pub ast: ast::top::Enum,
    pub impl_info: Option<GenericImplInfo>,
}

impl GenericEnumInfo {
    pub fn new(ast: ast::top::Enum) -> Self {
        Self {
            ast,
            impl_info: None,
        }
    }
}

#[derive(Debug)]
pub struct GenericFuncInfo {
    pub ast: ast::top::Fn,
}

#[derive(Debug)]
pub enum GenericInfo {
    Struct(GenericStructInfo),
    Enum(GenericEnumInfo),
    Func(GenericFuncInfo),
}

#[derive(Debug)]
pub struct VariableInfo {
    pub ty: Rc<ir_type::Ty>,
}

#[derive(Debug)]
pub enum SymbolTableValueKind {
    Variable(VariableInfo),
    Function(Rc<ir::top::Fn>),
    Struct(Rc<ir::top::Struct>),
    Enum(Rc<ir::top::Enum>),
    Generic(GenericInfo),
    // Module(Symbol),
}

#[derive(Debug)]
pub struct SymbolTableValue {
    pub kind: SymbolTableValueKind,
    pub module_path: ModulePath,
}

impl SymbolTableValue {
    pub fn new(kind: SymbolTableValueKind, analyzer: &SemanticAnalyzer) -> Self {
        Self {
            kind,
            module_path: analyzer.current_module_path().clone(),
        }
    }
}

pub struct SymbolTable {
    table: HashMap<Symbol, Rc<RefCell<SymbolTableValue>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn lookup(&self, symbol: &Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.table.get(symbol).cloned()
    }

    pub fn insert(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .table
            .insert(symbol, Rc::new(RefCell::new(value)))
            .is_some()
        {
            return Err(SemanticError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }
}

pub struct GenericArgumentTable {
    map: HashMap<Symbol, Rc<ir_type::Ty>>,
}

impl GenericArgumentTable {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.map.get(&symbol).cloned()
    }
}

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

        // Insert the generic arguments
        generic_params
            .names
            .iter()
            .zip(generic_args.iter())
            .for_each(|(ident, ty)| {
                self.generic_argument_table
                    .map
                    .insert(ident.symbol(), ty.clone());
            });

        let result = f(self);

        // Remove the generic arguments
        generic_params.names.iter().for_each(|ident| {
            self.generic_argument_table.map.remove(&ident.symbol());
        });

        result
    }
}
