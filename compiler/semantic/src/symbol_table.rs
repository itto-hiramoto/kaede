use std::{cell::RefCell, collections::HashMap, rc::Rc};

use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::error::SemanticError;

use kaede_ast as ast;

// #[derive(Debug)]
// pub struct FunctionInfo<'ctx> {
//     pub value: FunctionValue<'ctx>,
//     pub return_type: ReturnType,
//     pub param_types: Vec<Rc<Ty>>,
// }

// #[derive(Debug)]
// pub struct StructInfo<'ctx> {
//     pub mangled_name: Symbol,
//     pub ty: StructType<'ctx>,
//     pub fields: HashMap<Symbol, StructField>,
//     pub is_external: Option<Vec<Ident>>,
// }

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
pub enum SymbolTableValue {
    // Variable((PointerValue<'ctx>, Rc<Ty> /* Variable type */)),
    // Function(FunctionInfo<'ctx>),
    // Struct(StructInfo<'ctx>),
    // Enum(EnumInfo<'ctx>),
    Generic(GenericInfo),
    // Module(Symbol),
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
        return match self.table.get(symbol) {
            Some(value) => Some(value.clone()),
            None => None,
        };
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
