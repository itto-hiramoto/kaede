use std::rc::Rc;

use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{expr::Expr, ty::Ty};

#[derive(Debug)]
pub struct Assign {
    pub assignee: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug)]
pub struct Let {
    pub name: Symbol,
    pub init: Option<Expr>,
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TupleUnpack {
    /// None if ignore field
    pub names: Vec<Option<Symbol>>,
    pub init: Expr,
    pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Rc<Expr>),
    Let(Let),
    TupleUnpack(TupleUnpack),

    Assign(Assign),
}

/// Statement list
/// May be handled as expression
#[derive(Debug)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub last_expr: Option<Box<Expr>>,
    pub span: Span,
}
