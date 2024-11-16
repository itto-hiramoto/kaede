use std::rc::Rc;

use kaede_symbol::Symbol;
use kaede_type::Ty;

use crate::expr::Expr;

#[derive(Debug)]
pub struct Assign {
    pub assignee: Symbol,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Let {
    pub name: Symbol,
    pub init: Option<Expr>,
    pub ty: Rc<Ty>,
}

#[derive(Debug)]
pub struct TupleUnpack {
    /// None if ignore field
    pub names: Vec<Option<Symbol>>,
    pub init: Expr,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Rc<Expr>),
    Let(Let),

    Assign(Assign),
}

/// Statement list
/// May be handled as expression
#[derive(Debug)]
pub struct Block {
    pub body: Vec<Stmt>,
}
