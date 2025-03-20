use std::rc::Rc;

use kaede_span::Span;
use kaede_symbol::Ident;
use kaede_ast_type::{Mutability, Ty};

use crate::expr::Expr;

#[derive(Debug)]
pub enum AssignKind {
    /// '='
    Simple,
}

#[derive(Debug)]
pub struct Assign {
    pub lhs: Expr,
    pub kind: AssignKind,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug)]
pub struct Let {
    pub kind: LetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum LetKind {
    NormalLet(NormalLet),
    TupleUnpack(TupleUnpack),
}

#[derive(Debug)]
pub struct NormalLet {
    pub name: Ident,
    pub mutability: Mutability,
    pub init: Option<Rc<Expr>>,
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TupleUnpack {
    /// None if ignore field
    pub names: Vec<Option<(Ident, Mutability)>>,

    pub init: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Rc<Expr>),
    Let(Let),

    Assign(Assign),
}

/// Statement list
/// May be handled as expression
#[derive(Debug)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub span: Span,
}
