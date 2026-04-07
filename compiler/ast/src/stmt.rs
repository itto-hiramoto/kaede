use std::rc::Rc;

use kaede_ast_type::{Mutability, Ty};
use kaede_span::Span;
use kaede_symbol::Ident;

use crate::expr::Expr;

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: AssignOp,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Eq,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub kind: LetKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LetKind {
    NormalLet(NormalLet),
    TupleUnpack(TupleUnpack),
}

#[derive(Debug, Clone)]
pub struct NormalLet {
    pub name: Ident,
    pub mutability: Mutability,
    pub init: Option<Rc<Expr>>,
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleUnpack {
    /// None if ignore field
    pub names: Vec<Option<(Ident, Mutability)>>,

    pub init: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Rc<Expr>),
    Let(Let),

    Assign(Box<Assign>),
}

/// Statement list
/// May be handled as expression
#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub span: Span,
}
