use std::rc::Rc;

use kaede_ir_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};
use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::stmt::Block;

#[derive(Debug)]
pub struct StringLiteral {
    pub lit: Symbol,
}

#[derive(Debug)]
pub struct StructLiteral {
    pub name: Symbol,
    pub values: Vec<(Symbol, Expr)>,
}

#[derive(Debug)]
pub struct Args(pub Vec<Expr>);

#[derive(Debug)]
pub struct FnCall {
    pub callee: Symbol,
    pub args: Args,
}

#[derive(Debug)]
pub struct Int {
    pub kind: IntKind,
}

#[derive(Debug)]
pub enum IntKind {
    I32(i32),
    U64(u64),
}

impl Int {
    pub fn as_u64(&self) -> u64 {
        use IntKind::*;

        match self.kind {
            I32(n) => n as u64,

            U64(n) => n,
        }
    }

    pub fn get_type(&self) -> Ty {
        match self.kind {
            IntKind::I32(_) => make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not),
            IntKind::U64(_) => make_fundamental_type(FundamentalTypeKind::U64, Mutability::Not),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum BinaryKind {
    // Addition
    Add,
    // Subtraction
    Sub,
    // Multiplication
    Mul,
    // Division
    Div,
    /// Integer remainder
    Rem,

    /// Equal to
    Eq,
    /// Not equal to
    Ne,

    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,

    LogicalOr,
    LogicalAnd,
}

#[derive(Debug)]
pub struct Binary {
    pub lhs: Rc<Expr>,
    pub kind: BinaryKind,
    pub rhs: Rc<Expr>,
}

#[derive(Debug)]
pub struct Cast {
    pub operand: Box<Expr>,
    pub target_ty: Rc<Ty>,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub operand: Box<Expr>,
    pub field_name: Symbol,
    pub field_offset: u64,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Symbol,
    pub variant_offset: usize,
}

#[derive(Debug)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
}

#[derive(Debug)]
pub struct TupleLiteral {
    pub elements: Vec<Expr>,
}

#[derive(Debug)]
pub struct Indexing {
    pub operand: Rc<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug)]
pub struct Loop {
    pub body: Block,
}

#[derive(Debug)]
pub enum Else {
    If(If),
    Block(Box<Expr>),
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub else_: Option<Box<Else>>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Symbol,
    pub ty: Rc<Ty>,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(Int),
    StringLiteral(StringLiteral),
    StructLiteral(StructLiteral),
    ArrayLiteral(ArrayLiteral),
    TupleLiteral(TupleLiteral),
    Variable(Variable),
    BooleanLiteral(bool),
    False,
    Binary(Binary),
    Cast(Cast),
    FieldAccess(FieldAccess),
    EnumVariant(EnumVariant),
    Indexing(Indexing),
    LogicalNot(LogicalNot),
    FnCall(FnCall),
    Return(Option<Box<Expr>>),
    If(If),
    Loop(Loop),
    Break,
    Block(Block),
    DoNothing,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Rc<Ty>,
    pub span: Span,
}
