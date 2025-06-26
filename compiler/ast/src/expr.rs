use std::{collections::VecDeque, rc::Rc};

use inkwell::{context::Context, values::IntValue};
use kaede_ast_type::{
    make_fundamental_type, FundamentalTypeKind, GenericArgs, Mutability, Ty, UserDefinedType,
};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

use crate::stmt::Block;

#[derive(Debug)]
pub struct StringLiteral {
    pub syb: Symbol,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructLiteral {
    pub struct_ty: UserDefinedType,
    pub values: Vec<(Ident, Expr)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Args(pub VecDeque<Expr>, pub Span);

#[derive(Debug)]
pub struct FnCall {
    pub callee: Ident,
    pub generic_args: Option<GenericArgs>,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug)]
pub struct Int {
    pub kind: IntKind,
    pub span: Span,
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

    pub fn as_llvm_int<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        use IntKind::*;

        match self.kind {
            I32(n) => context.i32_type().const_int(n as u64, true),
            U64(n) => context.i64_type().const_int(n, false),
        }
    }

    pub fn get_type(&self) -> Ty {
        match self.kind {
            IntKind::I32(_) => {
                make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not, self.span)
            }
            IntKind::U64(_) => {
                make_fundamental_type(FundamentalTypeKind::U64, Mutability::Not, self.span)
            }
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

    /// Field access or module item access
    Access,

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

    ScopeResolution,

    Cast,
}

#[derive(Debug)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub kind: BinaryKind,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(lhs: Box<Expr>, op: BinaryKind, rhs: Box<Expr>) -> Self {
        Self { lhs, kind: op, rhs }
    }
}

#[derive(Debug)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TupleLiteral {
    pub elements: VecDeque<Expr>,
    pub span: Span,
}

/// Sometimes called `Array subscripting`
#[derive(Debug)]
pub struct Indexing {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Loop {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug)]
pub enum Else {
    If(If),
    Block(Rc<Block>),
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Box<Else>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Return {
    pub val: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Box<Expr>,
    pub code: Rc<Expr>,
    pub is_catch_all: bool,
}

#[derive(Debug)]
pub struct Match {
    pub value: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(Int),
    StringLiteral(StringLiteral),
    Binary(Binary),
    Ident(Ident),
    GenericIdent((Ident, GenericArgs)),
    FnCall(FnCall),
    StructLiteral(StructLiteral),
    True,
    False,
    LogicalNot(LogicalNot),
    ArrayLiteral(ArrayLiteral),
    Indexing(Indexing),
    TupleLiteral(TupleLiteral),
    Return(Return),
    If(If),
    Loop(Loop),
    Break(Break),
    Match(Match),
    Block(Block),
    Ty(Rc<Ty>),
}
