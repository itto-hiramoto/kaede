use std::rc::Rc;

use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{
    stmt::Block,
    top::{Enum, FnDecl, Struct},
    ty::{Ty, UserDefinedType},
};

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub syb: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CharLiteral {
    pub ch: char,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub struct_info: Rc<Struct>,
    pub values: Vec<(Symbol, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Args(pub Vec<Expr>, pub Span);

#[derive(Debug, Clone)]
pub struct FnCall {
    pub callee: Rc<FnDecl>,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Int {
    pub kind: IntKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IntKind {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    /// Integer literal with inferred type (value stored as i64)
    Infer(i64),
}

impl Int {
    pub fn as_u64(&self) -> u64 {
        use IntKind::*;

        match self.kind {
            I8(n) => n as u64,
            U8(n) => n as u64,
            I16(n) => n as u64,
            U16(n) => n as u64,
            I32(n) => n as u64,
            U32(n) => n as u64,
            I64(n) => n as u64,
            U64(n) => n,
            Infer(n) => n as u64,
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

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Rc<Expr>,
    pub kind: BinaryKind,
    pub rhs: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub operand: Box<Expr>,
    pub target_ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleIndexing {
    pub tuple: Rc<Expr>,
    pub element_ty: Rc<Ty>,
    pub index: u32,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub struct_info: Rc<Struct>,
    pub operand: Box<Expr>,
    pub field_name: Symbol,
    pub field_offset: u64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub enum_info: Rc<Enum>,
    pub variant_offset: u32,
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayRepeat {
    pub value: Box<Expr>,
    pub count: u32,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleLiteral {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<Symbol>,
    pub body: Box<Expr>,
    pub captures: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Indexing {
    pub operand: Rc<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Else {
    If(If),
    Block(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct EnumUnpack {
    pub name: Symbol,
    pub enum_ty: UserDefinedType,
    pub enum_value: Rc<Expr>,
    pub variant_ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub else_: Option<Box<Else>>,
    pub enum_unpack: Option<Box<EnumUnpack>>,
    pub is_match: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Symbol,
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BuiltinFnCallKind {
    Unreachable,
    Str,
}

#[derive(Debug, Clone)]
pub struct BuiltinFnCall {
    pub kind: BuiltinFnCallKind,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnPointer {
    pub decl: Rc<FnDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(Int),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    StructLiteral(StructLiteral),
    ArrayLiteral(ArrayLiteral),
    ArrayRepeat(ArrayRepeat),
    TupleLiteral(TupleLiteral),
    Variable(Variable),
    BooleanLiteral(bool),
    Binary(Binary),
    Cast(Cast),
    FieldAccess(FieldAccess),
    TupleIndexing(TupleIndexing),
    EnumVariant(EnumVariant),
    Indexing(Indexing),
    LogicalNot(LogicalNot),
    FnCall(FnCall),
    FnPointer(FnPointer),
    Closure(Closure),
    Return(Option<Box<Expr>>),
    If(If),
    Loop(Loop),
    Break,
    Block(Block),
    BuiltinFnCall(BuiltinFnCall),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Rc<Ty>,
    pub span: Span,
}
