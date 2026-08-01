use std::{collections::VecDeque, rc::Rc};

use inkwell::{context::Context, values::IntValue};
use kaede_ast_type::{
    make_fundamental_type, FundamentalTypeKind, GenericArgs, Mutability, Ty, UserDefinedType,
};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

use crate::stmt::Block;

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub syb: Symbol,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ByteStringLiteral {
    pub bytes: Vec<u8>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CharLiteral {
    pub ch: char,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ByteLiteral {
    pub byte: u8,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub struct_ty: UserDefinedType,
    pub values: Vec<(Ident, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Arg {
    /// `None` for positional arguments, `Some(name)` for keyword arguments.
    pub name: Option<Ident>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Args {
    pub args: VecDeque<Arg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub callee: Box<Expr>,
    pub generic_args: Option<GenericArgs>,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Spawn {
    pub callee: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ChannelSend {
    pub channel: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ChannelRecv {
    pub channel: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Int {
    pub kind: IntKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
/// Integer literals are always non-negative.
/// Negative numbers like `-123` are represented as unary minus operator applied to `123`.
/// Type suffixes are not yet supported; all literals have inferred types.
pub enum IntKind {
    /// Integer literal with inferred type
    Unsuffixed(u64),
}

impl Int {
    pub fn as_u64(&self) -> u64 {
        let IntKind::Unsuffixed(n) = self.kind;
        n
    }

    pub fn as_llvm_int<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        let IntKind::Unsuffixed(n) = self.kind;
        // Default to i32 for LLVM codegen
        // (actual type will be determined by type inference)
        context.i32_type().const_int(n, true)
    }

    pub fn get_type(&self) -> Ty {
        // Unsuffixed literals have no concrete type yet
        // Type will be determined by type inference
        // Return i32 as a placeholder
        make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not, self.span)
    }
}

#[derive(Debug, Clone)]
pub struct Float {
    pub kind: FloatKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
/// Float literals are always non-negative.
/// Negative numbers like `-3.14` are represented as unary minus applied to `3.14`.
/// Type suffixes are not yet supported; all literals have inferred types.
pub enum FloatKind {
    /// Float literal with inferred type (defaults to f64 if unconstrained)
    Unsuffixed(f64),
}

impl Float {
    pub fn as_f64(&self) -> f64 {
        let FloatKind::Unsuffixed(n) = self.kind;
        n
    }

    pub fn get_type(&self) -> Ty {
        // Unsuffixed float literals default to f64 if unconstrained
        make_fundamental_type(FundamentalTypeKind::F64, Mutability::Not, self.span)
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
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,

    ScopeResolution,

    Cast,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BitNot {
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
    pub count: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleLiteral {
    pub elements: VecDeque<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<Ident>,
    pub body: Box<Expr>,
    pub captures: Vec<Ident>,
    pub span: Span,
}

/// Sometimes called `Array subscripting`
#[derive(Debug, Clone)]
pub struct Indexing {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Slicing {
    pub operand: Box<Expr>,
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Try {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Box<Expr>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Else {
    If(If),
    Block(Rc<Block>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Box<Else>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub val: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Box<Expr>,
    pub code: Rc<Expr>,
    pub is_catch_all: bool,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub value: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SelectBinding {
    /// `case value = ch.recv()` — bind the received value to a name.
    Named(Ident),
    /// `case _ = ch.recv()` — discard the received value.
    Wildcard,
}

#[derive(Debug, Clone)]
pub enum SelectOp {
    /// `case ch.send(value)`
    Send {
        channel: Box<Expr>,
        value: Box<Expr>,
    },
    /// `case binding = ch.recv()`
    Recv {
        channel: Box<Expr>,
        binding: SelectBinding,
    },
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    pub op: SelectOp,
    pub body: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Select {
    pub arms: Vec<SelectArm>,
    pub default: Option<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn collect_access_chain(&self, out: &mut Vec<Ident>) {
        use BinaryKind;
        use ExprKind;

        match &self.kind {
            ExprKind::Binary(Binary {
                kind: BinaryKind::Access,
                lhs,
                rhs,
                ..
            }) => {
                Self::collect_access_chain(lhs, out);

                if let ExprKind::Ident(ident) = &rhs.kind {
                    out.push(*ident)
                }
            }

            ExprKind::Ident(ident) => {
                out.push(*ident);
            }

            _ => {}
        }
    }

    pub fn collect_scope_resolution_chain(&self, out: &mut Vec<Ident>) {
        use BinaryKind;
        use ExprKind;

        match &self.kind {
            ExprKind::Binary(Binary {
                kind: BinaryKind::ScopeResolution,
                lhs,
                rhs,
                ..
            }) => {
                lhs.collect_scope_resolution_chain(out);
                if let ExprKind::Ident(ident) = &rhs.kind {
                    out.push(*ident);
                }
            }

            ExprKind::Ident(ident) => out.push(*ident),

            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(Int),
    Float(Float),
    StringLiteral(StringLiteral),
    ByteStringLiteral(ByteStringLiteral),
    ByteLiteral(ByteLiteral),
    CharLiteral(CharLiteral),
    Binary(Binary),
    Ident(Ident),
    GenericIdent((Ident, GenericArgs)),
    FnCall(FnCall),
    StructLiteral(StructLiteral),
    True,
    False,
    LogicalNot(LogicalNot),
    BitNot(BitNot),
    ArrayLiteral(ArrayLiteral),
    ArrayRepeat(ArrayRepeat),
    Indexing(Indexing),
    Slicing(Slicing),
    Try(Try),
    TupleLiteral(TupleLiteral),
    Closure(Closure),
    Spawn(Spawn),
    ChannelSend(ChannelSend),
    ChannelRecv(ChannelRecv),
    Return(Return),
    If(If),
    Loop(Loop),
    While(While),
    Break(Break),
    Match(Match),
    Select(Select),
    Block(Block),
    Ty(Ty),
}
