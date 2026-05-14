use std::rc::Rc;

use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::{
    stmt::Block,
    top::{Enum, FnDecl, Interface, InterfaceMethod, Struct},
    ty::{Ty, UserDefinedType},
};

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
pub struct GenericFnCall {
    pub callee: Rc<FnDecl>,
    pub generic_args: Vec<Rc<Ty>>,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Spawn {
    pub callee: Rc<FnDecl>,
    pub args: Vec<Expr>,
    pub arg_types: Vec<Rc<Ty>>,
    pub span: Span,
    pub is_main: bool,
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

#[derive(Debug, Clone)]
pub struct Float {
    pub kind: FloatKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FloatKind {
    F32(f32),
    F64(f64),
    /// Float literal with inferred type (value stored as f64)
    Infer(f64),
}

impl Float {
    pub fn as_f64(&self) -> f64 {
        use FloatKind::*;

        match self.kind {
            F32(n) => n as f64,
            F64(n) => n,
            Infer(n) => n,
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
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,
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

// Temporary node emitted by semantic when receiver type is still unknown.
// Must be rewritten to `FieldAccess` during type inference before monomorphize/codegen.
#[derive(Debug, Clone)]
pub struct UnresolvedFieldAccess {
    pub operand: Box<Expr>,
    pub field_name: Symbol,
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
pub struct Slicing {
    pub operand: Rc<Expr>,
    pub start: Box<Expr>,
    pub end: Box<Expr>,
    pub elem_ty: Rc<Ty>,
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
pub enum FormatPart {
    Literal(String),
    Placeholder,
}

#[derive(Debug, Clone)]
pub enum BuiltinFnCallKind {
    Unreachable,
    Str,
    Format(Vec<FormatPart>),
    SliceFromRawParts,
    PointerAdd,
    SizeOf,
    TypeSize(Rc<Ty>),
    Panic,
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

/// Per `(concrete_ty, interface)` pair: the concrete `FnDecl` that fills each
/// interface method slot. `methods` is aligned with `interface.methods`.
#[derive(Debug, Clone)]
pub struct ITable {
    pub interface: Rc<Interface>,
    pub concrete_ty: Rc<Ty>,
    pub methods: Vec<Rc<FnDecl>>,
}

/// Wrap a concrete value into a fat-pointer interface value
/// (`{ data: *T, itable: *ITable }`).
#[derive(Debug, Clone)]
pub struct InterfaceBox {
    pub value: Box<Expr>,
    pub itable: Rc<ITable>,
    pub span: Span,
}

/// Dynamic dispatch through an interface fat pointer's itable.
/// `method_index` indexes into the interface's methods.
#[derive(Debug, Clone)]
pub struct InterfaceMethodCall {
    pub receiver: Box<Expr>,
    pub method_index: usize,
    pub method: InterfaceMethod,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SelectOp {
    Send,
    Recv,
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    pub op: SelectOp,
    /// Resolved Channel<T> expression.
    pub channel: Box<Expr>,
    /// Channel element type T.
    pub elem_ty: Rc<Ty>,
    /// For send arms: the value expression (already type-checked against T).
    /// For recv arms: None.
    pub value: Option<Box<Expr>>,
    /// For recv arms with a name binding: the bound symbol whose type is
    /// `Option<T>`. `None` for `_ = ...` (discarded) or for send arms.
    pub binding: Option<Symbol>,
    /// `Option<T>` type for recv arms (used by codegen to materialize the
    /// Some/None value). `None` for send arms.
    pub option_ty: Option<Rc<Ty>>,
    /// Pre-resolved `Option` enum metadata for recv arms (variants + name).
    /// Set during semantic analysis so codegen does not need to chase
    /// Placeholder lookups.
    pub option_enum_info: Option<Rc<crate::top::Enum>>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Select {
    pub arms: Vec<SelectArm>,
    pub default: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(Int),
    Float(Float),
    StringLiteral(StringLiteral),
    ByteStringLiteral(ByteStringLiteral),
    ByteLiteral(ByteLiteral),
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
    UnresolvedFieldAccess(UnresolvedFieldAccess),
    TupleIndexing(TupleIndexing),
    EnumVariant(EnumVariant),
    Indexing(Indexing),
    Slicing(Slicing),
    LogicalNot(LogicalNot),
    BitNot(BitNot),
    FnCall(FnCall),
    GenericFnCall(GenericFnCall),
    Spawn(Spawn),
    FnPointer(FnPointer),
    Closure(Closure),
    Return(Option<Box<Expr>>),
    If(If),
    Loop(Loop),
    Break,
    Block(Block),
    BuiltinFnCall(BuiltinFnCall),
    InterfaceBox(InterfaceBox),
    InterfaceMethodCall(InterfaceMethodCall),
    Select(Select),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Rc<Ty>,
    pub span: Span,
}
