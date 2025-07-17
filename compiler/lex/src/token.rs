use kaede_span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Int(String),
    Ident(String),
    StringLiteral(String),

    // Punctuators
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// ","
    Comma,
    /// ";"
    Semi,
    /// ":"
    Colon,
    /// "::"
    DoubleColon,
    /// "."
    Dot,
    /// ".."
    DotDot,
    /// "..."
    DotDotDot,
    /// "->"
    Arrow,

    // Operators
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Asterisk,
    /// "/"
    Slash,
    /// "="
    Eq,
    /// "=="
    DoubleEq,
    /// "&"
    And,
    /// "<"
    Lt,
    /// "<="
    Le,
    /// ">"
    Gt,
    /// ">="
    Ge,
    /// "!"
    LogicalNot,
    /// "!="
    Ne,
    /// "%"
    Percent,
    /// "&&"
    LogicalAnd,
    /// "||"
    LogicalOr,

    // Reserved words
    /// "fn"
    Fn,
    /// "return"
    Return,
    /// "let"
    Let,
    /// "mut"
    Mut,
    /// "if"
    If,
    /// "else"
    Else,
    /// "loop"
    Loop,
    /// "break"
    Break,
    /// "struct"
    Struct,
    /// "import"
    Import,
    /// "pub"
    Pub,
    /// "impl"
    Impl,
    /// "enum"
    Enum,
    /// "match"
    Match,
    /// "extern"
    Extern,
    /// "as"
    As,
    /// "self"
    Self_,
    /// "use"
    Use,

    /// "true"
    True,
    /// "false"
    False,

    /// "bridge"
    Bridge,

    /// End of input
    Eoi,

    NewLine,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenKind::*;

        match self {
            Int(_) => write!(f, "integer"),
            Ident(_) => write!(f, "identifier"),
            StringLiteral(_) => write!(f, "string literal"),

            OpenParen => write!(f, "'('"),
            CloseParen => write!(f, "')'"),
            OpenBrace => write!(f, "'{{'"),
            CloseBrace => write!(f, "'}}'"),
            OpenBracket => write!(f, "'['"),
            CloseBracket => write!(f, "']'"),
            Comma => write!(f, "','"),
            Semi => write!(f, "';'"),
            Colon => write!(f, "':'"),
            DoubleColon => write!(f, "'::'"),
            Dot => write!(f, "'.'"),
            DotDot => write!(f, "'..'"),
            DotDotDot => write!(f, "'...'"),
            Arrow => write!(f, "'->'"),

            Plus => write!(f, "'+'"),
            Minus => write!(f, "'-'"),
            Asterisk => write!(f, "'*'"),
            Slash => write!(f, "'/'"),
            Eq => write!(f, "'='"),
            DoubleEq => write!(f, "'=='"),
            And => write!(f, "'&'"),
            Lt => write!(f, "'<'"),
            Le => write!(f, "'<='"),
            Gt => write!(f, "'>'"),
            Ge => write!(f, "'>='"),
            LogicalNot => write!(f, "'!'"),
            Ne => write!(f, "'!='"),
            Percent => write!(f, "'%'"),
            LogicalAnd => write!(f, "'&&'"),
            LogicalOr => write!(f, "'||'"),

            Fn => write!(f, "'fn'"),
            Return => write!(f, "'return'"),
            Let => write!(f, "'let'"),
            Mut => write!(f, "'mut'"),
            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),
            Loop => write!(f, "'loop'"),
            Break => write!(f, "'break'"),
            Struct => write!(f, "'struct'"),
            Import => write!(f, "'import'"),
            Pub => write!(f, "'pub'"),
            Impl => write!(f, "'impl'"),
            Enum => write!(f, "'enum'"),
            Match => write!(f, "'match'"),
            Extern => write!(f, "'extern'"),
            As => write!(f, "'as'"),
            Self_ => write!(f, "'self'"),
            Use => write!(f, "'use'"),

            True => write!(f, "'true'"),
            False => write!(f, "'false'"),

            Bridge => write!(f, "'bridge'"),

            Eoi => write!(f, "end of input"),

            NewLine => write!(f, "newline"),
        }
    }
}
