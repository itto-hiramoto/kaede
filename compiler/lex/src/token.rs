use kaede_span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Int(String),
    Float(String),
    Ident(String),
    StringLiteral(String),
    ByteStringLiteral(Vec<u8>),
    ByteLiteral(u8),
    CharLiteral(char),

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
    /// ":="
    ColonEq,
    /// "."
    Dot,
    /// ".."
    DotDot,
    /// "..."
    DotDotDot,
    /// "$"
    Dollar,
    /// "?"
    Question,
    /// "<-"
    LeftArrow,
    /// "->"
    Arrow,

    // Operators
    /// "+"
    Plus,
    /// "+="
    PlusEq,
    /// "-"
    Minus,
    /// "-="
    MinusEq,
    /// "*="
    AsteriskEq,
    /// "/="
    SlashEq,
    /// "%="
    PercentEq,
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
    /// "|"
    Pipe,
    /// "^"
    Caret,
    /// "~"
    Tilde,

    // Reserved words
    /// "fun"
    Fun,
    /// "fn" (reserved old syntax)
    OldFn,
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
    /// "while"
    While,
    /// "struct"
    Struct,
    /// "interface"
    Interface,
    /// "import"
    Import,
    /// "export"
    Export,
    /// "pub" (reserved old syntax)
    OldPub,
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
    /// "spawn"
    Spawn,
    /// "select"
    Select,
    /// "default"
    Default,
    /// "case"
    Case,

    /// "type"
    Type,

    /// "true"
    True,
    /// "false"
    False,

    /// End of input
    Eoi,

    NewLine,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenKind::*;

        write!(
            f,
            "{}",
            match self {
                Int(_) => "integer",
                Float(_) => "float",
                Ident(_) => "identifier",
                StringLiteral(_) => "string literal",
                ByteStringLiteral(_) => "byte string literal",
                ByteLiteral(_) => "byte literal",
                CharLiteral(_) => "character literal",

                OpenParen => "'('",
                CloseParen => "')'",
                OpenBrace => "'{'",
                CloseBrace => "'}'",
                OpenBracket => "'['",
                CloseBracket => "']'",
                Comma => "','",
                Semi => "';'",
                Colon => "':'",
                DoubleColon => "'::'",
                ColonEq => "':='",
                Dot => "'.'",
                DotDot => "'..'",
                DotDotDot => "'...'",
                Dollar => "'$'",
                Question => "'?'",
                LeftArrow => "'<-'",
                Arrow => "'->'",

                Plus => "'+'",
                PlusEq => "'+='",
                Minus => "'-'",
                MinusEq => "'-='",
                Asterisk => "'*'",
                AsteriskEq => "'*='",
                Slash => "'/'",
                SlashEq => "'/='",
                Percent => "'%'",
                PercentEq => "'%='",
                Eq => "'='",
                DoubleEq => "'=='",
                And => "'&'",
                Lt => "'<'",
                Le => "'<='",
                Gt => "'>'",
                Ge => "'>='",
                LogicalNot => "'!'",
                Ne => "'!='",
                LogicalAnd => "'&&'",
                LogicalOr => "'||'",
                Pipe => "'|'",
                Caret => "'^'",
                Tilde => "'~'",

                Fun => "'fun'",
                OldFn => "'fn'",
                Return => "'return'",
                Let => "'let'",
                Mut => "'mut'",
                If => "'if'",
                Else => "'else'",
                Loop => "'loop'",
                Break => "'break'",
                While => "'while'",
                Struct => "'struct'",
                Interface => "'interface'",
                Import => "'import'",
                Export => "'export'",
                OldPub => "'pub'",
                Impl => "'impl'",
                Enum => "'enum'",
                Match => "'match'",
                Extern => "'extern'",
                As => "'as'",
                Self_ => "'self'",
                Use => "'use'",
                Spawn => "'spawn'",
                Select => "'select'",
                Default => "'default'",
                Case => "'case'",
                Type => "'type'",

                True => "'true'",
                False => "'false'",

                Eoi => "end of input",

                NewLine => "newline",
            }
        )
    }
}
