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

        write!(
            f,
            "{}",
            match self {
                Int(_) => "integer",
                Ident(_) => "identifier",
                StringLiteral(_) => "string literal",
                CharLiteral(_) => "character literal",

                OpenParen => "'('",
                CloseParen => "')'",
                OpenBrace => "'{{'",
                CloseBrace => "'}}'",
                OpenBracket => "'['",
                CloseBracket => "']'",
                Comma => "','",
                Semi => "';'",
                Colon => "':'",
                DoubleColon => "':'",
                Dot => "':'",
                DotDot => "':'",
                DotDotDot => "':'",
                Arrow => "':'",

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

                Fn => "'fn'",
                Return => "'return'",
                Let => "'let'",
                Mut => "'mut'",
                If => "'if'",
                Else => "'else'",
                Loop => "'loop'",
                Break => "'break'",
                Struct => "'struct'",
                Import => "'import'",
                Pub => "'pub'",
                Impl => "'impl'",
                Enum => "'enum'",
                Match => "'match'",
                Extern => "'extern'",
                As => "'as'",
                Self_ => "'self'",
                Use => "'use'",

                True => "'true'",
                False => "'false'",

                Bridge => "'bridge'",

                Eoi => "end of input",

                NewLine => "newline",
            }
        )
    }
}
