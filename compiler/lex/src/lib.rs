use std::collections::VecDeque;

use cursor::Cursor;
use kaede_span::file::FilePath;
use semi::insert_semi;
use token::{Token, TokenKind};

mod cursor;
mod semi;
pub mod token;

#[cfg(test)]
mod tests;

pub struct Lexer<'a> {
    source: &'a str,
    file: FilePath,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file: FilePath) -> Self {
        Self { source, file }
    }

    pub fn run(&self) -> VecDeque<Token> {
        insert_semi(self.run_without_insert_semi().into_iter(), self.file)
    }

    fn run_without_insert_semi(&self) -> VecDeque<Token> {
        let mut cursor = Cursor::new(self.source, self.file);
        let mut res = VecDeque::new();

        loop {
            let token = cursor.advance_token();

            match token.kind {
                TokenKind::Eoi => {
                    res.push_back(token);
                    break;
                }

                _ => res.push_back(token),
            }
        }

        res
    }
}

fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier
fn is_id_start(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier
fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

impl Cursor<'_> {
    /// Insert span
    fn create_token(&self, t: TokenKind) -> Token {
        Token {
            kind: t,
            span: self.span_builder.build(),
        }
    }

    fn advance_token(&mut self) -> Token {
        self.span_builder.start();

        let first_char = match self.bump() {
            Some(c) => c,
            None => return self.create_token(TokenKind::Eoi),
        };

        match first_char {
            '\n' => self.create_token(TokenKind::NewLine),

            // Whitespace
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            // Comments or Slash
            '/' => match self.first() {
                '/' => {
                    // Line comment (// This is a comment)
                    self.eat_line_comment();
                    self.create_token(TokenKind::NewLine)
                }
                '*' => {
                    // Block Comment (/* This is a comment */)
                    self.eat_block_comment();
                    self.advance_token()
                }
                '=' => {
                    self.bump().unwrap();
                    self.create_token(TokenKind::SlashEq)
                }
                _ => {
                    // '/'
                    self.create_token(TokenKind::Slash)
                }
            },

            // Number
            c @ '0'..='9' => {
                let n = self.number(c);
                self.create_token(TokenKind::Int(n))
            }

            // Byte string literal or byte literal
            'b' if self.first() == '"' => {
                // Consume the opening quote
                self.bump().unwrap();
                let bytes = self.byte_string_literal();
                self.create_token(TokenKind::ByteStringLiteral(bytes))
            }
            'b' if self.first() == '\'' => {
                // Consume the opening quote
                self.bump().unwrap();
                let byte = self.byte_literal();
                self.create_token(TokenKind::ByteLiteral(byte))
            }

            // Identifier or reserved words
            c if is_id_start(c) => {
                let ident = self.ident(c);

                match ident.as_str() {
                    // Reserved words
                    "fn" => self.create_token(TokenKind::Fn),
                    "return" => self.create_token(TokenKind::Return),
                    "let" => self.create_token(TokenKind::Let),
                    "if" => self.create_token(TokenKind::If),
                    "else" => self.create_token(TokenKind::Else),
                    "loop" => self.create_token(TokenKind::Loop),
                    "break" => self.create_token(TokenKind::Break),
                    "mut" => self.create_token(TokenKind::Mut),
                    "struct" => self.create_token(TokenKind::Struct),
                    "true" => self.create_token(TokenKind::True),
                    "false" => self.create_token(TokenKind::False),
                    "import" => self.create_token(TokenKind::Import),
                    "pub" => self.create_token(TokenKind::Pub),
                    "impl" => self.create_token(TokenKind::Impl),
                    "enum" => self.create_token(TokenKind::Enum),
                    "match" => self.create_token(TokenKind::Match),
                    "extern" => self.create_token(TokenKind::Extern),
                    "as" => self.create_token(TokenKind::As),
                    "self" => self.create_token(TokenKind::Self_),
                    "use" => self.create_token(TokenKind::Use),
                    "bridge" => self.create_token(TokenKind::Bridge),

                    // Identifier
                    _ => self.create_token(TokenKind::Ident(ident)),
                }
            }

            // String literal
            '"' => {
                let lit = self.string_literal();
                self.create_token(TokenKind::StringLiteral(lit))
            }

            // Character literal
            '\'' => {
                let ch = self.char_literal();
                self.create_token(TokenKind::CharLiteral(ch))
            }

            // Punctuators
            '(' => self.create_token(TokenKind::OpenParen),
            ')' => self.create_token(TokenKind::CloseParen),
            '{' => self.create_token(TokenKind::OpenBrace),
            '}' => self.create_token(TokenKind::CloseBrace),
            '[' => self.create_token(TokenKind::OpenBracket),
            ']' => self.create_token(TokenKind::CloseBracket),
            ',' => self.create_token(TokenKind::Comma),
            ';' => self.create_token(TokenKind::Semi),
            '.' => {
                if self.first() == '.' {
                    self.bump().unwrap();
                    if self.first() == '.' {
                        // ...
                        self.bump().unwrap();
                        self.create_token(TokenKind::DotDotDot)
                    } else {
                        // ..
                        self.create_token(TokenKind::DotDot)
                    }
                } else {
                    // .
                    self.create_token(TokenKind::Dot)
                }
            }
            ':' => {
                if self.first() == ':' {
                    // ::
                    self.bump().unwrap();
                    self.create_token(TokenKind::DoubleColon)
                } else {
                    // :
                    self.create_token(TokenKind::Colon)
                }
            }
            '-' => {
                if self.first() == '>' {
                    // ->
                    self.bump().unwrap();
                    self.create_token(TokenKind::Arrow)
                } else if self.first() == '=' {
                    // -=
                    self.bump().unwrap();
                    self.create_token(TokenKind::MinusEq)
                } else {
                    // -
                    self.create_token(TokenKind::Minus)
                }
            }

            // Operators
            '!' => {
                if self.first() == '=' {
                    // !=
                    self.bump().unwrap();
                    self.create_token(TokenKind::Ne)
                } else {
                    // !
                    self.create_token(TokenKind::LogicalNot)
                }
            }
            '+' => {
                if self.first() == '=' {
                    self.bump().unwrap();
                    self.create_token(TokenKind::PlusEq)
                } else {
                    self.create_token(TokenKind::Plus)
                }
            }
            '*' => {
                if self.first() == '=' {
                    self.bump().unwrap();
                    self.create_token(TokenKind::AsteriskEq)
                } else {
                    self.create_token(TokenKind::Asterisk)
                }
            }
            '%' => {
                if self.first() == '=' {
                    self.bump().unwrap();
                    self.create_token(TokenKind::PercentEq)
                } else {
                    self.create_token(TokenKind::Percent)
                }
            }
            '&' => {
                if self.first() == '&' {
                    // &&
                    self.bump().unwrap();
                    self.create_token(TokenKind::LogicalAnd)
                } else {
                    // &
                    self.create_token(TokenKind::And)
                }
            }
            '|' => {
                if self.first() == '|' {
                    // ||
                    self.bump().unwrap();
                    self.create_token(TokenKind::LogicalOr)
                } else {
                    // |
                    self.create_token(TokenKind::Pipe)
                }
            }
            '<' => {
                if self.first() == '=' {
                    // <=
                    self.bump().unwrap();
                    self.create_token(TokenKind::Le)
                } else {
                    // <
                    self.create_token(TokenKind::Lt)
                }
            }
            '>' => {
                if self.first() == '=' {
                    // >=
                    self.bump().unwrap();
                    self.create_token(TokenKind::Ge)
                } else {
                    // >
                    self.create_token(TokenKind::Gt)
                }
            }
            '=' => {
                if self.first() == '=' {
                    // ==
                    self.bump().unwrap();
                    self.create_token(TokenKind::DoubleEq)
                } else {
                    // =
                    self.create_token(TokenKind::Eq)
                }
            }

            c => unreachable!("{}", c),
        }
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(is_whitespace);
    }

    fn eat_line_comment(&mut self) {
        // // xxx
        //  ^
        self.bump();

        self.eat_while(|c| c != '\n');
    }

    fn eat_block_comment(&mut self) {
        // /* xxx */
        //  ^
        self.bump();

        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately
                        break;
                    }
                }
                _ => (),
            }
        }
    }

    /// Parse escape sequence for string/char literals
    /// Returns the unescaped character
    fn parse_char_escape(&mut self) -> char {
        // Assumes backslash has already been consumed
        let c = self.bump().unwrap();
        match c {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            _ => unreachable!(),
        }
    }

    /// Parse escape sequence for byte string/byte literals
    /// Returns the unescaped byte
    fn parse_byte_escape(&mut self) -> u8 {
        // Assumes backslash has already been consumed
        let c = self.bump().unwrap();
        match c {
            'n' => b'\n',
            'r' => b'\r',
            't' => b'\t',
            '\\' => b'\\',
            '"' => b'"',
            '\'' => b'\'',
            _ => unreachable!(),
        }
    }

    fn string_literal(&mut self) -> String {
        let mut lit = String::new();

        loop {
            let c = self.first();

            // Escape sequence
            if c == '\\' {
                self.bump().unwrap();
                lit.push(self.parse_char_escape());
                continue;
            }

            if c == '"' {
                // The last double quote is also consumed
                self.bump().unwrap();
                return lit;
            }

            lit.push(c);
            self.bump().unwrap();
        }
    }

    fn byte_string_literal(&mut self) -> Vec<u8> {
        let mut bytes = Vec::new();

        loop {
            let c = self.first();

            if c == '\\' {
                self.bump().unwrap();
                bytes.push(self.parse_byte_escape());
                continue;
            }

            if c == '"' {
                // Consume closing quote
                self.bump().unwrap();
                return bytes;
            }

            bytes.push(c as u8);
            self.bump().unwrap();
        }
    }

    fn char_literal(&mut self) -> char {
        let c = self.first();

        // Escape sequence
        let result = if c == '\\' {
            self.bump().unwrap();
            self.parse_char_escape()
        } else {
            // Regular character
            self.bump().unwrap();
            c
        };

        // Consume closing quote
        if self.first() == '\'' {
            self.bump().unwrap();
        } else {
            todo!()
        }

        result
    }

    fn byte_literal(&mut self) -> u8 {
        let c = self.first();

        // Escape sequence
        let result = if c == '\\' {
            self.bump().unwrap();
            self.parse_byte_escape()
        } else {
            // Regular byte (must be ASCII)
            self.bump().unwrap();
            c as u8
        };

        // Consume closing quote
        if self.first() == '\'' {
            self.bump().unwrap();
        } else {
            todo!()
        }

        result
    }

    fn number(&mut self, first_digit: char) -> String {
        assert!(first_digit.is_ascii_digit());

        let mut result = first_digit.to_string();

        loop {
            let c = self.first();

            if c.is_ascii_digit() {
                result.push(c);
                self.bump().unwrap();
            } else {
                return result;
            }
        }
    }

    fn ident(&mut self, first: char) -> String {
        let mut ident = String::from(first);

        loop {
            let c = self.first();

            if is_id_continue(c) {
                ident.push(c);
            } else {
                return ident;
            }

            self.bump().unwrap();
        }
    }
}
