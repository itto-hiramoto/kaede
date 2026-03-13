use std::str::Chars;

use kaede_span::{file::FilePath, SpanBuilder};

pub const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    chars: Chars<'a>,
    pub span_builder: SpanBuilder,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str, file: FilePath) -> Self {
        Self {
            chars: input.chars(),
            span_builder: SpanBuilder::new(file),
        }
    }

    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub fn second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty() || self.first() == EOF_CHAR
    }

    /// Advance to the next character
    pub fn bump(&mut self) -> Option<char> {
        let c = match self.chars.next() {
            Some(c) => c,
            r @ None => return r,
        };

        match c {
            '\n' => {
                self.span_builder.increase_line();
            }

            _ => {
                self.span_builder.increase_column();
            }
        }

        Some(c)
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump().unwrap();
        }
    }
}
