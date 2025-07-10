mod error;
mod expr;
mod stmt;
mod top;
mod ty;

use std::collections::VecDeque;

use error::{ParseError, ParseResult};
use kaede_ast::{expr::Expr, CompileUnit};
use kaede_lex::{
    token::{Token, TokenKind},
    Lexer,
};
use kaede_span::{file::FilePath, Location, Span};
use kaede_symbol::Symbol;

pub struct Parser {
    file: FilePath,

    tokens: VecDeque<Token>,
    consumed_tokens: VecDeque<Token>,
    // Stack of checkpoints - each checkpoint records the number of elements in consumed_tokens.
    checkpoint_stack: Vec<usize>,

    end_token: Option<Token>,

    in_cond_expr: bool,

    generic_param_names_stack: Vec<Vec<Symbol>>,

    imported_modules: Vec<Symbol>,
}

impl Parser {
    pub fn new(source: &str, file: FilePath) -> Self {
        Self {
            tokens: Lexer::new(source, file).run(),
            consumed_tokens: VecDeque::new(),
            checkpoint_stack: Vec::new(),
            end_token: None,
            in_cond_expr: false,
            generic_param_names_stack: Vec::new(),
            file,
            imported_modules: Vec::new(),
        }
    }

    pub fn run(&mut self) -> ParseResult<CompileUnit> {
        self.compile_unit(|self_| !self_.is_eof())
    }

    /// The argument specifies the condition under which the parsing is terminated
    ///
    /// When using "self", use the argument self
    ///
    /// Example:
    /// |self_| self_.xxx()
    fn compile_unit(
        &mut self,
        mut end_predicate: impl FnMut(&mut Self) -> bool,
    ) -> ParseResult<CompileUnit> {
        let mut compile_unit = CompileUnit {
            top_levels: Vec::new(),
        };

        while end_predicate(self) {
            let top = self.top_level()?;

            compile_unit.top_levels.push(top);
        }

        Ok(compile_unit)
    }

    /// Needed to **avoid confusion** between struct literals and block statements
    ///
    /// if x {}
    ///
    /// In such code as above,
    /// `then block` of `if statement` is not parsed as an initializer of a struct literals
    fn cond_expr(&mut self) -> ParseResult<Expr> {
        self.in_cond_expr = true;
        let cond = self.expr();
        self.in_cond_expr = false;
        cond
    }

    fn is_eof(&mut self) -> bool {
        self.end_token.is_some() || self.first().kind == TokenKind::Eoi
    }

    fn first(&mut self) -> &Token {
        self.tokens
            .front()
            .unwrap_or_else(|| self.end_token.as_ref().unwrap())
    }

    /// Advance to next token
    fn bump(&mut self) -> Option<Token> {
        let token = self.tokens.pop_front();

        // Record the consumed token
        if let Some(token) = &token {
            self.consumed_tokens.push_back(token.clone());
        }

        if let Some(token) = &token {
            if token.kind == TokenKind::Eoi {
                self.end_token = Some(token.clone());
            }
        }

        token
    }

    // Create a checkpoint.
    fn checkpoint(&mut self) {
        self.checkpoint_stack.push(self.consumed_tokens.len());
    }

    // Backtrack to the most recent checkpoint.
    fn backtrack(&mut self) {
        let checkpoint = self
            .checkpoint_stack
            .pop()
            .expect("Attempted to backtrack without a checkpoint");

        let len = self.consumed_tokens.len();
        let diff = len - checkpoint;

        for _ in 0..diff {
            self.tokens
                .push_front(self.consumed_tokens.pop_back().unwrap());
        }
    }

    /// Check without consuming tokens
    fn check(&mut self, tok: &TokenKind) -> bool {
        &self.first().kind == tok
    }

    fn consume(&mut self, tok: &TokenKind) -> ParseResult<Span> {
        let span = self.first().span;

        if &self.first().kind == tok {
            self.bump().unwrap();
            return Ok(span);
        }

        Err(ParseError::ExpectedError {
            expected: tok.to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        }
        .into())
    }

    /// Return boolean
    fn consume_b(&mut self, tok: &TokenKind) -> bool {
        if &self.first().kind == tok {
            self.bump().unwrap();
            return true;
        }

        false
    }

    /// Consume a semicolon
    /// ')' or '}', then success (Following Golang's rules)
    fn consume_semi(&mut self) -> ParseResult<Span> {
        if let Ok(span) = self.consume(&TokenKind::Semi) {
            return Ok(span);
        }

        if self.check(&TokenKind::CloseParen) || self.check(&TokenKind::CloseBrace) {
            return Ok(self.first().span);
        }

        Err(ParseError::ExpectedError {
            expected: TokenKind::Semi.to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        }
        .into())
    }

    /// Check a semicolon (Not consumed)
    /// ')' or '}', then success (Following Golang's rules)
    fn check_semi(&mut self) -> bool {
        if self.check(&TokenKind::Semi)
            || (self.check(&TokenKind::CloseParen) || self.check(&TokenKind::CloseBrace))
        {
            return true;
        }

        false
    }

    fn new_span(&self, start: Location, finish: Location) -> Span {
        Span::new(start, finish, self.file)
    }
}
