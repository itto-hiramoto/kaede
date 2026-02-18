use std::{collections::VecDeque, rc::Rc};

use kaede_ast::expr::{
    Arg, Args, ArrayLiteral, ArrayRepeat, Binary, BinaryKind, Break, ByteLiteral,
    ByteStringLiteral, CharLiteral, Closure, Else, Expr, ExprKind, FnCall, If, Indexing, Int,
    IntKind, LogicalNot, Loop, Match, MatchArm, Return, Slicing, Spawn, StringLiteral,
    StructLiteral, TupleLiteral, While,
};
use kaede_ast_type::{GenericArgs, Ty, TyKind};
use kaede_lex::token::TokenKind;
use kaede_span::{Location, Span};
use kaede_symbol::{Ident, Symbol};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl Parser {
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> ParseResult<Expr> {
        let mut node = self.logical_and()?;

        loop {
            if self.consume_b(&TokenKind::LogicalOr) {
                let right = self.logical_and()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::LogicalOr,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn logical_and(&mut self) -> ParseResult<Expr> {
        let mut node = self.eq_or_ne()?;

        loop {
            if self.consume_b(&TokenKind::LogicalAnd) {
                let right = self.eq_or_ne()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::LogicalAnd,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn eq_or_ne(&mut self) -> ParseResult<Expr> {
        let mut node = self.lt_gt_le_ge()?;

        loop {
            if self.consume_b(&TokenKind::DoubleEq) {
                let right = self.lt_gt_le_ge()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Eq, right.into())),
                };
            } else if self.consume_b(&TokenKind::Ne) {
                let right = self.lt_gt_le_ge()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Ne, right.into())),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn lt_gt_le_ge(&mut self) -> ParseResult<Expr> {
        let mut node = self.add_or_sub()?;

        loop {
            if self.consume_b(&TokenKind::Lt) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Lt, right.into())),
                };
            } else if self.consume_b(&TokenKind::Le) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Le, right.into())),
                };
            } else if self.consume_b(&TokenKind::Gt) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Gt, right.into())),
                };
            } else if self.consume_b(&TokenKind::Ge) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Ge, right.into())),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn add_or_sub(&mut self) -> ParseResult<Expr> {
        let mut node = self.mul_or_div_or_rem()?;

        loop {
            if self.consume_b(&TokenKind::Plus) {
                let right = self.mul_or_div_or_rem()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Add, right.into())),
                };
            } else if self.consume_b(&TokenKind::Minus) {
                let right = self.mul_or_div_or_rem()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Sub, right.into())),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn mul_or_div_or_rem(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if self.consume_b(&TokenKind::Asterisk) {
                let right = self.unary()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Mul, right.into())),
                };
            } else if self.consume_b(&TokenKind::Slash) {
                let right = self.unary()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Div, right.into())),
                };
            } else if self.consume_b(&TokenKind::Percent) {
                let right = self.unary()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Rem, right.into())),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::Plus) {
            return self.cast();
        }

        if let Ok(span) = self.consume(&TokenKind::Minus) {
            // Subtracting a number from 0 inverts the sign
            let zero = Expr {
                kind: ExprKind::Int(Int {
                    kind: IntKind::Unsuffixed(0),
                    span,
                }),
                span,
            }
            .into();

            let e = self.cast()?;

            return Ok(Expr {
                span: self.new_span(span.start, e.span.finish),
                kind: ExprKind::Binary(Binary::new(zero, BinaryKind::Sub, e.into())),
            });
        }

        // Logical not
        if let Ok(span) = self.consume(&TokenKind::LogicalNot) {
            let operand = self.cast()?;
            let span = self.new_span(span.start, operand.span.finish);

            return Ok(Expr {
                kind: ExprKind::LogicalNot(LogicalNot {
                    operand: Box::new(operand),
                    span,
                }),
                span,
            });
        }

        self.cast()
    }

    fn cast(&mut self) -> ParseResult<Expr> {
        let mut node = self.postfix()?;

        loop {
            if self.consume_b(&TokenKind::As) {
                let right = self.postfix()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::Cast,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Postfix operators: field access (.), indexing ([]), slicing ([start:end]), and function calls (())
    /// All have the same precedence and are evaluated left-to-right
    fn postfix(&mut self) -> ParseResult<Expr> {
        let mut node = self.scope_resolution()?;

        loop {
            if self.consume_b(&TokenKind::Dot) {
                // Field access or module item access or tuple indexing
                let right = self.scope_resolution()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::Access,
                        right.into(),
                    )),
                };
            } else if self.consume_b(&TokenKind::OpenBracket) {
                // Array subscripting or slicing
                let span_start = node.span.start;

                // Check for slicing syntax with ':'.
                let (start, end, is_slicing) = if self.consume_b(&TokenKind::Colon) {
                    // [:end] or [:]
                    let end = if self.check(&TokenKind::CloseBracket) {
                        None
                    } else {
                        Some(self.expr()?)
                    };
                    (None, end, true)
                } else {
                    let first = self.expr()?;

                    if self.consume_b(&TokenKind::Colon) {
                        // [start:end] or [start:]
                        let end = if self.check(&TokenKind::CloseBracket) {
                            None
                        } else {
                            Some(self.expr()?)
                        };
                        (Some(first), end, true)
                    } else {
                        // [index]
                        let finish = self.consume(&TokenKind::CloseBracket)?.finish;
                        let span = self.new_span(span_start, finish);
                        node = Expr {
                            span,
                            kind: ExprKind::Indexing(Indexing {
                                operand: node.into(),
                                index: first.into(),
                                span,
                            }),
                        };
                        continue;
                    }
                };

                if self.check(&TokenKind::Colon) {
                    return Err(ParseError::ExpectedError {
                        expected: "]".to_string(),
                        but: ":".to_string(),
                        span: self.first().span,
                    }
                    .into());
                }

                if is_slicing {
                    let finish = self.consume(&TokenKind::CloseBracket)?.finish;
                    let span = self.new_span(span_start, finish);
                    node = Expr {
                        span,
                        kind: ExprKind::Slicing(Slicing {
                            operand: node.into(),
                            start: start.map(Box::new),
                            end: end.map(Box::new),
                            span,
                        }),
                    };
                }
            } else if self.check(&TokenKind::OpenParen) {
                // Function call
                node = self.fn_call_from_expr(node)?;
            } else {
                break;
            }
        }

        Ok(node)
    }

    /// Scope resolution
    fn scope_resolution(&mut self) -> ParseResult<Expr> {
        let mut node = self.primary()?;

        loop {
            if self.consume_b(&TokenKind::DoubleColon) {
                let right = self.primary()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::ScopeResolution,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.check(&TokenKind::Self_) {
            let span = self.consume(&TokenKind::Self_).unwrap();
            return Ok(Expr {
                span,
                kind: ExprKind::Ident(Ident::new(Symbol::from("self".to_string()), span)),
            });
        }

        if self.check(&TokenKind::Break) {
            return self.break_();
        }

        // Support closures starting with `|` or `||` with no spaces.
        if self.check(&TokenKind::LogicalOr) || self.check(&TokenKind::Pipe) {
            return self.closure();
        }

        if self.check(&TokenKind::Loop) {
            return self.loop_();
        }

        if self.check(&TokenKind::While) {
            return self.while_();
        }

        if self.check(&TokenKind::If) {
            let node = self.if_()?;
            return Ok(Expr {
                span: node.span,
                kind: ExprKind::If(node),
            });
        }

        if self.check(&TokenKind::Match) {
            return self.match_();
        }

        if self.check(&TokenKind::Return) {
            return self.return_();
        }

        if self.check(&TokenKind::Spawn) {
            return self.spawn_expr();
        }

        // Array literal
        if self.check(&TokenKind::OpenBracket) {
            return self.array_literal();
        }

        if self.check(&TokenKind::Dollar) {
            return self.interpolated_string_literal();
        }

        if let Some(lit) = self.string_literal() {
            return Ok(lit);
        }

        if let Some(lit) = self.byte_string_literal() {
            return Ok(lit);
        }

        if let Some(lit) = self.byte_literal() {
            return Ok(lit);
        }

        if let Some(lit) = self.char_literal() {
            return Ok(lit);
        }

        if let Some(lit) = self.boolean_literal() {
            return Ok(lit);
        }

        // Block
        if self.check(&TokenKind::OpenBrace) {
            let block = self.block()?;
            return Ok(Expr {
                span: block.span,
                kind: ExprKind::Block(block),
            });
        }

        // Integer
        if matches!(self.first().kind, TokenKind::Int(_)) {
            let int = self.int()?;
            return Ok(Expr {
                span: int.span,
                kind: ExprKind::Int(int),
            });
        }

        if self.consume_b(&TokenKind::OpenParen) {
            let node = self.expr()?;

            if let Ok(span) = self.consume(&TokenKind::Comma) {
                // Tuple literal
                return self.tuple_literal(span.start, node);
            }

            // '(' expr ')'
            self.consume(&TokenKind::CloseParen)?;
            return Ok(node);
        }

        if let Ok(ty) = self.ty_without_access_chain() {
            assert!(!matches!(ty.kind.as_ref(), TyKind::External(_, _)));

            if let TyKind::Reference(refty) = ty.kind.as_ref() {
                if let TyKind::UserDefined(udt) = refty.refee_ty.kind.as_ref() {
                    // Function call
                    if self.first().kind == TokenKind::OpenParen {
                        return self.fn_call(ty);
                    }

                    // Struct literal
                    if self.first().kind == TokenKind::OpenBrace {
                        // Check if this brace is from a block statement
                        // if x {}
                        // Such codes must not be interpreted as struct literals

                        // If parsing an expression for a condition now, skip
                        if !self.in_cond_expr {
                            return self.struct_literal(ty);
                        }
                    }

                    if let Some(generic_args) = &udt.generic_args {
                        return Ok(Expr {
                            span: ty.span,
                            kind: ExprKind::GenericIdent((udt.name, generic_args.clone())),
                        });
                    } else {
                        return Ok(Expr {
                            span: ty.span,
                            kind: ExprKind::Ident(udt.name),
                        });
                    }
                }
            }

            // Type
            return Ok(Expr {
                span: ty.span,
                kind: ExprKind::Ty(ty),
            });
        }

        Err(ParseError::ExpectedError {
            expected: "expression".to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        }
        .into())
    }

    fn closure(&mut self) -> ParseResult<Expr> {
        // Accept both `| |` and `||` (no space) closures.
        if self.check(&TokenKind::LogicalOr) {
            let start = self.consume(&TokenKind::LogicalOr)?.start;
            let body = self.expr()?;
            let span = self.new_span(start, body.span.finish);

            return Ok(Expr {
                span,
                kind: ExprKind::Closure(Closure {
                    params: Vec::new(),
                    body: Box::new(body),
                    captures: Vec::new(),
                    span,
                }),
            });
        }

        let start = self.consume(&TokenKind::Pipe)?.start;

        let mut params = Vec::new();

        if !self.check(&TokenKind::Pipe) {
            loop {
                params.push(self.ident()?);

                if !self.consume_b(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let _ = self.consume(&TokenKind::Pipe)?;

        let body = self.expr()?;

        let span = self.new_span(start, body.span.finish);

        Ok(Expr {
            span,
            kind: ExprKind::Closure(Closure {
                params,
                body: Box::new(body),
                captures: Vec::new(),
                span,
            }),
        })
    }

    fn match_(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Match)?.start;

        let value = self.cond_expr()?;

        self.consume(&TokenKind::OpenBrace)?;

        let mut arms = Vec::new();

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let pattern = self.expr()?;

            self.consume(&TokenKind::Eq)?;
            self.consume(&TokenKind::Gt)?;

            let code = self.expr()?;

            let is_catch_all =
                matches!(pattern.kind, ExprKind::Ident(ref ident) if ident.as_str() == "_");

            arms.push(MatchArm {
                pattern: Box::new(pattern),
                is_catch_all,
                code: Rc::new(code),
            });

            // Arm separators: comma or semicolon (inserted by newline).
            // This allows:
            // match x { A => 1, B => 2 }
            // match x {
            //   A => 1
            //   B => 2
            // }
            if self.consume_b(&TokenKind::Comma) {
                continue;
            }
            if self.check(&TokenKind::CloseBrace) {
                break;
            }
            if self.check_semi() {
                self.consume_semi()?;
                continue;
            }

            return Err(ParseError::ExpectedError {
                expected: "',' or '}'".to_string(),
                but: self.first().kind.to_string(),
                span: self.first().span,
            }
            .into());
        }

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = self.new_span(start, finish);

        Ok(Expr {
            span,
            kind: ExprKind::Match(Match {
                value: value.into(),
                arms,
                span,
            }),
        })
    }

    fn comma_separated_elements(&mut self, end: &TokenKind) -> ParseResult<Vec<Expr>> {
        let mut elems = Vec::new();

        loop {
            elems.push(self.expr()?);

            if self.check(end) {
                return Ok(elems);
            }

            self.consume(&TokenKind::Comma)?;
        }
    }

    fn comma_separated_elements_deque(&mut self, end: &TokenKind) -> ParseResult<VecDeque<Expr>> {
        let mut elems = VecDeque::new();

        loop {
            elems.push_back(self.expr()?);

            if self.check(end) {
                return Ok(elems);
            }

            self.consume(&TokenKind::Comma)?;
        }
    }

    /// (xxx, 58, true)
    /// ^~~~~
    /// Expect that this part has already been analyzed
    fn tuple_literal(&mut self, start: Location, first_elem: Expr) -> ParseResult<Expr> {
        let mut elements = self.comma_separated_elements_deque(&TokenKind::CloseParen)?;

        elements.push_front(first_elem);

        let finish = self.consume(&TokenKind::CloseParen).unwrap().finish;

        let span = self.new_span(start, finish);

        Ok(Expr {
            kind: ExprKind::TupleLiteral(TupleLiteral { elements, span }),
            span,
        })
    }

    fn array_literal(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::OpenBracket).unwrap().start;

        let first_elem = self.expr()?;

        if self.consume_b(&TokenKind::Semi) {
            let count = self.expr()?;
            let finish = self.consume(&TokenKind::CloseBracket)?.finish;
            let span = self.new_span(start, finish);

            return Ok(Expr {
                kind: ExprKind::ArrayRepeat(ArrayRepeat {
                    value: first_elem.into(),
                    count: count.into(),
                    span,
                }),
                span,
            });
        }

        let mut elements = vec![first_elem];

        if !self.check(&TokenKind::CloseBracket) {
            self.consume(&TokenKind::Comma)?;
            elements.extend(self.comma_separated_elements(&TokenKind::CloseBracket)?);
        }

        let finish = self.consume(&TokenKind::CloseBracket).unwrap().finish;

        let span = self.new_span(start, finish);

        Ok(Expr {
            kind: ExprKind::ArrayLiteral(ArrayLiteral { elements, span }),
            span,
        })
    }

    fn boolean_literal(&mut self) -> Option<Expr> {
        if let Ok(span) = self.consume(&TokenKind::True) {
            return Some(Expr {
                kind: ExprKind::True,
                span,
            });
        }

        if let Ok(span) = self.consume(&TokenKind::False) {
            return Some(Expr {
                kind: ExprKind::False,
                span,
            });
        }

        None
    }

    fn struct_literal(&mut self, ty: Ty) -> ParseResult<Expr> {
        let udt = match ty.kind.as_ref() {
            // X {}
            TyKind::Reference(rty)
                if matches!(rty.refee_ty.kind.as_ref(), TyKind::UserDefined(_)) =>
            {
                match rty.refee_ty.kind.as_ref() {
                    TyKind::UserDefined(udt) => udt.clone(),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        let mut inits = Vec::new();

        self.consume(&TokenKind::OpenBrace)?;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            let init = if self.consume_b(&TokenKind::Colon) {
                self.expr()?
            } else {
                Expr {
                    kind: ExprKind::Ident(name),
                    span: name.span(),
                }
            };

            inits.push((name, init));

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = self.new_span(udt.name.span().start, finish);

        Ok(Expr {
            span,
            kind: ExprKind::StructLiteral(StructLiteral {
                struct_ty: udt,
                values: inits,
                span,
            }),
        })
    }

    fn string_literal(&mut self) -> Option<Expr> {
        self.string_literal_internal().map(|s| Expr {
            span: s.span,
            kind: ExprKind::StringLiteral(s),
        })
    }

    fn parse_interpolated_parts(
        &mut self,
        source: &str,
        span: Span,
    ) -> ParseResult<(String, Vec<Expr>)> {
        let mut template = String::new();
        let mut exprs = Vec::new();
        let mut it = source.char_indices().peekable();

        while let Some((_, ch)) = it.next() {
            match ch {
                '{' => {
                    if matches!(it.peek(), Some((_, '{'))) {
                        it.next();
                        template.push('{');
                        continue;
                    }

                    let expr_start = it.peek().map(|(idx, _)| *idx).unwrap_or(source.len());

                    let mut paren_depth = 0usize;
                    let mut bracket_depth = 0usize;
                    let mut brace_depth = 0usize;
                    let mut in_string: Option<char> = None;
                    let mut escaped = false;
                    let mut expr_end = None;

                    for (idx, c) in it.by_ref() {
                        if let Some(quote) = in_string {
                            if escaped {
                                escaped = false;
                                continue;
                            }
                            if c == '\\' {
                                escaped = true;
                                continue;
                            }
                            if c == quote {
                                in_string = None;
                            }
                            continue;
                        }

                        match c {
                            '"' | '\'' => in_string = Some(c),
                            '(' => paren_depth += 1,
                            ')' => paren_depth = paren_depth.saturating_sub(1),
                            '[' => bracket_depth += 1,
                            ']' => bracket_depth = bracket_depth.saturating_sub(1),
                            '{' => brace_depth += 1,
                            '}' => {
                                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                                    expr_end = Some(idx);
                                    break;
                                }
                                brace_depth = brace_depth.saturating_sub(1);
                            }
                            _ => {}
                        }
                    }

                    let expr_end = expr_end.ok_or(ParseError::ExpectedError {
                        expected: "'}'".to_string(),
                        but: "end of interpolated string".to_string(),
                        span,
                    })?;

                    let expr_src = source[expr_start..expr_end].trim();
                    if expr_src.is_empty() {
                        return Err(ParseError::ExpectedError {
                            expected: "expression".to_string(),
                            but: "empty interpolation".to_string(),
                            span,
                        }
                        .into());
                    }

                    let mut parser = Parser::new(expr_src, self.file);
                    let expr = parser.expr()?;
                    if parser.check_semi() {
                        let _ = parser.consume_semi()?;
                    }
                    if !parser.is_eof() {
                        return Err(ParseError::ExpectedError {
                            expected: "end of interpolation expression".to_string(),
                            but: parser.first().kind.to_string(),
                            span: parser.first().span,
                        }
                        .into());
                    }

                    exprs.push(expr);
                    template.push_str("{}");
                }
                '}' => {
                    if matches!(it.peek(), Some((_, '}'))) {
                        it.next();
                        template.push('}');
                    } else {
                        return Err(ParseError::ExpectedError {
                            expected: "'}}'".to_string(),
                            but: "'}'".to_string(),
                            span,
                        }
                        .into());
                    }
                }
                _ => template.push(ch),
            }
        }

        Ok((template, exprs))
    }

    fn interpolated_string_literal(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Dollar)?.start;
        let lit = self
            .string_literal_internal()
            .ok_or(ParseError::ExpectedError {
                expected: "string literal".to_string(),
                but: self.first().kind.to_string(),
                span: self.first().span,
            })?;

        let (template, exprs) = self.parse_interpolated_parts(lit.syb.as_str(), lit.span)?;

        let template_expr = Expr {
            span: lit.span,
            kind: ExprKind::StringLiteral(StringLiteral {
                syb: template.into(),
                span: lit.span,
            }),
        };

        let mut args = VecDeque::new();
        args.push_back(Arg {
            name: None,
            span: template_expr.span,
            value: template_expr,
        });
        for expr in exprs {
            args.push_back(Arg {
                name: None,
                span: expr.span,
                value: expr,
            });
        }

        let args_span = self.new_span(
            args.front().unwrap().span.start,
            args.back().unwrap().span.finish,
        );
        let callee = Expr {
            kind: ExprKind::Ident(Ident::new(Symbol::from("__format".to_string()), lit.span)),
            span: lit.span,
        };
        let span = self.new_span(start, args_span.finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall {
                callee: Box::new(callee),
                generic_args: None,
                args: Args {
                    args,
                    span: args_span,
                },
                span,
            }),
            span,
        })
    }

    fn byte_string_literal(&mut self) -> Option<Expr> {
        if matches!(self.first().kind, TokenKind::ByteStringLiteral(_)) {
            let token = self.bump().unwrap();

            if let TokenKind::ByteStringLiteral(bytes) = token.kind {
                return Some(Expr {
                    span: token.span,
                    kind: ExprKind::ByteStringLiteral(ByteStringLiteral {
                        bytes,
                        span: token.span,
                    }),
                });
            }
        }

        None
    }

    pub fn string_literal_internal(&mut self) -> Option<StringLiteral> {
        if matches!(self.first().kind, TokenKind::StringLiteral(_)) {
            let token = self.bump().unwrap();

            Some(StringLiteral {
                syb: match token.kind {
                    TokenKind::StringLiteral(s) => s.into(),
                    _ => unreachable!(),
                },
                span: token.span,
            })
        } else {
            None
        }
    }

    fn char_literal(&mut self) -> Option<Expr> {
        if matches!(self.first().kind, TokenKind::CharLiteral(_)) {
            let token = self.bump().unwrap();

            if let TokenKind::CharLiteral(ch) = token.kind {
                return Some(Expr {
                    span: token.span,
                    kind: ExprKind::CharLiteral(CharLiteral {
                        ch,
                        span: token.span,
                    }),
                });
            }
        }

        None
    }

    fn byte_literal(&mut self) -> Option<Expr> {
        if matches!(self.first().kind, TokenKind::ByteLiteral(_)) {
            let token = self.bump().unwrap();

            if let TokenKind::ByteLiteral(byte) = token.kind {
                return Some(Expr {
                    span: token.span,
                    kind: ExprKind::ByteLiteral(ByteLiteral {
                        byte,
                        span: token.span,
                    }),
                });
            }
        }

        None
    }

    fn fn_call(&mut self, callee: Ty) -> ParseResult<Expr> {
        let callees = match callee.kind.as_ref() {
            // f()
            TyKind::Reference(rty)
                if matches!(rty.refee_ty.kind.as_ref(), TyKind::UserDefined(_)) =>
            {
                match rty.refee_ty.kind.as_ref() {
                    TyKind::UserDefined(udt) => (udt.name, udt.generic_args.clone()),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        let callee_expr = Expr {
            span: callees.0.span(),
            kind: ExprKind::Ident(callees.0),
        };

        self.fn_call_from_expr_with_generics(callee_expr, callees.1)
    }

    /// Works with zero arguments
    fn fn_call_args(&mut self) -> ParseResult<Args> {
        let start = self.consume(&TokenKind::OpenParen)?.start;

        let mut args = VecDeque::new();

        if let Ok(span) = self.consume(&TokenKind::CloseParen) {
            // No arguments
            return Ok(Args {
                args,
                span: self.new_span(start, span.finish),
            });
        }

        loop {
            let arg_start = self.first().span.start;

            let (name, value) = if let Some(token) = self.tokens.front() {
                match token.kind {
                    TokenKind::Ident(_) => {
                        let is_keyword =
                            matches!(self.tokens.get(1).map(|t| &t.kind), Some(TokenKind::Eq));

                        if is_keyword {
                            let name = self.ident()?;
                            self.consume(&TokenKind::Eq)?;
                            (Some(name), self.expr()?)
                        } else {
                            (None, self.expr()?)
                        }
                    }
                    _ => (None, self.expr()?),
                }
            } else {
                (None, self.expr()?)
            };

            let span = self.new_span(arg_start, value.span.finish);

            args.push_back(Arg { name, value, span });

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseParen)?.finish;
        Ok(Args {
            args,
            span: self.new_span(start, finish),
        })
    }

    fn fn_call_from_expr(&mut self, callee: Expr) -> ParseResult<Expr> {
        self.fn_call_from_expr_with_generics(callee, None)
    }

    fn fn_call_from_expr_with_generics(
        &mut self,
        callee: Expr,
        generic_args: Option<GenericArgs>,
    ) -> ParseResult<Expr> {
        let args = self.fn_call_args()?;
        let span = self.new_span(callee.span.start, args.span.finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall {
                callee: Box::new(callee),
                generic_args,
                args,
                span,
            }),
            span,
        })
    }

    pub fn int(&mut self) -> ParseResult<Int> {
        let token = self.bump().unwrap();

        match token.kind {
            TokenKind::Int(int_s) => {
                // Integer literals are always non-negative
                // Parse as u64 (unsuffixed integer literal)
                match int_s.parse() {
                    Ok(n) => Ok(Int {
                        kind: IntKind::Unsuffixed(n),
                        span: token.span,
                    }),

                    Err(_) => Err(ParseError::OutOfRangeForI32(token.span).into()),
                }
            }

            _ => Err(ParseError::ExpectedError {
                expected: "integer".to_string(),
                but: token.kind.to_string(),
                span: token.span,
            }
            .into()),
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        if matches!(self.first().kind, TokenKind::Ident(_)) {
            let token = self.bump().unwrap();
            if let TokenKind::Ident(ident) = token.kind {
                return Ok(Ident::new(ident.into(), token.span));
            }
        }

        Err(ParseError::ExpectedError {
            expected: "identifier".to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        }
        .into())
    }

    fn break_(&mut self) -> ParseResult<Expr> {
        let span = self.consume(&TokenKind::Break).unwrap();

        Ok(Expr {
            kind: ExprKind::Break(Break { span }),
            span,
        })
    }

    fn loop_(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Loop).unwrap().start;

        let body = self.block()?;

        let span = self.new_span(start, body.span.finish);

        Ok(Expr {
            kind: ExprKind::Loop(Loop { span, body }),
            span,
        })
    }

    fn while_(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::While).unwrap().start;

        let cond = self.cond_expr()?;

        let body = self.block()?;

        let span = self.new_span(start, body.span.finish);

        Ok(Expr {
            kind: ExprKind::While(While {
                cond: Box::new(cond),
                body,
                span,
            }),
            span,
        })
    }

    fn if_(&mut self) -> ParseResult<If> {
        let start = self.consume(&TokenKind::If).unwrap().start;

        let cond = self.cond_expr()?;

        let then = self.block()?;

        let else_ = self.else_()?.map(Box::new);

        let finish = match else_.as_ref() {
            Some(else_) => match else_.as_ref() {
                Else::Block(block) => block.span.finish,
                Else::If(if_) => if_.span.finish,
            },

            None => then.span.finish,
        };

        Ok(If {
            cond: cond.into(),
            then,
            else_,
            span: self.new_span(start, finish),
        })
    }

    /// `None` if there is no else
    fn else_(&mut self) -> ParseResult<Option<Else>> {
        if !self.consume_b(&TokenKind::Else) {
            return Ok(None);
        }

        if self.check(&TokenKind::If) {
            return Ok(Some(Else::If(self.if_()?)));
        }

        Ok(Some(Else::Block(Rc::new(self.block()?))))
    }

    fn return_(&mut self) -> ParseResult<Expr> {
        let span = self.consume(&TokenKind::Return).unwrap();

        if self.check_semi() {
            return Ok(Expr {
                kind: ExprKind::Return(Return { val: None, span }),
                span,
            });
        }

        let expr = self.expr()?;

        let span = self.new_span(span.start, expr.span.finish);

        Ok(Expr {
            kind: ExprKind::Return(Return {
                span,
                val: Some(expr.into()),
            }),
            span,
        })
    }

    fn spawn_expr(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Spawn)?.start;
        let callee = self.expr()?;
        let span = self.new_span(start, callee.span.finish);

        Ok(Expr {
            kind: ExprKind::Spawn(Spawn {
                callee: callee.into(),
                span,
            }),
            span,
        })
    }
}
