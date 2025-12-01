use std::{collections::VecDeque, rc::Rc};

use kaede_ast::expr::{
    Args, ArrayLiteral, Binary, BinaryKind, Break, CharLiteral, Else, Expr, ExprKind, FnCall, If,
    Indexing, Int, IntKind, LogicalNot, Loop, Match, MatchArm, Return, StringLiteral,
    StructLiteral, TupleLiteral,
};
use kaede_ast_type::{Ty, TyKind};
use kaede_lex::token::TokenKind;
use kaede_span::Location;
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
        let mut node = self.access()?;

        loop {
            if self.consume_b(&TokenKind::As) {
                let right = self.access()?;
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

    // Field access or module item access or tuple indexing
    fn access(&mut self) -> ParseResult<Expr> {
        let mut node = self.indexing()?;

        loop {
            if self.consume_b(&TokenKind::Dot) {
                let right = self.indexing()?;
                node = Expr {
                    span: self.new_span(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::Access,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Array subscripting
    fn indexing(&mut self) -> ParseResult<Expr> {
        let mut node = self.scope_resolution()?;

        loop {
            if self.consume_b(&TokenKind::OpenBracket) {
                let index = self.expr()?;
                let finish = self.consume(&TokenKind::CloseBracket)?.finish;
                let span = self.new_span(node.span.start, finish);
                node = Expr {
                    span,
                    kind: ExprKind::Indexing(Indexing {
                        operand: node.into(),
                        index: index.into(),
                        span,
                    }),
                };
            } else {
                return Ok(node);
            }
        }
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

        if self.check(&TokenKind::Loop) {
            return self.loop_();
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

        // Array literal
        if self.check(&TokenKind::OpenBracket) {
            return self.array_literal();
        }

        if let Some(lit) = self.string_literal() {
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

            // Allow commas to be omitted only in the last arm.
            if !self.consume_b(&TokenKind::Comma) {
                self.consume_semi()?;
                break;
            }
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

        let elements = self.comma_separated_elements(&TokenKind::CloseBracket)?;

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

            self.consume(&TokenKind::Colon)?;

            let init = self.expr()?;

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

        let args = self.fn_call_args()?;

        let span = self.new_span(callees.0.span().start, args.1.finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall {
                callee: callees.0,
                generic_args: callees.1,
                args,
                span,
            }),
            span,
        })
    }

    /// Works with zero arguments
    fn fn_call_args(&mut self) -> ParseResult<Args> {
        let start = self.consume(&TokenKind::OpenParen)?.start;

        let mut args = VecDeque::new();

        if let Ok(span) = self.consume(&TokenKind::CloseParen) {
            // No arguments
            return Ok(Args(args, self.new_span(start, span.finish)));
        }

        loop {
            args.push_back(self.expr()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseParen)?.finish;
        Ok(Args(args, self.new_span(start, finish)))
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
}
