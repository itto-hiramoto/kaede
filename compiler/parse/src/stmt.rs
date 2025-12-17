use std::rc::Rc;

use kaede_ast::{
    expr::Expr,
    stmt::{Assign, AssignOp, Block, Let, LetKind, NormalLet, Stmt, StmtKind, TupleUnpack},
};
use kaede_ast_type::Ty;
use kaede_lex::token::TokenKind;
use kaede_span::Location;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl Parser {
    /// Consume a semicolon immediately following each statement
    pub fn block(&mut self) -> ParseResult<Block> {
        let mut body = Vec::new();

        let start = self.consume(&TokenKind::OpenBrace)?.start;

        loop {
            if let Ok(span) = self.consume(&TokenKind::CloseBrace) {
                return Ok(Block {
                    body,
                    span: self.new_span(start, span.finish),
                });
            } else if self.check(&TokenKind::Eoi) {
                return Err(ParseError::ExpectedError {
                    expected: TokenKind::CloseBrace.to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                }
                .into());
            }

            body.push(self.stmt()?);
            // Consume a semicolon immediately following each statement
            self.consume_semi()?;
        }
    }

    /// Semicolons are **not** consumed
    pub(super) fn stmt(&mut self) -> ParseResult<Stmt> {
        if self.check(&TokenKind::Let) {
            let l = self.let_()?;
            Ok(Stmt {
                span: l.span,
                kind: StmtKind::Let(l),
            })
        } else {
            let expr = self.expr()?;

            // Assignment statement
            let assign_op = if self.consume_b(&TokenKind::Eq) {
                Some(AssignOp::Eq)
            } else if self.consume_b(&TokenKind::PlusEq) {
                Some(AssignOp::AddAssign)
            } else if self.consume_b(&TokenKind::MinusEq) {
                Some(AssignOp::SubAssign)
            } else if self.consume_b(&TokenKind::AsteriskEq) {
                Some(AssignOp::MulAssign)
            } else if self.consume_b(&TokenKind::SlashEq) {
                Some(AssignOp::DivAssign)
            } else if self.consume_b(&TokenKind::PercentEq) {
                Some(AssignOp::RemAssign)
            } else {
                None
            };

            if let Some(op) = assign_op {
                let rhs = self.expr()?;

                let span = self.new_span(expr.span.start, rhs.span.finish);

                return Ok(Stmt {
                    kind: StmtKind::Assign(Box::new(Assign {
                        lhs: expr,
                        rhs,
                        op,
                        span,
                    })),
                    span,
                });
            }

            // Expression statement
            let expr_stmt = self.expr_stmt(expr);
            Ok(expr_stmt)
        }
    }

    fn expr_stmt(&mut self, e: Expr) -> Stmt {
        Stmt {
            span: e.span,
            kind: StmtKind::Expr(Rc::new(e)),
        }
    }

    fn let_(&mut self) -> ParseResult<Let> {
        let start = self.consume(&TokenKind::Let).unwrap().start;

        if self.check(&TokenKind::OpenParen) {
            // Tuple unpacking
            return self.tuple_unpacking(&start);
        }

        let mutability = self.consume_b(&TokenKind::Mut).into();

        let name = self.ident()?;

        if self.consume_b(&TokenKind::Eq) {
            let init = self.expr()?;

            let finish = init.span.finish;

            let span = self.new_span(start, finish);

            return Ok(Let {
                kind: LetKind::NormalLet(NormalLet {
                    name,
                    mutability,
                    init: Some(init.into()),
                    ty: Rc::new(Ty::new_var(span)),
                    span,
                }),
                span,
            });
        }

        self.consume(&TokenKind::Colon)?;
        let ty = self.ty()?;

        let init = if self.consume_b(&TokenKind::Eq) {
            Some(Rc::new(self.expr()?))
        } else {
            None
        };

        let span = match &init {
            Some(e) => e.span,
            None => self.new_span(start, name.span().finish),
        };

        Ok(Let {
            kind: LetKind::NormalLet(NormalLet {
                span,
                mutability,
                name,
                init,
                ty: ty.into(),
            }),
            span,
        })
    }

    // Expect that the Let token has already been consumed!
    fn tuple_unpacking(&mut self, start: &Location) -> ParseResult<Let> {
        self.consume(&TokenKind::OpenParen)?;

        let mut names = Vec::new();

        loop {
            let mutability = self.consume_b(&TokenKind::Mut).into();

            let ident = self.ident()?;

            if ident.as_str() == "_" {
                // Ignore field
                names.push(None);
            } else {
                names.push(Some((ident, mutability)));
            }

            if self.consume_b(&TokenKind::CloseParen) {
                break;
            }

            self.consume(&TokenKind::Comma)?;
        }

        self.consume(&TokenKind::Eq)?;

        let init = self.expr()?;

        let span = self.new_span(*start, init.span.finish);

        Ok(Let {
            kind: LetKind::TupleUnpack(TupleUnpack {
                names,
                init: init.into(),
                span,
            }),
            span,
        })
    }
}
