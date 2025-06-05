use std::rc::Rc;

use kaede_ast_type::{
    change_mutability, make_fundamental_type, FundamentalTypeKind, GenericArgs, GenericType,
    Mutability, PointerType, ReferenceType, Ty, TyKind, UserDefinedType,
};
use kaede_lex::token::TokenKind;
use kaede_symbol::Ident;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

fn wrap_in_reference(refee_ty: Ty) -> Ty {
    Ty {
        span: refee_ty.span,
        kind: TyKind::Reference(ReferenceType {
            refee_ty: refee_ty.into(),
        })
        .into(),
        mutability: Mutability::Not,
    }
}

impl Parser {
    pub fn generic_args(&mut self) -> ParseResult<Option<GenericArgs>> {
        self.checkpoint();

        let start = self.consume(&TokenKind::Lt)?.start;

        let mut types = Vec::new();

        loop {
            let ty = self.ty();

            match ty {
                Ok(ty) => types.push(Rc::new(ty)),
                Err(_) => {
                    self.backtrack();
                    return Ok(None);
                }
            }

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        if let Ok(span) = self.consume(&TokenKind::Gt) {
            Ok(Some(GenericArgs {
                types,
                span: self.new_span(start, span.finish),
            }))
        } else {
            self.backtrack();
            Ok(None)
        }
    }

    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        if self.consume_b(&TokenKind::Mut) {
            // Mutable type
            let mut ty = self.ty()?;
            change_mutability(&mut ty, Mutability::Mut);
            return Ok(ty);
        }

        if self.check(&TokenKind::Asterisk) {
            // Pointer type
            return self.pointer_ty();
        }

        if self.check(&TokenKind::OpenBracket) {
            // Array type
            return self.array_ty();
        }

        if self.check(&TokenKind::OpenParen) {
            // Tuple type
            return self.tuple_ty();
        }

        let type_ident = match self.ident() {
            Ok(t) => t,
            Err(_) => {
                return Err(ParseError::ExpectedError {
                    expected: "type".to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                })
            }
        };

        // External type
        if self.check(&TokenKind::Dot) {
            if let Some(r) = self.try_external_ty(type_ident)? {
                return Ok(r);
            }
        }

        // Span of the type identifier
        let span = type_ident.span();

        Ok(match type_ident.as_str() {
            "i8" => make_fundamental_type(I8, Mutability::Not, span),
            "u8" => make_fundamental_type(U8, Mutability::Not, span),
            "i32" => make_fundamental_type(I32, Mutability::Not, span),
            "u32" => make_fundamental_type(U32, Mutability::Not, span),
            "i64" => make_fundamental_type(I64, Mutability::Not, span),
            "u64" => make_fundamental_type(U64, Mutability::Not, span),
            "bool" => make_fundamental_type(Bool, Mutability::Not, span),

            "str" => wrap_in_reference(make_fundamental_type(Str, Mutability::Not, span)),

            // User defined type
            _ => {
                let generic_args = if self.check(&TokenKind::Lt) {
                    self.generic_args()?
                } else {
                    None
                };

                let mut is_generic_type = false;
                for names in self.generic_param_names_stack.iter() {
                    if names.contains(&type_ident.symbol()) {
                        is_generic_type = true;
                        break;
                    }
                }

                if is_generic_type {
                    Ty {
                        kind: TyKind::Generic(GenericType { name: type_ident }).into(),
                        mutability: Mutability::Not,
                        span,
                    }
                } else {
                    wrap_in_reference(Ty {
                        kind: TyKind::UserDefined(UserDefinedType::new(type_ident, generic_args))
                            .into(),
                        mutability: Mutability::Not,
                        span,
                    })
                }
            }
        })
    }

    fn try_external_ty(&mut self, maybe_module_name: Ident) -> ParseResult<Option<Ty>> {
        if !self.imported_modules.contains(&maybe_module_name.symbol()) {
            return Ok(None);
        }

        let start = self.consume(&TokenKind::Dot)?.start;

        let ty = self.ty()?;
        let span = self.new_span(start, ty.span.finish);

        Ok(Some(Ty::new_external(maybe_module_name, Rc::new(ty), span)))
    }

    fn pointer_ty(&mut self) -> ParseResult<Ty> {
        // *i32
        // ^
        let start = self.consume(&TokenKind::Asterisk)?.start;

        // *i32
        //  ^~~
        let ty = self.ty()?;

        Ok(Ty {
            span: self.new_span(start, ty.span.finish),
            kind: TyKind::Pointer(PointerType {
                pointee_ty: Rc::new(ty),
            })
            .into(),
            mutability: Mutability::Not,
        })
    }

    fn array_size(&mut self) -> ParseResult<u32> {
        let token = self.bump().unwrap();

        if let TokenKind::Int(s) = &token.kind {
            match s.parse() {
                Ok(n) => Ok(n),
                Err(_) => Err(ParseError::OutOfRangeForU32(token.span)),
            }
        } else {
            Err(ParseError::ExpectedError {
                expected: "array size".to_string(),
                but: token.kind.to_string(),
                span: token.span,
            })
        }
    }

    fn array_ty(&mut self) -> ParseResult<Ty> {
        // [i32; 58]
        // ^
        let start = self.consume(&TokenKind::OpenBracket)?.start;

        // [i32; 58]
        //  ^~~
        let element_ty = self.ty()?;

        // [i32; 58]
        //     ^
        self.consume(&TokenKind::Semi)?;

        // [i32; 58]
        //       ^~
        let size = self.array_size()?;

        // [i32; 58]
        //         ^
        let finish = self.consume(&TokenKind::CloseBracket)?.finish;

        Ok(wrap_in_reference(Ty {
            kind: TyKind::Array((element_ty.into(), size)).into(),
            mutability: Mutability::Not,
            span: self.new_span(start, finish),
        }))
    }

    fn tuple_ty(&mut self) -> ParseResult<Ty> {
        // (i32, bool)
        // ^
        let start = self.consume(&TokenKind::OpenParen)?.start;

        // (i32, bool)
        //  ^~~~~~~~~
        let mut field_types = Vec::new();

        loop {
            field_types.push(self.ty()?.into());

            if let Ok(span) = self.consume(&TokenKind::CloseParen) {
                return Ok(wrap_in_reference(Ty {
                    kind: TyKind::Tuple(field_types).into(),
                    mutability: Mutability::Not,
                    span: self.new_span(start, span.finish),
                }));
            }

            self.consume(&TokenKind::Comma)?;
        }
    }
}
