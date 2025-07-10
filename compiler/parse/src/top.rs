use std::{collections::VecDeque, rc::Rc};

use kaede_ast::top::{
    Enum, EnumVariant, Extern, Fn, FnDecl, GenericParams, Impl, Import, Param, Params, Path,
    Struct, StructField, TopLevel, TopLevelKind, Use, Visibility,
};
use kaede_ast_type::Mutability;
use kaede_lex::token::TokenKind;
use kaede_span::Location;

use crate::{error::ParseResult, Parser};

impl Parser {
    pub fn top_level(&mut self) -> ParseResult<TopLevel> {
        let vis = self.consume_b(&TokenKind::Pub).into();

        let token = self.first();

        let (span, kind) = match token.kind {
            TokenKind::Import => {
                let kind = self.import()?;
                (kind.span, TopLevelKind::Import(kind))
            }

            TokenKind::Fn => {
                let kind = self.func(vis)?;
                (kind.span, TopLevelKind::Fn(kind))
            }

            TokenKind::Struct => {
                let kind = self.struct_(vis)?;
                (kind.span, TopLevelKind::Struct(kind))
            }

            TokenKind::Impl => {
                let kind = self.impl_()?;
                (kind.span, TopLevelKind::Impl(kind))
            }

            TokenKind::Enum => {
                let kind = self.enum_(vis)?;
                (kind.span, TopLevelKind::Enum(kind))
            }

            TokenKind::Extern => {
                let kind = self.extern_(vis)?;
                (kind.span, TopLevelKind::Extern(kind))
            }

            TokenKind::Use => {
                let kind = self.use_(vis)?;
                (kind.span, TopLevelKind::Use(kind))
            }

            _ => unreachable!("{:?}", token.kind),
        };

        self.consume_semi()?;

        Ok(TopLevel { kind, span })
    }

    fn extern_(&mut self, vis: Visibility) -> ParseResult<Extern> {
        let start = self.consume(&TokenKind::Extern).unwrap().start;

        let lang_linkage = self.string_literal_internal();

        let fn_decl = self.fn_decl(vis)?;

        Ok(Extern {
            span: self.new_span(start, fn_decl.span.finish),
            lang_linkage,
            fn_decl,
            vis,
        })
    }

    fn generic_params(&mut self) -> ParseResult<Option<GenericParams>> {
        self.checkpoint();

        let start = self.consume(&TokenKind::Lt)?.start;

        let mut names = Vec::new();

        loop {
            names.push(self.ident()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(if let Ok(span) = self.consume(&TokenKind::Gt) {
            Some(GenericParams {
                names,
                span: self.new_span(start, span.finish),
            })
        } else {
            self.backtrack();
            None
        })
    }

    fn impl_(&mut self) -> ParseResult<Impl> {
        let start = self.consume(&TokenKind::Impl).unwrap().start;

        // Push generic param names to determine if types is generic types.
        let generic_params = if self.check(&TokenKind::Lt) {
            self.generic_params()?
        } else {
            None
        };

        if let Some(params) = &generic_params {
            self.generic_param_names_stack
                .push(params.names.iter().map(|i| i.symbol()).collect());
        }

        let ty = self.ty()?;

        self.consume(&TokenKind::OpenBrace)?;

        let mut items = Vec::new();

        loop {
            if let Ok(span) = self.consume(&TokenKind::CloseBrace) {
                // Pop generic param names.
                if generic_params.is_some() {
                    self.generic_param_names_stack.pop();
                }

                return Ok(Impl {
                    generic_params,
                    ty,
                    items: Rc::new(items),
                    span: self.new_span(start, span.finish),
                });
            }

            items.push(self.top_level()?);
        }
    }

    fn path(&mut self) -> ParseResult<Path> {
        let start = self.first().span.start;

        let mut segments = Vec::new();

        loop {
            let segment = self.ident()?;

            segments.push(segment);

            if !self.consume_b(&TokenKind::Dot) {
                break;
            }
        }

        let finish = self.first().span.finish;

        Ok(Path {
            segments,
            span: self.new_span(start, finish),
        })
    }

    fn use_(&mut self, vis: Visibility) -> ParseResult<Use> {
        let start = self.consume(&TokenKind::Use).unwrap().start;

        let path = self.path()?;

        let span = self.new_span(start, path.span.finish);

        Ok(Use { path, span, vis })
    }

    fn import(&mut self) -> ParseResult<Import> {
        let start = self.consume(&TokenKind::Import).unwrap().start;

        let module_path = self.path()?;

        let last = module_path.segments.last().unwrap().symbol();
        if !self.imported_modules.contains(&last) {
            self.imported_modules.push(last);
        }

        let span = self.new_span(start, module_path.span.finish);

        Ok(Import { module_path, span })
    }

    fn fn_decl(&mut self, vis: Visibility) -> ParseResult<FnDecl> {
        let start = self.consume(&TokenKind::Fn).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            self.generic_params()?
        } else {
            None
        };

        // Push generic param names to determine if types is generic types.
        if let Some(params) = &generic_params {
            self.generic_param_names_stack
                .push(params.names.iter().map(|i| i.symbol()).collect());
        }

        let params_start = self.consume(&TokenKind::OpenParen)?.start;

        let mutability = self.consume_b(&TokenKind::Mut).into();
        let has_self = self.consume_b(&TokenKind::Self_);
        if !has_self && mutability == Mutability::Mut {
            todo!("`mut is not allowed not in type");
        }
        let first_param = if has_self {
            let _ = self.consume(&TokenKind::Comma);
            None
        } else if self.check(&TokenKind::CloseParen) {
            None
        } else {
            let param = self.fn_param()?;
            let _ = self.consume(&TokenKind::Comma);
            Some(param)
        };

        let params = {
            let mut params = self.fn_params(params_start)?;
            if let Some(first_param) = first_param {
                params.v.push_front(first_param);
            }
            params
        };

        let (return_ty, finish) = if let Ok(span) = self.consume(&TokenKind::Colon) {
            (Some(Rc::new(self.ty()?)), span.finish)
        } else {
            (None, params.span.finish)
        };

        // Pop generic param names.
        if generic_params.is_some() {
            self.generic_param_names_stack.pop();
        }

        Ok(FnDecl {
            vis,
            self_: if has_self { Some(mutability) } else { None },
            name,
            generic_params,
            params,
            return_ty,
            span: self.new_span(start, finish),
        })
    }

    fn func(&mut self, vis: Visibility) -> ParseResult<Fn> {
        let decl = self.fn_decl(vis)?;

        let body = Rc::new(self.block()?);

        let span = self.new_span(decl.span.start, body.span.finish);

        Ok(Fn { decl, body, span })
    }

    fn fn_params(&mut self, span_start: Location) -> ParseResult<Params> {
        let mut params = VecDeque::new();

        if let Ok(span) = self.consume(&TokenKind::CloseParen) {
            return Ok(Params {
                v: params,
                span: self.new_span(span_start, span.finish),
                is_var_args: false,
            });
        }

        loop {
            if self.var_arg().is_some() {
                let finish = self.consume(&TokenKind::CloseParen)?.finish;
                break Ok(Params {
                    v: params,
                    span: self.new_span(span_start, finish),
                    is_var_args: true,
                });
            }

            params.push_back(self.fn_param()?);

            if !self.consume_b(&TokenKind::Comma) {
                let finish = self.consume(&TokenKind::CloseParen)?.finish;
                break Ok(Params {
                    v: params,
                    span: self.new_span(span_start, finish),
                    is_var_args: false,
                });
            }
        }
    }

    fn fn_param(&mut self) -> ParseResult<Param> {
        let name = self.ident()?;

        self.consume(&TokenKind::Colon)?;

        let ty = self.ty()?;

        Ok(Param {
            name,
            ty: Rc::new(ty),
        })
    }

    fn var_arg(&mut self) -> Option<()> {
        if self.consume_b(&TokenKind::DotDotDot) {
            Some(())
        } else {
            None
        }
    }

    fn enum_(&mut self, vis: Visibility) -> ParseResult<Enum> {
        let start = self.consume(&TokenKind::Enum).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            self.generic_params()?
        } else {
            None
        };

        // Push generic param names to determine if types is generic types.
        if let Some(params) = &generic_params {
            self.generic_param_names_stack
                .push(params.names.iter().map(|i| i.symbol()).collect());
        }

        self.consume(&TokenKind::OpenBrace)?;

        let variants = self.enum_variants()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        // Pop generic param names.
        if generic_params.is_some() {
            self.generic_param_names_stack.pop();
        }

        Ok(Enum {
            vis,
            name,
            generic_params,
            variants,
            span: self.new_span(start, finish),
        })
    }

    fn enum_variants(&mut self) -> ParseResult<Vec<EnumVariant>> {
        let mut variants = Vec::new();

        let mut offset = 0;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            let ty = if self.consume_b(&TokenKind::OpenParen) {
                let ty = self.ty()?;
                self.consume(&TokenKind::CloseParen)?;
                Some(ty)
            } else {
                None
            };

            variants.push(EnumVariant {
                name,
                ty,
                vis: Visibility::Public,
                offset,
            });

            // Allow commas to be omitted only in the last variant.
            if !self.consume_b(&TokenKind::Comma) {
                self.consume_semi()?;
                break;
            }

            offset += 1;
        }

        Ok(variants)
    }

    fn struct_(&mut self, vis: Visibility) -> ParseResult<Struct> {
        let start = self.consume(&TokenKind::Struct).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            self.generic_params()?
        } else {
            None
        };

        // Push generic param names to determine if types is generic types.
        if let Some(params) = &generic_params {
            self.generic_param_names_stack
                .push(params.names.iter().map(|i| i.symbol()).collect());
        }

        self.consume(&TokenKind::OpenBrace)?;

        let fields = self.struct_fields()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = self.new_span(start, finish);

        // Pop generic param names.
        if generic_params.is_some() {
            self.generic_param_names_stack.pop();
        }

        Ok(Struct {
            vis,
            name,
            generic_params,
            fields,
            span,
        })
    }

    fn struct_fields(&mut self) -> ParseResult<Vec<StructField>> {
        let mut fields = Vec::new();

        let mut offset = 0;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            self.consume(&TokenKind::Colon)?;

            let ty = self.ty()?;

            fields.push(StructField {
                name,
                ty: Rc::new(ty),
                vis: Visibility::Public,
                offset,
            });

            // Allow commas to be omitted only in the last field.
            if !self.consume_b(&TokenKind::Comma) {
                self.consume_semi()?;
                break;
            }

            offset += 1;
        }

        Ok(fields)
    }
}
