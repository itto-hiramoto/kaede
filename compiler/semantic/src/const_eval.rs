use std::rc::Rc;

use kaede_ast as ast;
use kaede_ir::{self as ir, ty as ir_type};
use kaede_span::Span;
use kaede_symbol_table::{ConstValue, SymbolTableValue, SymbolTableValueKind};

use crate::SemanticAnalyzer;

impl SemanticAnalyzer {
    fn const_integer_from_symbol(value: &SymbolTableValue) -> Option<i128> {
        match &value.kind {
            SymbolTableValueKind::Variable(info) if info.is_const => info
                .const_value
                .as_ref()
                .map(|ConstValue::Integer(value)| *value),
            _ => None,
        }
    }

    /// Resolve `module.path.CONST` / `module::CONST` to an exported top-level integer const.
    fn evaluate_imported_module_const_expr(&self, node: &ast::expr::Binary) -> Option<i128> {
        use ast::expr::BinaryKind;

        let ast::expr::ExprKind::Ident(const_name) = &node.rhs.kind else {
            return None;
        };

        let mut module_segments = Vec::new();
        match node.kind {
            BinaryKind::Access => node.lhs.collect_access_chain(&mut module_segments),
            BinaryKind::ScopeResolution => node
                .lhs
                .collect_scope_resolution_chain(&mut module_segments),
            _ => return None,
        }

        if module_segments.is_empty() {
            return None;
        }

        let (_, module_path) = self
            .create_module_path_from_access_chain(
                &module_segments
                    .iter()
                    .map(|ident| ident.symbol())
                    .collect::<Vec<_>>(),
                node.lhs.span,
            )
            .ok()?;

        let module = self.modules.get(&module_path)?;
        let value = module.lookup_public_symbol(&const_name.symbol())?;
        let borrowed = value.borrow();
        Self::const_integer_from_symbol(&borrowed)
    }

    pub(crate) fn is_const_initializer(&self, expr: &ast::expr::Expr) -> bool {
        use ast::expr::{BinaryKind, ExprKind};

        match &expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::StringLiteral(_)
            | ExprKind::ByteStringLiteral(_)
            | ExprKind::ByteLiteral(_)
            | ExprKind::CharLiteral(_)
            | ExprKind::True
            | ExprKind::False => true,

            ExprKind::Ident(ident) => self
                .lookup_symbol_with_depth(ident.symbol())
                .map(|(value, _)| match &value.borrow().kind {
                    SymbolTableValueKind::Variable(info) => info.is_const,
                    _ => false,
                })
                .unwrap_or(false),

            ExprKind::LogicalNot(node) => self.is_const_initializer(&node.operand),
            ExprKind::BitNot(node) => self.is_const_initializer(&node.operand),

            ExprKind::Binary(node) => {
                if matches!(node.kind, BinaryKind::Access | BinaryKind::ScopeResolution) {
                    return self.evaluate_imported_module_const_expr(node).is_some();
                }

                if matches!(node.kind, BinaryKind::Cast) {
                    return self.is_const_initializer(&node.lhs);
                }

                self.is_const_initializer(&node.lhs) && self.is_const_initializer(&node.rhs)
            }

            ExprKind::TupleLiteral(node) => {
                node.elements.iter().all(|e| self.is_const_initializer(e))
            }

            _ => false,
        }
    }

    pub(crate) fn evaluate_integer_const_expr(&self, expr: &ast::expr::Expr) -> Option<i128> {
        use ast::expr::{BinaryKind, ExprKind};

        match &expr.kind {
            ExprKind::Int(int) => Some(i128::from(int.as_u64())),

            ExprKind::Ident(ident) => {
                self.lookup_symbol_with_depth(ident.symbol())
                    .and_then(|(value, _)| match &value.borrow().kind {
                        SymbolTableValueKind::Variable(info) => info
                            .const_value
                            .as_ref()
                            .map(|ConstValue::Integer(value)| *value),
                        _ => None,
                    })
            }

            ExprKind::BitNot(node) => self.evaluate_integer_const_expr(&node.operand).map(|v| !v),

            ExprKind::Binary(node) => {
                if matches!(node.kind, BinaryKind::Access | BinaryKind::ScopeResolution) {
                    return self.evaluate_imported_module_const_expr(node);
                }

                if matches!(node.kind, BinaryKind::Cast) {
                    return self.evaluate_integer_const_expr(&node.lhs);
                }

                let lhs = self.evaluate_integer_const_expr(&node.lhs)?;
                let rhs = self.evaluate_integer_const_expr(&node.rhs)?;

                match node.kind {
                    BinaryKind::Add => lhs.checked_add(rhs),
                    BinaryKind::Sub => lhs.checked_sub(rhs),
                    BinaryKind::Mul => lhs.checked_mul(rhs),
                    BinaryKind::Div => lhs.checked_div(rhs),
                    BinaryKind::Rem => lhs.checked_rem(rhs),
                    BinaryKind::BitOr => Some(lhs | rhs),
                    BinaryKind::BitXor => Some(lhs ^ rhs),
                    BinaryKind::BitAnd => Some(lhs & rhs),
                    BinaryKind::Shl => u32::try_from(rhs).ok().and_then(|rhs| lhs.checked_shl(rhs)),
                    BinaryKind::Shr => u32::try_from(rhs).ok().and_then(|rhs| lhs.checked_shr(rhs)),
                    _ => None,
                }
            }

            _ => None,
        }
    }

    /// Build a typed integer literal for a top-level `const` referenced at a use site.
    pub(crate) fn inline_top_level_const_int(
        &self,
        value: i128,
        ty: &Rc<ir_type::Ty>,
        span: Span,
    ) -> Option<ir::expr::Expr> {
        let int_kind = match ty.kind.as_ref() {
            ir_type::TyKind::Fundamental(kind) => match kind.kind {
                ir_type::FundamentalTypeKind::I8 => ir::expr::IntKind::I8(value as i8),
                ir_type::FundamentalTypeKind::U8 => ir::expr::IntKind::U8(value as u8),
                ir_type::FundamentalTypeKind::I16 => ir::expr::IntKind::I16(value as i16),
                ir_type::FundamentalTypeKind::U16 => ir::expr::IntKind::U16(value as u16),
                ir_type::FundamentalTypeKind::I32 => ir::expr::IntKind::I32(value as i32),
                ir_type::FundamentalTypeKind::U32 => ir::expr::IntKind::U32(value as u32),
                ir_type::FundamentalTypeKind::I64 => ir::expr::IntKind::I64(value as i64),
                ir_type::FundamentalTypeKind::U64 => ir::expr::IntKind::U64(value as u64),
                _ => return None,
            },
            _ => return None,
        };

        Some(ir::expr::Expr {
            kind: ir::expr::ExprKind::Int(ir::expr::Int {
                kind: int_kind,
                span,
            }),
            ty: ty.clone(),
            span,
        })
    }
}
