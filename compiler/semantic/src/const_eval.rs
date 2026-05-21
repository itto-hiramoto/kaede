use kaede_ast as ast;
use kaede_symbol_table::{ConstValue, SymbolTableValueKind};

use crate::SemanticAnalyzer;

impl SemanticAnalyzer {
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
                    return false;
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
}
