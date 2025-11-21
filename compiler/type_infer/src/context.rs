use std::{collections::HashMap, rc::Rc};

use kaede_ast_type::{FundamentalTypeKind, Mutability, PointerType, ReferenceType, Ty, TyKind, VarId};
use kaede_span::Span;

#[derive(Default)]
pub struct InferContext {
    next_var: VarId,
    subst: HashMap<VarId, Rc<Ty>>,
}

impl InferContext {
    pub fn fresh(&mut self) -> Rc<Ty> {
        let id = self.next_var;
        self.next_var += 1;
        self.new_var(id)
    }

    fn new_var(&self, id: usize) -> Rc<Ty> {
        Rc::new(Ty {
            kind: TyKind::Var(id).into(),
            mutability: Mutability::Not,
            span: Span::dummy(),
        })
    }

    fn apply(&self, t: &Rc<Ty>) -> Rc<Ty> {
        match t.kind.as_ref() {
            TyKind::Var(id) => {
                if let Some(tt) = self.subst.get(id) {
                    self.apply(tt)
                } else {
                    self.new_var(*id)
                }
            }

            TyKind::Pointer(pty) => Ty {
                kind: TyKind::Pointer(PointerType {
                    pointee_ty: self.apply(&pty.pointee_ty),
                })
                .into(),
                mutability: Mutability::Not,
                span: Span::dummy(),
            }
            .into(),

            TyKind::Reference(rty) => Ty {
                kind: TyKind::Reference(ReferenceType {
                    refee_ty: self.apply(&rty.refee_ty),
                })
                .into(),
                mutability: Mutability::Not,
                span: Span::dummy(),
            }
            .into(),

            TyKind::Array(aty) => Ty {
                kind: TyKind::Array((self.apply(&aty.0), aty.1)).into(),
                mutability: Mutability::Not,
                span: Span::dummy(),
            }
            .into(),

            TyKind::Tuple(tys) => Ty {
                kind: TyKind::Tuple(tys.iter().map(|t| self.apply(t)).collect()).into(),
                mutability: Mutability::Not,
                span: Span::dummy(),
            }
            .into(),

            _ => t.clone(),
        }
    }

    fn occurs(&self, var_id: usize, t: &Rc<Ty>) -> bool {
        match self.apply(t).kind.as_ref() {
            TyKind::Var(id) => *id == var_id,
            TyKind::Pointer(pty) => self.occurs(var_id, &pty.pointee_ty),
            TyKind::Reference(rty) => self.occurs(var_id, &rty.refee_ty),
            TyKind::Array(aty) => self.occurs(var_id, &aty.0),
            TyKind::Tuple(tys) => tys.iter().any(|t| self.occurs(var_id, t)),
            _ => false,
        }
    }

    fn pick_var_case<'t>(a: &'t Rc<Ty>, b: &'t Rc<Ty>) -> Option<(VarId, &'t Rc<Ty>)> {
        match (a.kind.as_ref(), b.kind.as_ref()) {
            (TyKind::Var(id), _) => Some((*id, b)),
            (_, TyKind::Var(id)) => Some((*id, a)),
            _ => None,
        }
    }

    fn unify(&mut self, a: &Rc<Ty>, b: &Rc<Ty>) -> Result<(), String> {
        let a = self.apply(a);
        let b = self.apply(b);

        if a == b {
            return Ok(());
        }

        if let Some((id, other)) = Self::pick_var_case(&a, &b) {
            if self.occurs(id, other) {
                return Err(format!(
                    "occurs check failed: α{} occurs in {:?}",
                    id, other
                ));
            }
            self.subst.insert(id, other.clone());
            return Ok(());
        }

        match (a.kind.as_ref(), b.kind.as_ref()) {
            (TyKind::Fundamental(f1), TyKind::Fundamental(f2)) => {
                if f1 == f2 {
                    return Ok(());
                }

                match (f1.kind, f2.kind) {
                    (FundamentalTypeKind::I8, FundamentalTypeKind::I32) => {
                        Ok(())
                    }
                    (FundamentalTypeKind::I32, FundamentalTypeKind::I64) => {
                        Ok(())
                    }
                    _ => Err(format!("cannot unify {:?} with {:?}", f1, f2)),
                }
            },

            (x, y) => Err(format!("cannot unify {:?} with {:?}", x, y)),
        }
    }
}
