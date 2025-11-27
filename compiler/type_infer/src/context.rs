use std::{collections::HashMap, rc::Rc};

use kaede_ir::ty::{Mutability, PointerType, ReferenceType, Ty, TyKind, VarId};

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
        })
    }

    pub fn apply(&self, t: &Rc<Ty>) -> Rc<Ty> {
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
            }
            .into(),

            TyKind::Reference(rty) => Ty {
                kind: TyKind::Reference(ReferenceType {
                    refee_ty: self.apply(&rty.refee_ty),
                })
                .into(),
                mutability: Mutability::Not,
            }
            .into(),

            TyKind::Array(aty) => Ty {
                kind: TyKind::Array((self.apply(&aty.0), aty.1)).into(),
                mutability: Mutability::Not,
            }
            .into(),

            TyKind::Tuple(tys) => Ty {
                kind: TyKind::Tuple(tys.iter().map(|t| self.apply(t)).collect()).into(),
                mutability: Mutability::Not,
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

    pub fn unify(&mut self, a: &Rc<Ty>, b: &Rc<Ty>) -> anyhow::Result<()> {
        let a = self.apply(a);
        let b = self.apply(b);

        if a == b {
            return Ok(());
        }

        // Never type unifies with anything (bottom type)
        if matches!(a.kind.as_ref(), TyKind::Never) || matches!(b.kind.as_ref(), TyKind::Never) {
            return Ok(());
        }

        if let Some((id, other)) = Self::pick_var_case(&a, &b) {
            if self.occurs(id, other) {
                anyhow::bail!("occurs check failed: α{} occurs in {:?}", id, other);
            }
            self.subst.insert(id, other.clone());
            return Ok(());
        }

        match (a.kind.as_ref(), b.kind.as_ref()) {
            (TyKind::Fundamental(f1), TyKind::Fundamental(f2)) => {
                if f1 == f2 {
                    return Ok(());
                }

                // No implicit type conversions - types must match exactly
                // Use explicit casts (as keyword) for type conversions
                anyhow::bail!("cannot unify {:?} with {:?}", f1, f2)
            }

            (TyKind::Pointer(pty1), TyKind::Pointer(pty2)) => {
                self.unify(&pty1.pointee_ty, &pty2.pointee_ty)
            }
            (TyKind::Reference(rty1), TyKind::Reference(rty2)) => {
                self.unify(&rty1.refee_ty, &rty2.refee_ty)
            }
            (TyKind::Array(aty1), TyKind::Array(aty2)) => self.unify(&aty1.0, &aty2.0),
            (TyKind::Tuple(tys1), TyKind::Tuple(tys2)) => {
                if tys1.len() != tys2.len() {
                    anyhow::bail!("arity mismatch in tuple types: {:?} and {:?}", tys1, tys2);
                }
                for (t1, t2) in tys1.iter().zip(tys2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Handle unifying non-reference with reference by unwrapping
            (_, TyKind::Reference(rty)) => self.unify(&a, &rty.refee_ty),
            (TyKind::Reference(rty), _) => self.unify(&rty.refee_ty, &b),

            (x, y) => anyhow::bail!("cannot unify {:?} with {:?}", x, y),
        }
    }
}
