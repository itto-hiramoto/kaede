use std::{collections::HashMap, rc::Rc};

use kaede_ir::ty::{Mutability, PointerType, ReferenceType, Ty, TyKind, VarId};
use kaede_span::Span;

use crate::error::TypeInferError;

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

    /// Recursively substitute type variables using current bindings, rebuilding composite types.
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

            TyKind::Slice(elem_ty) => Ty {
                kind: TyKind::Slice(self.apply(elem_ty)).into(),
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

            TyKind::Closure(closure_ty) => Ty {
                kind: TyKind::Closure(kaede_ir::ty::ClosureType {
                    param_tys: closure_ty.param_tys.iter().map(|t| self.apply(t)).collect(),
                    ret_ty: self.apply(&closure_ty.ret_ty),
                    captures: closure_ty.captures.iter().map(|t| self.apply(t)).collect(),
                })
                .into(),
                mutability: Mutability::Not,
            }
            .into(),

            _ => t.clone(),
        }
    }

    /// Occurs check to prevent constructing infinite types (e.g., α = [α]).
    fn occurs(&self, var_id: usize, t: &Rc<Ty>) -> bool {
        match self.apply(t).kind.as_ref() {
            TyKind::Var(id) => *id == var_id,
            TyKind::Pointer(pty) => self.occurs(var_id, &pty.pointee_ty),
            TyKind::Reference(rty) => self.occurs(var_id, &rty.refee_ty),
            TyKind::Slice(elem_ty) => self.occurs(var_id, elem_ty),
            TyKind::Array(aty) => self.occurs(var_id, &aty.0),
            TyKind::Tuple(tys) => tys.iter().any(|t| self.occurs(var_id, t)),
            TyKind::Closure(closure_ty) => {
                closure_ty.param_tys.iter().any(|t| self.occurs(var_id, t))
                    || self.occurs(var_id, &closure_ty.ret_ty)
                    || closure_ty.captures.iter().any(|t| self.occurs(var_id, t))
            }
            _ => false,
        }
    }

    /// Pick which side of a unification is the type variable.
    /// The pure `Var`/`Var` case is handled earlier, so this keeps the mixed
    /// `Var` vs. concrete type logic in one place without duplicating matches.
    fn pick_var_case<'t>(a: &'t Rc<Ty>, b: &'t Rc<Ty>) -> Option<(VarId, &'t Rc<Ty>)> {
        match (a.kind.as_ref(), b.kind.as_ref()) {
            (TyKind::Var(id), _) => Some((*id, b)),
            (_, TyKind::Var(id)) => Some((*id, a)),
            _ => None,
        }
    }

    pub fn bind_var(&mut self, id: VarId, ty: Rc<Ty>) {
        // Find the ultimate representative for this var (follow Var->Var chains)
        let mut root = id;
        while let Some(TyKind::Var(next)) = self.subst.get(&root).map(|t| t.kind.as_ref()) {
            root = *next;
        }
        self.subst.insert(root, ty);
    }

    /// Unify two types, binding type variables as needed and producing errors on mismatches.
    pub fn unify(&mut self, a: &Rc<Ty>, b: &Rc<Ty>, span: Span) -> Result<(), TypeInferError> {
        let a = self.apply(a);
        let b = self.apply(b);

        if let (TyKind::Var(a_id), TyKind::Var(b_id)) = (a.kind.as_ref(), b.kind.as_ref()) {
            if a_id == b_id {
                return Ok(());
            }
            let (from, to) = if a_id > b_id { (a_id, &b) } else { (b_id, &a) };
            self.subst.insert(*from, to.clone());
            return Ok(());
        }

        if !matches!(a.kind.as_ref(), TyKind::Var(_))
            && !matches!(b.kind.as_ref(), TyKind::Var(_))
            && a == b
        {
            return Ok(());
        }

        // Handle type variables first (before Never check)
        // This ensures that unifying TyVar with Never binds TyVar to Never
        if let Some((id, other)) = Self::pick_var_case(&a, &b) {
            if self.occurs(id, other) {
                return Err(TypeInferError::OccursCheckFailed {
                    var_id: id,
                    ty: other.kind.to_string(),
                });
            }
            self.subst.insert(id, other.clone());
            return Ok(());
        }

        // Never type unifies with anything (bottom type)
        // This comes after type variable check so that TyVar gets bound to Never
        if matches!(a.kind.as_ref(), TyKind::Never) || matches!(b.kind.as_ref(), TyKind::Never) {
            return Ok(());
        }

        match (a.kind.as_ref(), b.kind.as_ref()) {
            (TyKind::Fundamental(f1), TyKind::Fundamental(f2)) => {
                if f1 == f2 {
                    return Ok(());
                }

                // No implicit type conversions - types must match exactly
                // Use explicit casts (as keyword) for type conversions
                Err(TypeInferError::CannotUnify {
                    a: f1.kind.to_string(),
                    b: f2.kind.to_string(),
                    span,
                })
            }

            (TyKind::Pointer(pty1), TyKind::Pointer(pty2)) => {
                self.unify(&pty1.pointee_ty, &pty2.pointee_ty, span)
            }
            (TyKind::Reference(rty1), TyKind::Reference(rty2)) => {
                self.unify(&rty1.refee_ty, &rty2.refee_ty, span)
            }
            (TyKind::Array(aty1), TyKind::Array(aty2)) => self.unify(&aty1.0, &aty2.0, span),
            (TyKind::Array((elem_ty, _)), TyKind::Slice(slice_elem))
            | (TyKind::Slice(slice_elem), TyKind::Array((elem_ty, _))) => {
                self.unify(elem_ty, slice_elem, span)
            }
            (TyKind::Tuple(tys1), TyKind::Tuple(tys2)) => {
                if tys1.len() != tys2.len() {
                    return Err(TypeInferError::TupleArityMismatchInUnify {
                        a: tys1
                            .iter()
                            .map(|t| t.kind.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        b: tys2
                            .iter()
                            .map(|t| t.kind.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    });
                }
                for (t1, t2) in tys1.iter().zip(tys2.iter()) {
                    self.unify(t1, t2, span)?;
                }
                Ok(())
            }

            (TyKind::Closure(c1), TyKind::Closure(c2)) => {
                if c1.param_tys.len() != c2.param_tys.len() {
                    return Err(TypeInferError::CannotUnify {
                        a: a.kind.to_string(),
                        b: b.kind.to_string(),
                        span,
                    });
                }

                for (p1, p2) in c1.param_tys.iter().zip(c2.param_tys.iter()) {
                    self.unify(p1, p2, span)?;
                }

                self.unify(&c1.ret_ty, &c2.ret_ty, span)?;

                Ok(())
            }

            // Handle unifying non-reference with reference by unwrapping
            (_, TyKind::Reference(rty)) => self.unify(&a, &rty.refee_ty, span),
            (TyKind::Reference(rty), _) => self.unify(&rty.refee_ty, &b, span),

            (x, y) => Err(TypeInferError::CannotUnify {
                a: x.to_string(),
                b: y.to_string(),
                span,
            }),
        }
    }
}
