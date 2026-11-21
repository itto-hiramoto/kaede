use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use anyhow::{anyhow, Context};
use kaede_ir::{
    qualified_symbol::QualifiedSymbol,
    ty::{change_mutability_dup, FundamentalTypeKind, Mutability, Ty, TyKind},
};
use kaede_span::Span;
use kaede_symbol::Symbol;

use crate::error::SemanticError;

/// What a generic origin is instantiated into.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstanceKind {
    Fn,
    /// A generic struct or enum.
    Type,
    /// One method of a generic struct, enum, or slice impl.
    Method {
        name: Symbol,
        is_static: bool,
    },
}

/// How deeply types may nest inside one generic argument.
pub const MAX_INSTANCE_ARG_DEPTH: usize = 64;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceKey {
    pub kind: InstanceKind,
    pub origin: QualifiedSymbol,
    pub args: Vec<ConcreteTyKey>,
}

impl InstanceKey {
    pub fn from_types(
        kind: InstanceKind,
        origin: QualifiedSymbol,
        args: &[Rc<Ty>],
        span: Span,
    ) -> anyhow::Result<Self> {
        let args = args
            .iter()
            .enumerate()
            .map(|(index, ty)| {
                ConcreteTyKey::try_from_ty(ty).with_context(|| {
                    format!(
                        "cannot enqueue generic instance {origin:?}: argument {index} is not concrete at {}:{}",
                        span.start.line, span.start.column
                    )
                })
            })
            .collect::<anyhow::Result<_>>()?;

        Ok(Self { kind, origin, args })
    }
}

/// Identifies a concrete type as far as specialization is concerned: types that differ only in
/// mutability or closure captures share one generated instance.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConcreteTyKey(ConcreteTyKind);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConcreteTyKind {
    Fundamental(FundamentalTypeKind),
    UserDefined {
        origin: QualifiedSymbol,
        args: Vec<ConcreteTyKey>,
    },
    Closure {
        params: Vec<ConcreteTyKey>,
        return_ty: Box<ConcreteTyKey>,
    },
    Reference(Box<ConcreteTyKey>),
    Pointer(Box<ConcreteTyKey>),
    Slice(Box<ConcreteTyKey>),
    Array {
        element: Box<ConcreteTyKey>,
        size: u32,
    },
    Tuple(Vec<ConcreteTyKey>),
    Unit,
    Never,
}

impl ConcreteTyKey {
    fn depth(&self) -> usize {
        let max = |keys: &[ConcreteTyKey]| keys.iter().map(Self::depth).max().unwrap_or(0);
        1 + match &self.0 {
            ConcreteTyKind::Fundamental(_) | ConcreteTyKind::Unit | ConcreteTyKind::Never => 0,
            ConcreteTyKind::UserDefined { args, .. } | ConcreteTyKind::Tuple(args) => max(args),
            ConcreteTyKind::Closure { params, return_ty } => max(params).max(return_ty.depth()),
            ConcreteTyKind::Reference(inner)
            | ConcreteTyKind::Pointer(inner)
            | ConcreteTyKind::Slice(inner) => inner.depth(),
            ConcreteTyKind::Array { element, .. } => element.depth(),
        }
    }

    fn try_from_ty(ty: &Ty) -> anyhow::Result<Self> {
        let key = Self;
        let convert_all = |tys: &[Rc<Ty>]| {
            tys.iter()
                .map(|ty| Self::try_from_ty(ty))
                .collect::<anyhow::Result<Vec<_>>>()
        };

        Ok(match ty.kind.as_ref() {
            TyKind::Fundamental(fundamental) => key(ConcreteTyKind::Fundamental(fundamental.kind)),
            TyKind::UserDefined(udt) => {
                let (origin, args) = match &udt.generic_instance {
                    Some(instance) => (instance.origin.clone(), convert_all(&instance.args)?),
                    None => (udt.qualified_symbol(), Vec::new()),
                };
                key(ConcreteTyKind::UserDefined { origin, args })
            }
            TyKind::Closure(closure) => key(ConcreteTyKind::Closure {
                params: convert_all(&closure.param_tys)?,
                return_ty: Box::new(Self::try_from_ty(&closure.ret_ty)?),
            }),
            TyKind::Reference(reference) => key(ConcreteTyKind::Reference(Box::new(
                Self::try_from_ty(&reference.refee_ty)?,
            ))),
            TyKind::Pointer(pointer) => key(ConcreteTyKind::Pointer(Box::new(Self::try_from_ty(
                &pointer.pointee_ty,
            )?))),
            TyKind::Slice(element) => {
                key(ConcreteTyKind::Slice(Box::new(Self::try_from_ty(element)?)))
            }
            TyKind::Array((element, size)) => key(ConcreteTyKind::Array {
                element: Box::new(Self::try_from_ty(element)?),
                size: *size,
            }),
            TyKind::Tuple(elements) => key(ConcreteTyKind::Tuple(convert_all(elements)?)),
            TyKind::Unit => key(ConcreteTyKind::Unit),
            TyKind::Never => key(ConcreteTyKind::Never),
            TyKind::Infer(id) => {
                return Err(anyhow!("contains unresolved inference variable ?{id}"))
            }
            TyKind::GenericParam(param) => {
                return Err(anyhow!(
                    "contains generic parameter {}#{}",
                    param.origin.symbol(),
                    param.index
                ))
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstanceState {
    Queued,
    InProgress,
    Done,
}

#[derive(Debug, Clone)]
pub struct WorkItem {
    pub key: InstanceKey,
    pub args: Vec<Rc<Ty>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
struct Entry {
    state: InstanceState,
    args: Vec<Rc<Ty>>,
    span: Span,
}

#[derive(Debug, Default)]
pub struct InstanceWorklist {
    queue: VecDeque<InstanceKey>,
    entries: HashMap<InstanceKey, Entry>,
}

impl InstanceWorklist {
    pub fn enqueue(
        &mut self,
        kind: InstanceKind,
        origin: QualifiedSymbol,
        args: Vec<Rc<Ty>>,
        span: Span,
    ) -> anyhow::Result<bool> {
        let args = args
            .into_iter()
            .map(|arg| change_mutability_dup(arg, Mutability::Not))
            .collect::<Vec<_>>();
        let key = InstanceKey::from_types(kind, origin, &args, span)?;
        if key.args.iter().map(ConcreteTyKey::depth).max().unwrap_or(0) > MAX_INSTANCE_ARG_DEPTH {
            return Err(SemanticError::GenericInstantiationTooDeep {
                name: key.origin.symbol(),
                limit: MAX_INSTANCE_ARG_DEPTH,
                span,
            }
            .into());
        }
        if self.entries.contains_key(&key) {
            return Ok(false);
        }

        self.queue.push_back(key.clone());
        self.entries.insert(
            key,
            Entry {
                state: InstanceState::Queued,
                args,
                span,
            },
        );
        Ok(true)
    }

    pub fn next_item(&mut self) -> Option<WorkItem> {
        let key = self.queue.pop_front()?;
        let entry = self
            .entries
            .get_mut(&key)
            .expect("queued instance must have an entry");
        debug_assert_eq!(entry.state, InstanceState::Queued);
        entry.state = InstanceState::InProgress;

        Some(WorkItem {
            key,
            args: entry.args.clone(),
            span: entry.span,
        })
    }

    pub fn complete(&mut self, key: &InstanceKey) {
        let entry = self
            .entries
            .get_mut(key)
            .expect("completed instance must have been enqueued");
        debug_assert_eq!(entry.state, InstanceState::InProgress);
        entry.state = InstanceState::Done;
    }

    #[cfg(test)]
    pub fn state(&self, key: &InstanceKey) -> Option<InstanceState> {
        self.entries.get(key).map(|entry| entry.state)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use kaede_ir::{
        module_path::ModulePath,
        qualified_symbol::QualifiedSymbol,
        ty::{
            make_fundamental_type, FundamentalTypeKind, GenericInstanceInfo, GenericParamId,
            Mutability, PointerType, Ty, TyKind, UserDefinedType, UserDefinedTypeKind,
        },
    };
    use kaede_span::{Location, Span};
    use kaede_symbol::Symbol;

    use super::{InstanceKind, InstanceState, InstanceWorklist, MAX_INSTANCE_ARG_DEPTH};

    fn symbol(name: &str) -> QualifiedSymbol {
        QualifiedSymbol::new(ModulePath::root(), Symbol::from(name.to_owned()))
    }

    fn i32_ty(mutability: Mutability) -> Rc<Ty> {
        Rc::new(make_fundamental_type(FundamentalTypeKind::I32, mutability))
    }

    #[test]
    fn deduplicates_structurally_equal_instances_and_tracks_states() {
        let mut worklist = InstanceWorklist::default();
        let origin = symbol("identity");

        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                origin.clone(),
                vec![i32_ty(Mutability::Not)],
                Span::dummy()
            )
            .unwrap());
        assert!(!worklist
            .enqueue(
                InstanceKind::Fn,
                origin,
                vec![i32_ty(Mutability::Not)],
                Span::dummy()
            )
            .unwrap());

        let item = worklist.next_item().expect("queued item");
        assert_eq!(worklist.state(&item.key), Some(InstanceState::InProgress));
        assert_eq!(item.args.len(), 1);
        worklist.complete(&item.key);
        assert_eq!(worklist.state(&item.key), Some(InstanceState::Done));
        assert!(worklist.next_item().is_none());
    }

    #[test]
    fn ignores_top_level_argument_mutability() {
        let mut worklist = InstanceWorklist::default();

        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("identity"),
                vec![i32_ty(Mutability::Mut)],
                Span::dummy(),
            )
            .unwrap());
        assert!(!worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("identity"),
                vec![i32_ty(Mutability::Not)],
                Span::dummy(),
            )
            .unwrap());
        assert_eq!(
            worklist.next_item().unwrap().args[0].mutability,
            Mutability::Not,
        );
    }

    #[test]
    fn ignores_nested_pointer_mutability() {
        let mut worklist = InstanceWorklist::default();
        let ptr = |mutability| {
            Rc::new(Ty {
                kind: TyKind::Pointer(PointerType {
                    pointee_ty: i32_ty(mutability),
                })
                .into(),
                mutability: Mutability::Not,
            })
        };

        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("identity"),
                vec![ptr(Mutability::Mut)],
                Span::dummy()
            )
            .unwrap());
        assert!(!worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("identity"),
                vec![ptr(Mutability::Not)],
                Span::dummy()
            )
            .unwrap());
    }

    #[test]
    fn identifies_a_user_defined_instance_by_its_generic_origin() {
        let generic_origin = symbol("Box");
        let instance = |generated_name: &str| {
            Rc::new(Ty {
                kind: TyKind::UserDefined(UserDefinedType::with_generic_instance(
                    UserDefinedTypeKind::Placeholder(symbol(generated_name)),
                    GenericInstanceInfo::new(generic_origin.clone(), vec![i32_ty(Mutability::Not)]),
                ))
                .into(),
                mutability: Mutability::Not,
            })
        };
        let mut worklist = InstanceWorklist::default();

        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("consume"),
                vec![instance("Box_i32")],
                Span::dummy()
            )
            .unwrap());
        assert!(!worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("consume"),
                vec![instance("another_generated_name")],
                Span::dummy(),
            )
            .unwrap());
    }

    #[test]
    fn distinguishes_instance_kinds_of_one_origin() {
        let mut worklist = InstanceWorklist::default();
        let origin = symbol("Vector");
        let method = |name: &str| InstanceKind::Method {
            name: Symbol::from(name.to_owned()),
            is_static: false,
        };

        for kind in [InstanceKind::Type, method("push"), method("pop")] {
            assert!(worklist
                .enqueue(
                    kind,
                    origin.clone(),
                    vec![i32_ty(Mutability::Not)],
                    Span::dummy()
                )
                .unwrap());
        }
    }

    #[test]
    fn rejects_arguments_nested_beyond_the_depth_limit() {
        let wrap = |inner: Rc<Ty>| {
            Rc::new(Ty {
                kind: TyKind::Pointer(PointerType { pointee_ty: inner }).into(),
                mutability: Mutability::Not,
            })
        };
        let nested = |depth: usize| (1..depth).fold(i32_ty(Mutability::Not), |ty, _| wrap(ty));
        let mut worklist = InstanceWorklist::default();

        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("f"),
                vec![nested(MAX_INSTANCE_ARG_DEPTH)],
                Span::dummy()
            )
            .is_ok());
        assert!(worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("f"),
                vec![nested(MAX_INSTANCE_ARG_DEPTH + 1)],
                Span::dummy()
            )
            .is_err());
    }

    #[test]
    fn rejects_inference_variables_before_enqueue_with_source_location() {
        let span = Span {
            start: Location {
                line: 12,
                column: 7,
            },
            ..Span::dummy()
        };
        let inferred = Rc::new(Ty {
            kind: TyKind::Infer(3).into(),
            mutability: Mutability::Not,
        });
        let mut worklist = InstanceWorklist::default();

        let error = worklist
            .enqueue(InstanceKind::Fn, symbol("f"), vec![inferred], span)
            .unwrap_err()
            .to_string();
        assert!(error.contains("12:7"), "{error}");
        assert!(error.contains("not concrete"), "{error}");
        assert!(worklist.next_item().is_none());
    }

    #[test]
    fn rejects_generic_parameters_before_enqueue() {
        let parameter = Rc::new(Ty {
            kind: TyKind::GenericParam(GenericParamId::new(symbol("f"), 0)).into(),
            mutability: Mutability::Not,
        });
        let mut worklist = InstanceWorklist::default();

        let error = worklist
            .enqueue(
                InstanceKind::Fn,
                symbol("f"),
                vec![parameter],
                Span::dummy(),
            )
            .unwrap_err();
        assert!(format!("{error:#}").contains("generic parameter"));
        assert!(worklist.next_item().is_none());
    }
}
