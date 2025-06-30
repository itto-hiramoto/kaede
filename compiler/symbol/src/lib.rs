use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::RwLock,
};

use kaede_span::Span;
use once_cell::sync::Lazy;
use slab::Slab;

// Do not derive PartialEq or Eq!
// Unintended behavior is likely to be caused by comparisons involving span!
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Ident {
    name: Symbol,
    span: Span,
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    pub fn symbol(&self) -> Symbol {
        self.name
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

static SYMBOLS: Lazy<RwLock<Slab<Box<str>>>> = Lazy::new(|| RwLock::new(Slab::new()));

// Do not derive PartialEq, Hash, etc!
// We need to compare symbols(string) as well as id
#[derive(Clone, Copy)]
pub struct Symbol {
    id: usize,
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        let symbols = SYMBOLS.read().unwrap();
        // SAFETY: Strings are never removed from the slab, so this reference is valid
        // for the entire program duration
        unsafe { std::mem::transmute::<&str, &str>(&symbols[self.id]) }
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Self {
            id: SYMBOLS.write().unwrap().insert(value.into_boxed_str()),
        }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id || self.as_str() == other.as_str()
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // It is important not to include id in hash!
        SYMBOLS.read().unwrap()[self.id].hash(state);
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
