use std::{collections::HashMap, rc::Rc};

use kaede_ir::ty::Ty;
use kaede_symbol::Symbol;

/// Type environment for managing variable bindings
#[derive(Default)]
pub struct Env {
    vars: HashMap<Symbol, Rc<Ty>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    /// Insert a variable binding into the environment
    pub fn insert(&mut self, name: Symbol, ty: Rc<Ty>) {
        self.vars.insert(name, ty);
    }

    /// Look up a variable in the environment
    pub fn lookup(&self, name: &Symbol) -> Option<Rc<Ty>> {
        self.vars.get(name).cloned()
    }
}
