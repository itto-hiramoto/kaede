use std::{collections::HashMap, rc::Rc};

use kaede_ir::ty::Ty;
use kaede_symbol::Symbol;

/// Type environment for managing variable bindings
pub struct Env {
    scopes: Vec<HashMap<Symbol, Rc<Ty>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Insert a variable binding into the environment
    pub fn insert(&mut self, name: Symbol, ty: Rc<Ty>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    /// Look up a variable in the environment
    pub fn lookup(&self, name: &Symbol) -> Option<Rc<Ty>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }
}
