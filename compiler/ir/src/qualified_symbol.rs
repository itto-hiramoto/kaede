use kaede_symbol::Symbol;

use crate::module_path::ModulePath;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedSymbol {
    module_path: ModulePath,
    name: Symbol,
}

impl QualifiedSymbol {
    pub fn new(module_path: ModulePath, name: Symbol) -> Self {
        Self { module_path, name }
    }

    pub fn symbol(&self) -> Symbol {
        self.name
    }

    pub fn module_path(&self) -> &ModulePath {
        &self.module_path
    }
}
