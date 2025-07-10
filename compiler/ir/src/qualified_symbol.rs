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

    pub fn mangle(&self) -> Symbol {
        let module_names = self.module_path().get_module_names_from_root();

        if module_names.is_empty() {
            return self.symbol();
        }

        Symbol::from(format!(
            "{}.{}",
            module_names
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join("."),
            self.symbol()
        ))
    }
}
