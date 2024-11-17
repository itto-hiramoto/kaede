use kaede_symbol::Symbol;

use crate::SemanticAnalyzer;

#[derive(Debug, Clone)]
pub struct ModulePath {
    modules_from_root: Vec<Symbol>,
}

impl ModulePath {
    pub fn new(modules_from_root: Vec<Symbol>) -> Self {
        Self { modules_from_root }
    }

    pub fn mangle(&self) -> Symbol {
        self.modules_from_root
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join(".")
            .into()
    }
}

impl SemanticAnalyzer {
    pub fn mangle_fn_name(&self, name: &str) -> Symbol {
        format!("{}.{}", self.current_module_path.mangle(), name).into()
    }

    pub fn mangle_struct_name(&self, name: &str) -> Symbol {
        format!("{}.{}", self.current_module_path.mangle(), name).into()
    }
}
