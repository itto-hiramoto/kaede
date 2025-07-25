use kaede_symbol::Symbol;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModulePath {
    modules_from_root: Vec<Symbol>,
}

impl ModulePath {
    pub fn new(modules_from_root: Vec<Symbol>) -> Self {
        Self { modules_from_root }
    }

    pub fn get_module_names_from_root(&self) -> &[Symbol] {
        &self.modules_from_root
    }
}
