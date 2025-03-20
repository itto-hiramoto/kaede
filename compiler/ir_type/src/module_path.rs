use kaede_symbol::Symbol;

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
