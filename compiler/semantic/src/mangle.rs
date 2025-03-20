use kaede_symbol::Symbol;

use crate::SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn mangle_fn_name(&self, name: &str) -> Symbol {
        format!("{}.{}", self.current_module_path().mangle(), name).into()
    }

    pub fn mangle_struct_name(&self, name: &str) -> Symbol {
        format!("{}.{}", self.current_module_path().mangle(), name).into()
    }

    pub fn mangle_enum_name(&self, name: &str) -> Symbol {
        format!("{}.{}", self.current_module_path().mangle(), name).into()
    }
}
