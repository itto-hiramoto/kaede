use std::rc::Rc;

use kaede_symbol::Symbol;

use crate::SemanticAnalyzer;
use kaede_ir_type as ir_type;

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

    pub fn mangle_generic_fn_name(&self, name: &str, generic_args: &[Rc<ir_type::Ty>]) -> Symbol {
        let mut mangled = format!("{}.{}", self.current_module_path().mangle(), name);
        for arg in generic_args {
            mangled.push_str(&format!("_{}", arg.kind));
        }
        mangled.into()
    }
}
