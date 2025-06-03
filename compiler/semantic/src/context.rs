use kaede_ir_type::ModulePath;

use crate::SemanticAnalyzer;

#[derive(Debug, Clone)]
pub struct AnalysisContext {
    module_path: ModulePath,
    is_inside_loop: bool,
}

impl AnalysisContext {
    pub fn new() -> Self {
        Self {
            module_path: ModulePath::new(vec![]),
            is_inside_loop: false,
        }
    }

    pub fn set_module_path(&mut self, path: ModulePath) {
        self.module_path = path;
    }
}

impl SemanticAnalyzer {
    pub fn current_module_path(&self) -> &ModulePath {
        &self.context.module_path
    }

    // Temporarily changes the current context, executes the provided closure.
    pub fn with_context<F, R>(&mut self, context: AnalysisContext, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_context = std::mem::replace(&mut self.context, context);
        let result = f(self);
        self.context = old_context;
        result
    }

    // Temporarily changes the current module path, executes the provided closure.
    pub fn with_external_module<F, R>(&mut self, path: ModulePath, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let mut new_context = self.context.clone();
        new_context.module_path = path;
        self.with_context(new_context, f)
    }

    // Temporarily sets the context to be inside a loop, executes the provided closure.
    pub fn with_inside_loop<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.context.is_inside_loop = true;
        let result = f(self);
        self.context.is_inside_loop = false;
        result
    }

    pub fn is_inside_loop(&self) -> bool {
        self.context.is_inside_loop
    }
}
