use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Component, PathBuf},
    rc::Rc,
};

use context::AnalysisContext;
use kaede_common::{kaede_autoload_dir, kaede_lib_src_dir};
use kaede_ir::{
    module_path::ModulePath,
    qualified_symbol::QualifiedSymbol,
    ty::{self as ir_type},
};

use kaede_parse::Parser;
use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};
use kaede_symbol_table::{
    GenericInfo, GenericKind, GenericSliceInfo, ScopedSymbolTable, SymbolTable, SymbolTableValue,
    SymbolTableValueKind,
};

mod context;
mod error;
mod expr;
mod stmt;
mod symbol_table;
mod top;
mod ty;

pub use error::SemanticError;
use kaede_ast::{self as ast, top::Visibility};
use kaede_ir as ir;
use kaede_type_infer::InferContext;
pub use top::TopLevelAnalysisResult;

use crate::context::{AnalyzeCommand, ModuleContext};

#[derive(Debug, Clone)]
struct ClosureCapture {
    base_depth: usize,
    captured: HashSet<Symbol>,
}

pub struct SemanticAnalyzer {
    modules: HashMap<ModulePath, ModuleContext>,
    context: AnalysisContext,
    generated_generics: Vec<ir::top::TopLevel>,
    generating_generics: HashSet<Symbol>,
    imported_module_paths: HashSet<PathBuf>,
    root_dir: PathBuf,
    autoloads_imported: bool,
    infer_context: InferContext,
    closure_capture_stack: Vec<ClosureCapture>,
    temp_symbol_counter: usize,
    generic_fn_substitutions: HashMap<QualifiedSymbol, HashMap<ir_type::VarId, Rc<ir_type::Ty>>>,
}

impl SemanticAnalyzer {
    fn insert_builtin_slice_symbol(module_context: &mut ModuleContext, module_path: ModulePath) {
        let symbol_table_value = SymbolTableValue::new(
            SymbolTableValueKind::Generic(
                GenericInfo::new(
                    GenericKind::Slice(GenericSliceInfo::new()),
                    module_path.clone(),
                )
                .into(),
            ),
            module_path,
        );

        module_context
            .insert_symbol_to_root_scope(
                Symbol::from("__builtin_slice".to_owned()),
                symbol_table_value,
                Visibility::Public,
                Span::dummy(),
            )
            .unwrap();
    }

    pub fn new(file_path: FilePath, root_dir: PathBuf) -> Self {
        if !root_dir.is_dir() {
            panic!("Root directory is not a directory");
        }

        // Set the current module name in the context.
        let module_path =
            Self::create_module_path_from_file_path(root_dir.clone(), file_path).unwrap();
        let mut context = AnalysisContext::new(module_path.clone());
        context.set_current_module_path(module_path.clone());

        let mut module_context = ModuleContext::new(file_path);
        Self::insert_builtin_slice_symbol(&mut module_context, module_path.clone());

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generated_generics: Vec::new(),
            generating_generics: HashSet::new(),
            imported_module_paths: HashSet::new(),
            root_dir,
            autoloads_imported: false,
            infer_context: InferContext::default(),
            closure_capture_stack: Vec::new(),
            temp_symbol_counter: 0,
            generic_fn_substitutions: HashMap::new(),
        }
    }

    pub fn new_for_single_file_test() -> Self {
        let module_path = ModulePath::new(vec![Symbol::from("test".to_string())]);
        let mut context = AnalysisContext::new(module_path.clone());
        context.set_current_module_path(module_path.clone());

        let mut module_context = ModuleContext::new(FilePath::from(PathBuf::from("test.kd")));
        Self::insert_builtin_slice_symbol(&mut module_context, module_path.clone());
        module_context.push_scope(SymbolTable::new());

        Self {
            modules: HashMap::from([(module_path, module_context)]),
            context,
            generated_generics: Vec::new(),
            generating_generics: HashSet::new(),
            imported_module_paths: HashSet::new(),
            root_dir: PathBuf::from("."),
            autoloads_imported: false,
            infer_context: InferContext::default(),
            closure_capture_stack: Vec::new(),
            temp_symbol_counter: 0,
            generic_fn_substitutions: HashMap::new(),
        }
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn dump_symbol_tables(&self) {
        for (module_path, module_context) in self.modules.iter() {
            eprintln!("Module: {module_path:?}");
            eprintln!("--- Symbol tables ---");
            for table in module_context.get_symbol_tables().iter() {
                table.dump();
            }
            eprintln!("---------------------");
            eprintln!("--- Private symbol table ---");
            module_context.get_private_symbol_table().dump();
            eprintln!("---------------------");
        }
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn dump_variables(&self) {
        for (module_path, module_context) in self.modules.iter() {
            eprintln!("Module: {module_path:?}");
            eprintln!("--- Variables ---");
            for symbol_table in module_context.get_symbol_tables().iter() {
                symbol_table.dump_variables();
            }
            eprintln!("---------------------");
        }
    }

    fn fresh_temp_symbol(&mut self, prefix: &str) -> Symbol {
        let name = format!("{prefix}{}", self.temp_symbol_counter);
        self.temp_symbol_counter += 1;
        Symbol::from(name)
    }

    fn create_module_path_from_file_path(
        root_dir: PathBuf,
        file_path: FilePath,
    ) -> anyhow::Result<ModulePath> {
        let mut diff_from_root = {
            // Get the canonical paths
            let kaede_lib_src_dir = kaede_lib_src_dir().canonicalize()?;
            let file_parent = file_path.path().parent().unwrap().canonicalize()?;

            // Try to strip the project root first, if that fails try the standard library root
            if let Ok(relative_path) = file_parent.strip_prefix(&root_dir.canonicalize()?) {
                relative_path
                    .components()
                    .map(|c| {
                        // The path is canonicalized, so that the components are all normal.
                        if let Component::Normal(os_str) = c {
                            Symbol::from(os_str.to_string_lossy().to_string())
                        } else {
                            unreachable!();
                        }
                    })
                    .collect::<Vec<_>>()
            } else if let Ok(relative_path) = file_parent.strip_prefix(&kaede_lib_src_dir) {
                // For standard library modules
                relative_path
                    .components()
                    .map(|c| {
                        if let Component::Normal(os_str) = c {
                            Symbol::from(os_str.to_string_lossy().to_string())
                        } else {
                            unreachable!();
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                return Err(anyhow::anyhow!(
                    "File path '{}' is not within project root '{}' or standard library root '{}'",
                    file_parent.display(),
                    root_dir.display(),
                    kaede_lib_src_dir.display()
                ));
            }
        };

        // Add the file name to the module path
        diff_from_root.push(
            file_path
                .path()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string()
                .into(),
        );

        Ok(ModulePath::new(diff_from_root))
    }

    pub fn lookup_symbol(&self, symbol: Symbol) -> Option<Rc<RefCell<SymbolTableValue>>> {
        let panic = || {
            panic!(
                "Module not found: {:?}",
                self.current_module_path().get_module_names_from_root()
            )
        };

        self.modules
            .get(self.current_module_path())
            .unwrap_or_else(panic)
            .lookup_symbol(&symbol)
            .or_else(|| {
                self.modules
                    .get(self.module_path())
                    .unwrap_or_else(panic)
                    .lookup_symbol(&symbol)
            })
    }

    pub fn lookup_qualified_symbol(
        &self,
        symbol: QualifiedSymbol,
    ) -> Option<Rc<RefCell<SymbolTableValue>>> {
        self.modules
            .get(symbol.module_path())
            .unwrap_or_else(|| {
                panic!(
                    "Module not found: {:?}",
                    symbol.module_path().get_module_names_from_root()
                )
            })
            .lookup_symbol(&symbol.symbol())
    }

    pub fn lookup_generic_argument(&self, symbol: Symbol) -> Option<Rc<ir_type::Ty>> {
        self.modules
            .get(self.current_module_path())
            .unwrap()
            .lookup_generic_argument(symbol)
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        span: Span,
    ) -> anyhow::Result<()> {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .insert_symbol_to_current_scope(symbol, value, span)
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue,
        vis: Visibility,
        span: Span,
    ) -> anyhow::Result<()> {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .insert_symbol_to_root_scope(symbol, value, vis, span)
    }

    pub fn create_generated_generic_key(&self, name: Symbol, args: &[Rc<ir_type::Ty>]) -> Symbol {
        format!(
            "{}_{}",
            name,
            args.iter()
                .map(|ty| match ty.kind.as_ref() {
                    ir_type::TyKind::Var(id) => format!("var{id}"),
                    _ => ty.kind.to_string(),
                })
                .collect::<Vec<_>>()
                .join("_")
        )
        .into()
    }

    pub fn slice_method_parent_name(&self, elem_ty: &Rc<ir_type::Ty>) -> Symbol {
        format!("slice<{}>", elem_ty.kind).into()
    }

    pub fn create_method_key(
        &self,
        parent_name: Symbol,
        method_name: Symbol,
        is_static: bool,
    ) -> Symbol {
        if is_static {
            format!("{parent_name}::{method_name}").into()
        } else {
            format!("{parent_name}.{method_name}").into()
        }
    }

    pub fn push_scope(&mut self, symbol_table: SymbolTable) {
        let module_path = self.current_module_path().clone();
        self.modules
            .get_mut(&module_path)
            .unwrap()
            .push_scope(symbol_table);
    }

    pub fn pop_scope(&mut self) {
        let module_path = self.current_module_path().clone();
        self.modules.get_mut(&module_path).unwrap().pop_scope();
    }

    fn symbol_table_depth(&self) -> usize {
        let module_path = self.current_module_path();
        self.modules
            .get(module_path)
            .expect("Module context must exist")
            .symbol_table_depth()
    }

    fn lookup_symbol_with_depth(
        &self,
        symbol: Symbol,
    ) -> Option<(Rc<RefCell<SymbolTableValue>>, usize)> {
        let module_path = self.current_module_path();
        self.modules
            .get(module_path)
            .expect("Module context must exist")
            .lookup_symbol_with_depth(&symbol)
    }

    fn push_closure_capture(&mut self, base_depth: usize) {
        self.closure_capture_stack.push(ClosureCapture {
            base_depth,
            captured: HashSet::new(),
        });
    }

    fn pop_closure_capture(&mut self) -> Option<ClosureCapture> {
        self.closure_capture_stack.pop()
    }

    fn register_capture(&mut self, symbol: Symbol, depth: usize) {
        if let Some(state) = self.closure_capture_stack.last_mut() {
            if depth < state.base_depth {
                state.captured.insert(symbol);
            }
        }
    }

    fn inject_generated_generics_to_compile_unit(
        &mut self,
        mut compile_unit: ir::CompileUnit,
    ) -> ir::CompileUnit {
        for top_level in self.generated_generics.iter() {
            compile_unit.top_levels.push(top_level.clone());
        }

        compile_unit
    }

    fn merge_generic_fn_substitutions(
        &mut self,
        substitutions: HashMap<QualifiedSymbol, HashMap<ir_type::VarId, Rc<ir_type::Ty>>>,
    ) {
        for (name, subst) in substitutions {
            self.generic_fn_substitutions
                .entry(name)
                .or_default()
                .extend(subst);
        }
    }

    fn apply_var_subst_to_ty(
        ty: &Rc<ir_type::Ty>,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) -> Rc<ir_type::Ty> {
        match ty.kind.as_ref() {
            ir_type::TyKind::Var(id) => subst.get(id).cloned().unwrap_or_else(|| ty.clone()),
            ir_type::TyKind::Pointer(pty) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                    pointee_ty: Self::apply_var_subst_to_ty(&pty.pointee_ty, subst),
                })
                .into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Reference(rty) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Reference(ir_type::ReferenceType {
                    refee_ty: Self::apply_var_subst_to_ty(&rty.refee_ty, subst),
                })
                .into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Slice(elem) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Slice(Self::apply_var_subst_to_ty(elem, subst)).into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Array((elem, size)) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Array((Self::apply_var_subst_to_ty(elem, subst), *size))
                    .into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Tuple(elems) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Tuple(
                    elems
                        .iter()
                        .map(|t| Self::apply_var_subst_to_ty(t, subst))
                        .collect(),
                )
                .into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Closure(closure) => Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Closure(ir_type::ClosureType {
                    param_tys: closure
                        .param_tys
                        .iter()
                        .map(|t| Self::apply_var_subst_to_ty(t, subst))
                        .collect(),
                    ret_ty: Self::apply_var_subst_to_ty(&closure.ret_ty, subst),
                    captures: closure
                        .captures
                        .iter()
                        .map(|t| Self::apply_var_subst_to_ty(t, subst))
                        .collect(),
                })
                .into(),
                mutability: ty.mutability,
            }),
            ir_type::TyKind::Fundamental(_)
            | ir_type::TyKind::UserDefined(_)
            | ir_type::TyKind::Unit
            | ir_type::TyKind::Never => ty.clone(),
        }
    }

    fn apply_var_subst_to_fn_decl(
        decl: &mut ir::top::FnDecl,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        for param in &mut decl.params {
            param.ty = Self::apply_var_subst_to_ty(&param.ty, subst);
            if let Some(default) = &mut param.default {
                Self::apply_var_subst_to_expr(Rc::make_mut(default), subst);
            }
        }
        decl.return_ty = Self::apply_var_subst_to_ty(&decl.return_ty, subst);
    }

    fn apply_var_subst_to_struct(
        struct_: &mut ir::top::Struct,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        for field in &mut struct_.fields {
            field.ty = Self::apply_var_subst_to_ty(&field.ty, subst);
        }
    }

    fn apply_var_subst_to_enum(
        enum_: &mut ir::top::Enum,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        for variant in &mut enum_.variants {
            if let Some(ty) = &variant.ty {
                variant.ty = Some(Self::apply_var_subst_to_ty(ty, subst));
            }
        }
    }

    fn apply_var_subst_to_stmt(
        stmt: &mut ir::stmt::Stmt,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        match stmt {
            ir::stmt::Stmt::Expr(expr) => Self::apply_var_subst_to_expr(Rc::make_mut(expr), subst),
            ir::stmt::Stmt::Let(let_stmt) => {
                let_stmt.ty = Self::apply_var_subst_to_ty(&let_stmt.ty, subst);
                if let Some(init) = &mut let_stmt.init {
                    Self::apply_var_subst_to_expr(init, subst);
                }
            }
            ir::stmt::Stmt::TupleUnpack(tuple_unpack) => {
                Self::apply_var_subst_to_expr(&mut tuple_unpack.init, subst);
            }
            ir::stmt::Stmt::Assign(assign) => {
                Self::apply_var_subst_to_expr(&mut assign.assignee, subst);
                Self::apply_var_subst_to_expr(&mut assign.value, subst);
            }
        }
    }

    fn apply_var_subst_to_block(
        block: &mut ir::stmt::Block,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        for stmt in &mut block.body {
            Self::apply_var_subst_to_stmt(stmt, subst);
        }
        if let Some(last_expr) = &mut block.last_expr {
            Self::apply_var_subst_to_expr(last_expr, subst);
        }
    }

    fn apply_var_subst_to_expr(
        expr: &mut ir::expr::Expr,
        subst: &HashMap<ir_type::VarId, Rc<ir_type::Ty>>,
    ) {
        expr.ty = Self::apply_var_subst_to_ty(&expr.ty, subst);

        match &mut expr.kind {
            ir::expr::ExprKind::Variable(var) => {
                var.ty = Self::apply_var_subst_to_ty(&var.ty, subst);
            }
            ir::expr::ExprKind::ArrayLiteral(arr) => {
                for elem in &mut arr.elements {
                    Self::apply_var_subst_to_expr(elem, subst);
                }
            }
            ir::expr::ExprKind::ArrayRepeat(rep) => {
                Self::apply_var_subst_to_expr(&mut rep.value, subst);
            }
            ir::expr::ExprKind::TupleLiteral(tuple) => {
                for elem in &mut tuple.elements {
                    Self::apply_var_subst_to_expr(elem, subst);
                }
            }
            ir::expr::ExprKind::StructLiteral(lit) => {
                for (_, value) in &mut lit.values {
                    Self::apply_var_subst_to_expr(value, subst);
                }
            }
            ir::expr::ExprKind::Binary(bin) => {
                Self::apply_var_subst_to_expr(Rc::make_mut(&mut bin.lhs), subst);
                Self::apply_var_subst_to_expr(Rc::make_mut(&mut bin.rhs), subst);
            }
            ir::expr::ExprKind::Cast(cast) => {
                Self::apply_var_subst_to_expr(&mut cast.operand, subst);
                cast.target_ty = Self::apply_var_subst_to_ty(&cast.target_ty, subst);
            }
            ir::expr::ExprKind::FieldAccess(field) => {
                Self::apply_var_subst_to_expr(&mut field.operand, subst);
            }
            ir::expr::ExprKind::TupleIndexing(tuple_idx) => {
                Self::apply_var_subst_to_expr(Rc::make_mut(&mut tuple_idx.tuple), subst);
                tuple_idx.element_ty = Self::apply_var_subst_to_ty(&tuple_idx.element_ty, subst);
            }
            ir::expr::ExprKind::EnumVariant(enum_var) => {
                if let Some(value) = &mut enum_var.value {
                    Self::apply_var_subst_to_expr(value, subst);
                }
            }
            ir::expr::ExprKind::Indexing(idx) => {
                Self::apply_var_subst_to_expr(Rc::make_mut(&mut idx.operand), subst);
                Self::apply_var_subst_to_expr(&mut idx.index, subst);
            }
            ir::expr::ExprKind::Slicing(slicing) => {
                Self::apply_var_subst_to_expr(Rc::make_mut(&mut slicing.operand), subst);
                Self::apply_var_subst_to_expr(&mut slicing.start, subst);
                Self::apply_var_subst_to_expr(&mut slicing.end, subst);
                slicing.elem_ty = Self::apply_var_subst_to_ty(&slicing.elem_ty, subst);
            }
            ir::expr::ExprKind::LogicalNot(not) => {
                Self::apply_var_subst_to_expr(&mut not.operand, subst);
            }
            ir::expr::ExprKind::BitNot(not) => {
                Self::apply_var_subst_to_expr(&mut not.operand, subst);
            }
            ir::expr::ExprKind::FnCall(call) => {
                for arg in &mut call.args.0 {
                    Self::apply_var_subst_to_expr(arg, subst);
                }
                let mut callee = (*call.callee).clone();
                Self::apply_var_subst_to_fn_decl(&mut callee, subst);
                call.callee = Rc::new(callee);
            }
            ir::expr::ExprKind::GenericFnCall(call) => {
                for arg in &mut call.args.0 {
                    Self::apply_var_subst_to_expr(arg, subst);
                }
                let mut callee = (*call.callee).clone();
                Self::apply_var_subst_to_fn_decl(&mut callee, subst);
                call.callee = Rc::new(callee);
                call.generic_args = call
                    .generic_args
                    .iter()
                    .map(|arg| Self::apply_var_subst_to_ty(arg, subst))
                    .collect();
            }
            ir::expr::ExprKind::Spawn(spawn) => {
                for arg in &mut spawn.args {
                    Self::apply_var_subst_to_expr(arg, subst);
                }
                let mut callee = (*spawn.callee).clone();
                Self::apply_var_subst_to_fn_decl(&mut callee, subst);
                spawn.callee = Rc::new(callee);
                spawn.arg_types = spawn
                    .arg_types
                    .iter()
                    .map(|ty| Self::apply_var_subst_to_ty(ty, subst))
                    .collect();
            }
            ir::expr::ExprKind::FnPointer(fn_ptr) => {
                let mut decl = (*fn_ptr.decl).clone();
                Self::apply_var_subst_to_fn_decl(&mut decl, subst);
                fn_ptr.decl = Rc::new(decl);
            }
            ir::expr::ExprKind::Closure(closure) => {
                for capture in &mut closure.captures {
                    Self::apply_var_subst_to_expr(capture, subst);
                }
                Self::apply_var_subst_to_expr(&mut closure.body, subst);
            }
            ir::expr::ExprKind::Return(ret) => {
                if let Some(value) = ret {
                    Self::apply_var_subst_to_expr(value, subst);
                }
            }
            ir::expr::ExprKind::If(if_expr) => {
                Self::apply_var_subst_to_expr(&mut if_expr.cond, subst);
                Self::apply_var_subst_to_expr(&mut if_expr.then, subst);
                if let Some(else_) = &mut if_expr.else_ {
                    match else_.as_mut() {
                        ir::expr::Else::If(if_) => {
                            let mut wrapped = ir::expr::Expr {
                                kind: ir::expr::ExprKind::If(if_.clone()),
                                ty: if_.then.ty.clone(),
                                span: if_.span,
                            };
                            Self::apply_var_subst_to_expr(&mut wrapped, subst);
                            if let ir::expr::ExprKind::If(updated_if) = wrapped.kind {
                                *if_ = updated_if;
                            } else {
                                unreachable!()
                            }
                        }
                        ir::expr::Else::Block(block) => {
                            Self::apply_var_subst_to_expr(block, subst);
                        }
                    }
                }
                if let Some(enum_unpack) = &mut if_expr.enum_unpack {
                    Self::apply_var_subst_to_expr(Rc::make_mut(&mut enum_unpack.enum_value), subst);
                    enum_unpack.variant_ty =
                        Self::apply_var_subst_to_ty(&enum_unpack.variant_ty, subst);
                }
            }
            ir::expr::ExprKind::Loop(loop_expr) => {
                Self::apply_var_subst_to_block(&mut loop_expr.body, subst);
            }
            ir::expr::ExprKind::Block(block) => {
                Self::apply_var_subst_to_block(block, subst);
            }
            ir::expr::ExprKind::BuiltinFnCall(call) => {
                for arg in &mut call.args.0 {
                    Self::apply_var_subst_to_expr(arg, subst);
                }
            }
            ir::expr::ExprKind::Int(_)
            | ir::expr::ExprKind::StringLiteral(_)
            | ir::expr::ExprKind::ByteStringLiteral(_)
            | ir::expr::ExprKind::ByteLiteral(_)
            | ir::expr::ExprKind::CharLiteral(_)
            | ir::expr::ExprKind::BooleanLiteral(_)
            | ir::expr::ExprKind::Break => {}
        }
    }

    fn apply_substitutions_to_generated_generic_functions(&mut self) {
        if self.generic_fn_substitutions.is_empty() {
            return;
        }

        let global_subst = self
            .generic_fn_substitutions
            .values()
            .flat_map(|m| m.iter())
            .map(|(k, v)| (*k, v.clone()))
            .collect::<HashMap<_, _>>();

        for top_level in &mut self.generated_generics {
            match top_level {
                ir::top::TopLevel::Fn(fn_) => {
                    let fn_ = Rc::get_mut(fn_).expect("generated function must be uniquely owned");
                    if !global_subst.is_empty() {
                        Self::apply_var_subst_to_fn_decl(&mut fn_.decl, &global_subst);
                        if let Some(body) = &mut fn_.body {
                            Self::apply_var_subst_to_block(body, &global_subst);
                        }
                    }
                    if let Some(subst) = self.generic_fn_substitutions.get(&fn_.decl.name) {
                        Self::apply_var_subst_to_fn_decl(&mut fn_.decl, subst);
                        if let Some(body) = &mut fn_.body {
                            Self::apply_var_subst_to_block(body, subst);
                        }
                    }
                }
                ir::top::TopLevel::Struct(struct_) => {
                    if !global_subst.is_empty() {
                        let mut new_struct = (**struct_).clone();
                        Self::apply_var_subst_to_struct(&mut new_struct, &global_subst);
                        *struct_ = Rc::new(new_struct);
                    }
                }
                ir::top::TopLevel::Enum(enum_) => {
                    if !global_subst.is_empty() {
                        let mut new_enum = (**enum_).clone();
                        Self::apply_var_subst_to_enum(&mut new_enum, &global_subst);
                        *enum_ = Rc::new(new_enum);
                    }
                }
                ir::top::TopLevel::Impl(impl_) => {
                    let impl_ = Rc::get_mut(impl_).expect("generated impl must be uniquely owned");
                    for method in &mut impl_.methods {
                        let method = Rc::get_mut(method)
                            .expect("generated impl method must be uniquely owned");
                        if !global_subst.is_empty() {
                            Self::apply_var_subst_to_fn_decl(&mut method.decl, &global_subst);
                            if let Some(body) = &mut method.body {
                                Self::apply_var_subst_to_block(body, &global_subst);
                            }
                        }
                        if let Some(subst) = self.generic_fn_substitutions.get(&method.decl.name) {
                            Self::apply_var_subst_to_fn_decl(&mut method.decl, subst);
                            if let Some(body) = &mut method.body {
                                Self::apply_var_subst_to_block(body, subst);
                            }
                        }
                    }
                }
            }
        }
    }

    fn analyze_prelude(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        let prelude_file_path = FilePath::from(kaede_lib_src_dir().join("prelude.kd"));
        let prelude_source = std::fs::read_to_string(prelude_file_path.path())?;

        let prelude_ast = Parser::new(prelude_source.as_str(), prelude_file_path).run()?;

        // Extend from the front
        for top_level in prelude_ast.top_levels.into_iter() {
            match self.analyze_top_level(top_level)? {
                TopLevelAnalysisResult::TopLevel(top_level) => {
                    top_level_irs.push(top_level);
                }

                TopLevelAnalysisResult::Imported(imported_irs) => {
                    imported_irs.iter().for_each(|top_level| {
                        top_level_irs.push(top_level.clone());
                    });
                }

                TopLevelAnalysisResult::GenericTopLevel => {}
            }
        }

        Ok(())
    }

    pub fn insert_prelude(&mut self, compile_unit: &mut ast::CompileUnit) -> anyhow::Result<()> {
        let prelude_file_path = FilePath::from(kaede_lib_src_dir().join("prelude.kd"));
        let prelude_source = std::fs::read_to_string(prelude_file_path.path())?;

        let prelude_ast = Parser::new(prelude_source.as_str(), prelude_file_path).run()?;

        // Extend from the front
        for top_level in prelude_ast.top_levels.into_iter().rev() {
            compile_unit.top_levels.push_front(top_level);
        }

        Ok(())
    }

    fn import_autoloads(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        // Only import autoloads once to prevent duplicate definitions
        if self.autoloads_imported {
            return Ok(());
        }
        self.autoloads_imported = true;

        let autoload_dir = kaede_autoload_dir();

        if !autoload_dir.exists() {
            panic!("Autoload directory not found!");
        }

        let autoload_libs = std::fs::read_dir(autoload_dir)?
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|e| e == "kd")) // Exclude non-source files
            .collect::<Vec<_>>();

        self.with_no_prelude(|analyzer| {
            for lib in autoload_libs {
                let segments = lib
                    .iter()
                    .map(|s| {
                        ast::top::PathSegment::Segment(Ident::new(
                            s.to_string_lossy().to_string().into(),
                            Span::dummy(),
                        ))
                    })
                    .collect::<Vec<_>>();

                let import_ast = ast::top::TopLevel {
                    kind: ast::top::TopLevelKind::Import(ast::top::Import {
                        module_path: ast::top::Path {
                            segments,
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    }),
                    span: Span::dummy(),
                };

                let import_ir = analyzer.analyze_top_level(import_ast)?;

                if let TopLevelAnalysisResult::Imported(imported_irs) = import_ir {
                    imported_irs.iter().for_each(|top_level| {
                        top_level_irs.push(top_level.clone());
                    });
                } else {
                    unreachable!("{:?}", import_ir);
                }
            }

            Ok(())
        })
    }

    // Analyze argc and argv and convert them into an easy-to-use format
    fn prepare_command_line_arguments(&mut self) -> anyhow::Result<ir::expr::Expr> {
        let argc_ty = Rc::new(ir::ty::make_fundamental_type(
            ir::ty::FundamentalTypeKind::I32,
            ir::ty::Mutability::Not,
        ));

        let argv_ty = Rc::new(ir::ty::Ty::wrap_in_pointer(
            ir::ty::Ty::wrap_in_pointer(
                ir::ty::make_fundamental_type(
                    ir::ty::FundamentalTypeKind::Char,
                    ir::ty::Mutability::Not,
                )
                .into(),
            )
            .into(),
        ));

        let argc_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                name: "argc".to_owned().into(),
                ty: argc_ty.clone(),
                span: Span::dummy(),
            }),
            ty: argc_ty,
            span: Span::dummy(),
        };

        let argv_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                name: "argv".to_owned().into(),
                ty: argv_ty.clone(),
                span: Span::dummy(),
            }),
            ty: argv_ty,
            span: Span::dummy(),
        };

        let prepare_command_line_arguments_symbol = self
            .lookup_symbol("__prepare_command_line_arguments".to_owned().into())
            .unwrap();

        let prepare_command_line_arguments_decl =
            match &prepare_command_line_arguments_symbol.borrow().kind {
                SymbolTableValueKind::Function(fn_decl) => fn_decl.clone(),
                _ => unreachable!(),
            };

        let fn_call = ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: prepare_command_line_arguments_decl.clone(),
                args: ir::expr::Args(vec![argc_expr, argv_expr], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: prepare_command_line_arguments_decl.return_ty.clone(),
            span: Span::dummy(),
        };

        Ok(fn_call)
    }

    fn build_main_function(
        &mut self,
        top_level_irs: &mut Vec<ir::top::TopLevel>,
    ) -> anyhow::Result<()> {
        let params = vec![
            ir::top::Param {
                name: "argc".to_owned().into(),
                ty: ir::ty::make_fundamental_type(
                    ir::ty::FundamentalTypeKind::I32,
                    ir::ty::Mutability::Not,
                )
                .into(),
                default: None,
            },
            ir::top::Param {
                name: "argv".to_owned().into(),
                ty: ir::ty::Ty::wrap_in_pointer(
                    ir::ty::Ty::wrap_in_pointer(
                        ir::ty::make_fundamental_type(
                            ir::ty::FundamentalTypeKind::Char,
                            ir::ty::Mutability::Not,
                        )
                        .into(),
                    )
                    .into(),
                )
                .into(),
                default: None,
            },
        ];

        let main_fn_decl = ir::top::FnDecl {
            lang_linkage: kaede_common::LangLinkage::Default,
            link_once: false,
            name: QualifiedSymbol::new(ModulePath::new(vec![]), "main".to_owned().into()),
            params,
            is_c_variadic: false,
            return_ty: Rc::new(ir::ty::make_fundamental_type(
                ir::ty::FundamentalTypeKind::I32,
                ir::ty::Mutability::Not,
            )),
        };

        let runtime_init_decl = ir::top::FnDecl {
            lang_linkage: kaede_common::LangLinkage::C,
            link_once: false,
            name: QualifiedSymbol::new(
                ModulePath::new(vec![]),
                "kaede_runtime_init".to_owned().into(),
            ),
            params: vec![],
            is_c_variadic: false,
            return_ty: Rc::new(ir::ty::Ty::new_unit()),
        };

        let runtime_run_decl = ir::top::FnDecl {
            lang_linkage: kaede_common::LangLinkage::C,
            link_once: false,
            name: QualifiedSymbol::new(
                ModulePath::new(vec![]),
                "kaede_runtime_run".to_owned().into(),
            ),
            params: vec![],
            is_c_variadic: false,
            return_ty: Rc::new(ir::ty::make_fundamental_type(
                ir::ty::FundamentalTypeKind::I32,
                ir::ty::Mutability::Not,
            )),
        };

        let runtime_shutdown_decl = ir::top::FnDecl {
            lang_linkage: kaede_common::LangLinkage::C,
            link_once: false,
            name: QualifiedSymbol::new(
                ModulePath::new(vec![]),
                "kaede_runtime_shutdown".to_owned().into(),
            ),
            params: vec![],
            is_c_variadic: false,
            return_ty: Rc::new(ir::ty::Ty::new_unit()),
        };

        top_level_irs.push(ir::top::TopLevel::Fn(Rc::new(ir::top::Fn {
            decl: runtime_init_decl.clone(),
            body: None,
        })));
        top_level_irs.push(ir::top::TopLevel::Fn(Rc::new(ir::top::Fn {
            decl: runtime_run_decl.clone(),
            body: None,
        })));
        top_level_irs.push(ir::top::TopLevel::Fn(Rc::new(ir::top::Fn {
            decl: runtime_shutdown_decl.clone(),
            body: None,
        })));

        let kdmain_symbol =
            self.lookup_symbol("main".to_owned().into())
                .ok_or(SemanticError::Undeclared {
                    name: "main".to_owned().into(),
                    span: Span::dummy(),
                })?;

        let kdmain_decl = match &kdmain_symbol.borrow().kind {
            SymbolTableValueKind::Function(fn_decl) => fn_decl.clone(),
            _ => unreachable!(),
        };

        let mut body = Vec::new();

        let runtime_init_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: runtime_init_decl.clone().into(),
                args: ir::expr::Args(vec![], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: Rc::new(ir::ty::Ty::new_unit()),
            span: Span::dummy(),
        };
        body.push(ir::stmt::Stmt::Expr(Rc::new(runtime_init_expr)));

        let (spawn_args, spawn_arg_types, args_let) = if kdmain_decl.params.len() == 1 {
            let args_expr = self.prepare_command_line_arguments()?;
            let args_symbol = self.fresh_temp_symbol("__kdmain_args");

            let args_let = ir::stmt::Stmt::Let(ir::stmt::Let {
                name: args_symbol,
                init: Some(args_expr.clone()),
                ty: args_expr.ty.clone(),
                span: Span::dummy(),
            });

            let args_var = ir::expr::Expr {
                kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                    name: args_symbol,
                    ty: args_expr.ty.clone(),
                    span: Span::dummy(),
                }),
                ty: args_expr.ty.clone(),
                span: Span::dummy(),
            };

            (vec![args_var], vec![args_expr.ty], Some(args_let))
        } else {
            (vec![], vec![], None)
        };

        if let Some(args_let) = args_let {
            body.push(args_let);
        }

        let spawn_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::Spawn(ir::expr::Spawn {
                callee: kdmain_decl.clone(),
                args: spawn_args,
                arg_types: spawn_arg_types,
                span: Span::dummy(),
                is_main: true,
            }),
            ty: Rc::new(ir::ty::Ty::new_unit()),
            span: Span::dummy(),
        };
        body.push(ir::stmt::Stmt::Expr(Rc::new(spawn_expr)));

        let exit_code_symbol = self.fresh_temp_symbol("__kaede_exit_code");
        let exit_code_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: runtime_run_decl.clone().into(),
                args: ir::expr::Args(vec![], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: runtime_run_decl.return_ty.clone(),
            span: Span::dummy(),
        };

        body.push(ir::stmt::Stmt::Let(ir::stmt::Let {
            name: exit_code_symbol,
            init: Some(exit_code_expr),
            ty: runtime_run_decl.return_ty.clone(),
            span: Span::dummy(),
        }));

        let runtime_shutdown_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: runtime_shutdown_decl.clone().into(),
                args: ir::expr::Args(vec![], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: runtime_shutdown_decl.return_ty.clone(),
            span: Span::dummy(),
        };
        body.push(ir::stmt::Stmt::Expr(Rc::new(runtime_shutdown_expr)));

        let exit_code_var = ir::expr::Expr {
            kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                name: exit_code_symbol,
                ty: runtime_run_decl.return_ty.clone(),
                span: Span::dummy(),
            }),
            ty: runtime_run_decl.return_ty.clone(),
            span: Span::dummy(),
        };

        let return_statement = ir::expr::ExprKind::Return(Some(Box::new(exit_code_var)));

        let main_fn = ir::top::TopLevel::Fn(Rc::new(ir::top::Fn {
            decl: main_fn_decl,
            body: Some(ir::stmt::Block {
                body,
                last_expr: Some(Box::new(ir::expr::Expr {
                    kind: return_statement,
                    ty: ir::ty::make_fundamental_type(
                        ir::ty::FundamentalTypeKind::I32,
                        ir::ty::Mutability::Not,
                    )
                    .into(),
                    span: Span::dummy(),
                })),
                span: Span::dummy(),
            }),
        }));

        top_level_irs.push(main_fn);

        Ok(())
    }

    /// Run type inference on a function body while scopes are still active
    /// This should be called during semantic analysis, not after
    pub fn infer_function_body_inline(
        &mut self,
        body: &mut kaede_ir::stmt::Block,
        decl: &ir::top::FnDecl,
    ) -> anyhow::Result<()> {
        use kaede_type_infer::TypeInferrer;

        // Get all symbol tables currently in scope
        let module_path = self.current_module_path().clone();
        let module = self.modules.get(&module_path).unwrap();

        // Merge all symbol tables from the stack (includes root scope + all local scopes)
        // This allows type inference to see all symbols: globals, function params, and locals
        let symbol_table_view = ScopedSymbolTable::merge_for_inference(module.get_symbol_tables());

        // Create a type inferrer with the merged symbol table
        let mut inferrer = TypeInferrer::new(
            symbol_table_view,
            decl.return_ty.clone(),
            self.infer_context.type_var_allocator.clone(),
        );

        // Infer types for all statements in the block
        for stmt in &body.body {
            inferrer.infer_stmt(stmt)?;
        }

        // Infer the last expression if present
        if let Some(last_expr) = &body.last_expr {
            inferrer.infer_expr(last_expr)?;
        }

        // Apply inferred types back to the IR
        inferrer.apply_block(body)?;
        self.merge_generic_fn_substitutions(inferrer.into_generic_fn_substitutions());

        Ok(())
    }

    fn fn_decl_has_unresolved_types(decl: &ir::top::FnDecl) -> bool {
        decl.params.iter().any(|p| ir::ty::contains_type_var(&p.ty))
            || ir::ty::contains_type_var(&decl.return_ty)
    }

    fn infer_function_body_with_decl(
        &mut self,
        body: &mut kaede_ir::stmt::Block,
        decl: &ir::top::FnDecl,
    ) -> anyhow::Result<()> {
        use kaede_type_infer::TypeInferrer;

        let tables = vec![SymbolTable::new()];
        let symbol_table_view = ScopedSymbolTable::merge_for_inference(&tables);

        let mut inferrer = TypeInferrer::new(
            symbol_table_view,
            decl.return_ty.clone(),
            self.infer_context.type_var_allocator.clone(),
        );

        for param in &decl.params {
            inferrer.register_variable(param.name, param.ty.clone());
        }

        for stmt in &body.body {
            inferrer.infer_stmt(stmt)?;
        }

        if let Some(last_expr) = &body.last_expr {
            inferrer.infer_expr(last_expr)?;
        }

        inferrer.apply_block(body)?;
        self.merge_generic_fn_substitutions(inferrer.into_generic_fn_substitutions());

        Ok(())
    }

    fn infer_generated_generic_bodies_after_substitution(&mut self) -> anyhow::Result<()> {
        for i in 0..self.generated_generics.len() {
            let mut top_level = self.generated_generics[i].clone();

            match &mut top_level {
                ir::top::TopLevel::Fn(fn_) => {
                    let fn_ = Rc::make_mut(fn_);
                    if !Self::fn_decl_has_unresolved_types(&fn_.decl) {
                        if let Some(body) = &mut fn_.body {
                            self.infer_function_body_with_decl(body, &fn_.decl)?;
                        }
                    }
                }
                ir::top::TopLevel::Impl(impl_) => {
                    let impl_ = Rc::make_mut(impl_);
                    for method in &mut impl_.methods {
                        let method = Rc::make_mut(method);
                        if !Self::fn_decl_has_unresolved_types(&method.decl) {
                            if let Some(body) = &mut method.body {
                                self.infer_function_body_with_decl(body, &method.decl)?;
                            }
                        }
                    }
                }
                ir::top::TopLevel::Struct(_) | ir::top::TopLevel::Enum(_) => {}
            }

            self.generated_generics[i] = top_level;
        }

        Ok(())
    }

    pub fn analyze(
        &mut self,
        compile_unit: ast::CompileUnit,
        no_autoload: bool,
        no_prelude: bool,
    ) -> anyhow::Result<ir::CompileUnit> {
        let mut top_level_irs = vec![];

        // Create root module
        let mut root_module = ModuleContext::new(FilePath::dummy());
        root_module.push_scope(SymbolTable::new());
        Self::insert_builtin_slice_symbol(&mut root_module, ModulePath::new(vec![]));
        self.modules.insert(ModulePath::new(vec![]), root_module);

        self.context.set_no_prelude(no_prelude);

        if !no_autoload {
            self.import_autoloads(&mut top_level_irs)?;
        }

        if !no_prelude {
            self.analyze_prelude(&mut top_level_irs)?;
        }

        // Ensure we are working in the current compile unit's module context
        self.context
            .set_current_module_path(self.module_path().clone());

        let (types, others): (Vec<_>, Vec<_>) =
            compile_unit.top_levels.into_iter().partition(|top| {
                matches!(
                    top.kind,
                    ast::top::TopLevelKind::Struct(_)
                        | ast::top::TopLevelKind::Enum(_)
                        | ast::top::TopLevelKind::TypeAlias(_)
                )
            });

        let (imports, others): (Vec<_>, Vec<_>) = others
            .into_iter()
            .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Import(_)));

        let (uses, others): (Vec<_>, Vec<_>) = others
            .into_iter()
            .partition(|top| matches!(top.kind, ast::top::TopLevelKind::Use(_)));

        // Analyze all imports
        for top_level in imports {
            if let TopLevelAnalysisResult::Imported(imported_irs) =
                self.analyze_top_level(top_level)?
            {
                imported_irs.iter().for_each(|top_level| {
                    top_level_irs.push(top_level.clone());
                });
            } else {
                unreachable!()
            }
        }

        // Analyze all use directives
        for top_level in uses {
            if let TopLevelAnalysisResult::Imported(imported_irs) =
                self.analyze_top_level(top_level)?
            {
                if !imported_irs.is_empty() {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }

        // Ensure analysis continues in the current compile unit's module after imports/uses
        self.context
            .set_current_module_path(self.module_path().clone());

        // Declare all types
        // (This is necessary to avoid errors when declaring functions and methods)
        for top_level in types {
            match self.analyze_top_level(top_level)? {
                TopLevelAnalysisResult::TopLevel(top_level) => {
                    top_level_irs.push(top_level);
                }
                TopLevelAnalysisResult::GenericTopLevel => {}
                TopLevelAnalysisResult::Imported(_) => {}
            }
        }

        // Declare all functions and methods
        // (This process removes the need to worry about function declaration order)
        self.with_analyze_command(AnalyzeCommand::OnlyFnDeclare, |analyzer| {
            for top_level in others.iter() {
                match &top_level.kind {
                    ast::top::TopLevelKind::Fn(function) => {
                        analyzer.analyze_fn(function.clone())?;
                    }

                    ast::top::TopLevelKind::Impl(impl_block) => {
                        analyzer.analyze_impl(impl_block.clone())?;
                    }

                    _ => {}
                }
            }
            Ok::<(), anyhow::Error>(())
        })?;

        // Analyze all top levels
        self.with_analyze_command(AnalyzeCommand::WithoutFnDeclare, |analyzer| {
            for top_level in others {
                match analyzer.analyze_top_level(top_level)? {
                    TopLevelAnalysisResult::TopLevel(top_level) => {
                        top_level_irs.push(top_level);
                    }

                    TopLevelAnalysisResult::GenericTopLevel => {}

                    _ => unreachable!(),
                }
            }
            Ok::<(), anyhow::Error>(())
        })?;

        // Add main function
        if self.lookup_symbol("main".to_owned().into()).is_some() {
            self.build_main_function(&mut top_level_irs)?;
        }

        self.apply_substitutions_to_generated_generic_functions();
        self.infer_generated_generic_bodies_after_substitution()?;

        Ok(
            self.inject_generated_generics_to_compile_unit(ir::CompileUnit {
                top_levels: top_level_irs,
            }),
        )
    }
}
