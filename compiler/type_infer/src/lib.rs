//! Bidirectional Type Inference

use std::rc::Rc;

use kaede_span::Span;
use kaede_symbol::Symbol;
use kaede_symbol_table::{ScopedSymbolTable, SymbolTableValueKind};

pub use crate::context::InferContext;
use crate::env::Env;
pub use crate::error::TypeInferError;

use kaede_ir::{
    expr::{
        Binary, BinaryKind, BuiltinFnCall, BuiltinFnCallKind, Cast, EnumVariant, Expr, ExprKind,
        FieldAccess, FnCall, If, Indexing, Int, IntKind, LogicalNot, Loop, TupleIndexing,
    },
    stmt::{Assign, Block, Let, Stmt, TupleUnpack},
    ty::{FundamentalTypeKind, Mutability, Ty, TyKind, make_fundamental_type},
};
mod context;
mod env;
mod error;

pub struct TypeInferrer {
    context: InferContext,
    symbol_table_view: ScopedSymbolTable,
    env: Env,
    function_return_ty: Rc<Ty>,
}

impl TypeInferrer {
    pub fn new(symbol_table_view: ScopedSymbolTable, function_return_ty: Rc<Ty>) -> Self {
        Self {
            context: InferContext::default(),
            symbol_table_view,
            env: Env::new(),
            function_return_ty,
        }
    }

    fn with_new_scope<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<R, TypeInferError>,
    ) -> Result<R, TypeInferError> {
        self.env.push_scope();
        let result = f(self);
        self.env.pop_scope();
        result
    }

    /// Register a variable in the environment (used for function parameters)
    pub fn register_variable(&mut self, name: Symbol, ty: Rc<Ty>) {
        self.env.insert(name, ty);
    }

    /// Dereference a type if it's wrapped in Reference
    /// Tuple, Array, and UserDefined types are typically wrapped in Reference due to GC
    /// Only unwraps one level of Reference
    fn unwrap_reference(ty: &Rc<Ty>) -> Rc<Ty> {
        match ty.kind.as_ref() {
            TyKind::Reference(rty) => rty.refee_ty.clone(),
            _ => ty.clone(),
        }
    }

    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Rc<Ty>, TypeInferError> {
        use ExprKind::*;

        // Get the type assigned during semantic analysis
        let expr_ty = expr.ty.clone();

        // Infer the actual type based on the expression kind
        let inferred_ty = match &expr.kind {
            // Literals
            Int(int_lit) => self.infer_int(int_lit),
            StringLiteral(_) => Ok(Rc::new(Ty::new_str(Mutability::Not))),
            CharLiteral(_) => Ok(Rc::new(make_fundamental_type(
                FundamentalTypeKind::Char,
                Mutability::Not,
            ))),
            BooleanLiteral(_) => Ok(Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            ))),

            // Structured literals
            ArrayLiteral(arr_lit) => self.infer_array_literal(arr_lit),
            TupleLiteral(tuple_lit) => self.infer_tuple_literal(tuple_lit),
            StructLiteral(struct_lit) => self.infer_struct_literal(struct_lit),

            // Variables
            Variable(var) => self.infer_variable(var),

            // Binary operations
            Binary(bin) => self.infer_binary(bin),

            // Unary operations
            LogicalNot(not) => self.infer_logical_not(not),
            Cast(cast) => self.infer_cast(cast),

            // Field and index access
            FieldAccess(field) => self.infer_field_access(field),
            TupleIndexing(tuple_idx) => self.infer_tuple_indexing(tuple_idx),
            Indexing(idx) => self.infer_indexing(idx),

            // Enum
            EnumVariant(enum_var) => self.infer_enum_variant(enum_var),

            // Function calls
            FnCall(fn_call) => self.infer_fn_call(fn_call),
            BuiltinFnCall(builtin_call) => self.infer_builtin_fn_call(builtin_call),
            FnPointer(_) => Ok(expr_ty.clone()),

            // Control flow
            If(if_expr) => self.infer_if(if_expr),
            Loop(loop_expr) => self.infer_loop(loop_expr),
            Block(block) => self.infer_block(block),
            Return(ret) => self.infer_return(ret, expr.span),
            Break => Ok(Rc::new(Ty::new_never())),
            Closure(closure) => {
                for cap in &closure.captures {
                    self.infer_expr(cap)?;
                }

                let body_ty = self.with_new_scope(|this| {
                    if let TyKind::Closure(closure_ty) = expr_ty.kind.as_ref() {
                        for (param, param_ty) in
                            closure.params.iter().zip(closure_ty.param_tys.iter())
                        {
                            this.register_variable(*param, param_ty.clone());
                        }
                    }
                    this.infer_expr(&closure.body)
                })?;

                if let TyKind::Closure(closure_ty) = expr_ty.kind.as_ref() {
                    self.context
                        .unify(&body_ty, &closure_ty.ret_ty, closure.span)?;
                }

                Ok(expr_ty.clone())
            }
        }?;

        // Unify the expression's type (from semantic analysis) with the inferred type
        self.context.unify(&expr_ty, &inferred_ty, expr.span)?;

        // Return the unified type
        Ok(self.context.apply(&expr_ty))
    }

    /// Bidirectional type checking: check an expression against an expected type
    pub fn check_expr(
        &mut self,
        expr: &Expr,
        expected_ty: &Rc<Ty>,
    ) -> Result<Rc<Ty>, TypeInferError> {
        use ExprKind::*;

        if let Closure(closure) = &expr.kind
            && let Some(ty) = self.check_closure(expr, closure, expected_ty)?
        {
            return Ok(ty);
        }

        match &expr.kind {
            // Structured literals benefit from checking
            ArrayLiteral(arr_lit) => self.check_array_literal(arr_lit, expected_ty, expr.span),
            TupleLiteral(tuple_lit) => self.check_tuple_literal(tuple_lit, expected_ty, expr.span),

            // Control flow can use expected type
            If(if_expr) => self.check_if(if_expr, expected_ty),
            Block(block) => self.check_block(block, expected_ty),

            // For other expressions, fall back to inference + unification
            _ => {
                let inferred_ty = self.infer_expr(expr)?;
                self.context.unify(&inferred_ty, expected_ty, expr.span)?;
                Ok(self.context.apply(expected_ty))
            }
        }
    }

    /// Specialized checking for closures to flow expected param/return types into the body
    fn check_closure(
        &mut self,
        expr: &Expr,
        closure: &kaede_ir::expr::Closure,
        expected_ty: &Rc<Ty>,
    ) -> Result<Option<Rc<Ty>>, TypeInferError> {
        let expected_ty = self.context.apply(expected_ty);
        let unwrapped = Self::unwrap_reference(&expected_ty);

        let TyKind::Closure(expected_closure) = unwrapped.kind.as_ref() else {
            // Expected type is not a closure, so we can't check it
            return Ok(None);
        };

        // Resolve captures before checking body so their types participate in inference
        for cap in &closure.captures {
            self.infer_expr(cap)?;
        }

        let closure_ty = match expr.ty.kind.as_ref() {
            TyKind::Closure(ty) => ty,
            _ => unreachable!(),
        };

        self.with_new_scope(|this| {
            // Bind parameter types to the expected closure signature
            for ((param_name, param_ty), expected_param_ty) in closure
                .params
                .iter()
                .zip(closure_ty.param_tys.iter())
                .zip(expected_closure.param_tys.iter())
            {
                // Make sure the parameter type matches the expected signature (fills in type vars)
                this.context
                    .unify(param_ty, expected_param_ty, closure.span)?;
                this.register_variable(*param_name, expected_param_ty.clone());
            }

            // Check body against expected return type
            let body_ty = this.check_expr(&closure.body, &expected_closure.ret_ty)?;
            // Body result must match the expected return type
            this.context
                .unify(&body_ty, &expected_closure.ret_ty, closure.span)?;

            Ok(())
        })?;

        // Unify the closure expression type with the expected type
        self.context.unify(&expr.ty, &expected_ty, expr.span)?;

        // Return the fully applied expected type so callers see concrete types
        Ok(Some(self.context.apply(&expected_ty)))
    }

    pub fn infer_stmt(&mut self, stmt: &Stmt) -> Result<Rc<Ty>, TypeInferError> {
        match stmt {
            Stmt::Expr(expr_rc) => self.infer_expr(expr_rc),
            Stmt::Let(let_stmt) => self.infer_let(let_stmt),
            Stmt::TupleUnpack(tuple_unpack) => self.infer_tuple_unpack(tuple_unpack),
            Stmt::Assign(assign) => self.infer_assign(assign),
        }
    }

    // Literal inference methods
    fn infer_int(&mut self, int_lit: &Int) -> Result<Rc<Ty>, TypeInferError> {
        let ty = match int_lit.kind {
            IntKind::I8(_) => make_fundamental_type(FundamentalTypeKind::I8, Mutability::Not),
            IntKind::U8(_) => make_fundamental_type(FundamentalTypeKind::U8, Mutability::Not),
            IntKind::I32(_) => make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not),
            IntKind::U32(_) => make_fundamental_type(FundamentalTypeKind::U32, Mutability::Not),
            IntKind::I64(_) => make_fundamental_type(FundamentalTypeKind::I64, Mutability::Not),
            IntKind::U64(_) => make_fundamental_type(FundamentalTypeKind::U64, Mutability::Not),
            IntKind::Infer(_) => {
                // Type will be inferred from context
                // The expression already has a type variable assigned during semantic analysis
                // We'll return a fresh type variable here, but it will be unified with the expression's type
                return Ok(self.context.fresh());
            }
        };
        Ok(Rc::new(ty))
    }

    fn infer_array_literal(
        &mut self,
        arr_lit: &kaede_ir::expr::ArrayLiteral,
    ) -> Result<Rc<Ty>, TypeInferError> {
        if arr_lit.elements.is_empty() {
            // Empty array - need more context, create a fresh type variable
            let elem_ty = self.context.fresh();
            return Ok(Rc::new(Ty {
                kind: TyKind::Array((elem_ty, 0)).into(),
                mutability: Mutability::Not,
            }));
        }

        // Infer type from first element
        let first_ty = self.infer_expr(&arr_lit.elements[0])?;

        // Check all elements have the same type
        for elem in &arr_lit.elements[1..] {
            let elem_ty = self.infer_expr(elem)?;
            self.context.unify(&first_ty, &elem_ty, arr_lit.span)?;
        }

        let size = arr_lit.elements.len() as u32;
        Ok(Rc::new(Ty {
            kind: TyKind::Array((first_ty, size)).into(),
            mutability: Mutability::Not,
        }))
    }

    fn check_array_literal(
        &mut self,
        arr_lit: &kaede_ir::expr::ArrayLiteral,
        expected_ty: &Rc<Ty>,
        span: Span,
    ) -> Result<Rc<Ty>, TypeInferError> {
        let expected_ty = self.context.apply(expected_ty);
        let unwrapped = Self::unwrap_reference(&expected_ty);

        match unwrapped.kind.as_ref() {
            TyKind::Array((elem_ty, expected_size)) => {
                let actual_size = arr_lit.elements.len() as u32;

                if *expected_size != actual_size && *expected_size != 0 {
                    return Err(TypeInferError::ArraySizeMismatch {
                        expected: *expected_size as usize,
                        actual: actual_size as usize,
                        span,
                    });
                }

                // Check each element against the expected element type
                for elem in &arr_lit.elements {
                    self.check_expr(elem, elem_ty)?;
                }

                Ok(Rc::new(Ty {
                    kind: TyKind::Array((elem_ty.clone(), actual_size)).into(),
                    mutability: expected_ty.mutability,
                }))
            }
            TyKind::Var(_) => {
                // Type variable - fall back to inference and unify
                let inferred = self.infer_array_literal(arr_lit)?;
                self.context.unify(&inferred, &expected_ty, span)?;
                Ok(self.context.apply(&expected_ty))
            }
            _ => Err(TypeInferError::ExpectedArrayType {
                actual: expected_ty.kind.to_string(),
                span,
            }),
        }
    }

    fn infer_tuple_literal(
        &mut self,
        tuple_lit: &kaede_ir::expr::TupleLiteral,
    ) -> Result<Rc<Ty>, TypeInferError> {
        let elem_tys = tuple_lit
            .elements
            .iter()
            .map(|e| self.infer_expr(e))
            .collect::<Result<Vec<_>, TypeInferError>>()?;

        Ok(Rc::new(Ty {
            kind: TyKind::Tuple(elem_tys).into(),
            mutability: Mutability::Not,
        }))
    }

    fn check_tuple_literal(
        &mut self,
        tuple_lit: &kaede_ir::expr::TupleLiteral,
        expected_ty: &Rc<Ty>,
        span: Span,
    ) -> Result<Rc<Ty>, TypeInferError> {
        let expected_ty = self.context.apply(expected_ty);
        let unwrapped = Self::unwrap_reference(&expected_ty);

        match unwrapped.kind.as_ref() {
            TyKind::Tuple(expected_elem_tys) => {
                if expected_elem_tys.len() != tuple_lit.elements.len() {
                    return Err(TypeInferError::TupleArityMismatch {
                        expected: expected_elem_tys.len(),
                        actual: tuple_lit.elements.len(),
                        span,
                    });
                }

                // Check each element against the corresponding expected type
                let mut checked_tys = Vec::new();
                for (elem, expected_elem_ty) in
                    tuple_lit.elements.iter().zip(expected_elem_tys.iter())
                {
                    let checked_ty = self.check_expr(elem, expected_elem_ty)?;
                    checked_tys.push(checked_ty);
                }

                Ok(Rc::new(Ty {
                    kind: TyKind::Tuple(checked_tys).into(),
                    mutability: expected_ty.mutability,
                }))
            }
            TyKind::Var(_) => {
                // Type variable - fall back to inference and unify
                let inferred = self.infer_tuple_literal(tuple_lit)?;
                self.context.unify(&inferred, &expected_ty, span)?;
                Ok(self.context.apply(&expected_ty))
            }
            _ => Err(TypeInferError::ExpectedTupleType {
                actual: expected_ty.kind.to_string(),
                span,
            }),
        }
    }

    fn infer_struct_literal(
        &mut self,
        struct_lit: &kaede_ir::expr::StructLiteral,
    ) -> Result<Rc<Ty>, TypeInferError> {
        // Infer types for all field values
        for (field_name, field_expr) in &struct_lit.values {
            let field_ty = self.infer_expr(field_expr)?;

            // Find the corresponding field in the struct definition
            if let Some(field_def) = struct_lit
                .struct_info
                .fields
                .iter()
                .find(|f| f.name == *field_name)
            {
                // Unify with the declared field type
                self.context
                    .unify(&field_ty, &field_def.ty, field_expr.span)?;
            }
        }

        Ok(Rc::new(Ty {
            kind: TyKind::UserDefined(kaede_ir::ty::UserDefinedType::new(
                kaede_ir::ty::UserDefinedTypeKind::Struct(struct_lit.struct_info.clone()),
            ))
            .into(),
            mutability: Mutability::Not,
        }))
    }

    fn infer_variable(&mut self, var: &kaede_ir::expr::Variable) -> Result<Rc<Ty>, TypeInferError> {
        // The variable has a type from semantic analysis (var.ty)
        // We need to unify it with the type from the environment/symbol table

        // Look up in local environment first
        if let Some(env_ty) = self.env.lookup(&var.name) {
            // Unify var.ty with env_ty
            self.context.unify(&var.ty, &env_ty, var.span)?;
            return Ok(self.context.apply(&var.ty));
        }

        // Fallback to symbol table
        if let Some(symbol_value) = self.symbol_table_view.lookup(&var.name) {
            let borrowed = symbol_value.borrow();
            if let SymbolTableValueKind::Variable(var_info) = &borrowed.kind {
                // Unify var.ty with var_info.ty
                self.context.unify(&var.ty, &var_info.ty, var.span)?;
                return Ok(self.context.apply(&var.ty));
            }
        }

        Err(TypeInferError::UndefinedVariable {
            name: var.name,
            span: var.span,
        })
    }

    // Binary operations
    fn infer_binary(&mut self, bin: &Binary) -> Result<Rc<Ty>, TypeInferError> {
        let lhs_ty = self.infer_expr(&bin.lhs)?;
        let rhs_ty = self.infer_expr(&bin.rhs)?;

        if matches!(lhs_ty.kind.as_ref(), TyKind::Never) {
            return Ok(rhs_ty);
        }

        if matches!(rhs_ty.kind.as_ref(), TyKind::Never) {
            return Ok(lhs_ty);
        }

        match bin.kind {
            // Arithmetic operations
            BinaryKind::Add
            | BinaryKind::Sub
            | BinaryKind::Mul
            | BinaryKind::Div
            | BinaryKind::Rem => {
                self.context.unify(&lhs_ty, &rhs_ty, bin.span)?;
                Ok(self.context.apply(&lhs_ty))
            }

            // Comparison operations
            BinaryKind::Eq
            | BinaryKind::Ne
            | BinaryKind::Lt
            | BinaryKind::Le
            | BinaryKind::Gt
            | BinaryKind::Ge => {
                self.context.unify(&lhs_ty, &rhs_ty, bin.span)?;
                Ok(Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )))
            }

            // Logical operations
            BinaryKind::LogicalOr | BinaryKind::LogicalAnd => {
                let bool_ty = Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                ));
                self.context.unify(&lhs_ty, &bool_ty, bin.span)?;
                self.context.unify(&rhs_ty, &bool_ty, bin.span)?;
                Ok(bool_ty)
            }
        }
    }

    fn infer_logical_not(&mut self, not: &LogicalNot) -> Result<Rc<Ty>, TypeInferError> {
        let operand_ty = self.infer_expr(&not.operand)?;
        let bool_ty = Rc::new(make_fundamental_type(
            FundamentalTypeKind::Bool,
            Mutability::Not,
        ));
        self.context.unify(&operand_ty, &bool_ty, not.span)?;
        Ok(bool_ty)
    }

    fn infer_cast(&mut self, cast: &Cast) -> Result<Rc<Ty>, TypeInferError> {
        // Infer the operand type
        let operand_ty = self.infer_expr(&cast.operand)?;

        // For integer types, unify operand with target type
        // This allows: let c = 3; let d = c as i8  (c gets inferred as i8)
        let target_ty = &cast.target_ty;

        // Check if target is an integer type
        let target_is_int = matches!(
            target_ty.kind.as_ref(),
            TyKind::Fundamental(fty) if fty.kind.is_int()
        );

        // If operand is a type variable and target is an integer, unify them
        if matches!(operand_ty.kind.as_ref(), TyKind::Var(_)) && target_is_int {
            // Unify operand with target type to propagate type information
            self.context.unify(&operand_ty, target_ty, cast.span)?;
        }

        // Return the target type
        Ok(target_ty.clone())
    }

    // Field and index access
    fn infer_field_access(&mut self, field: &FieldAccess) -> Result<Rc<Ty>, TypeInferError> {
        let struct_ty = self.infer_expr(&field.operand)?;
        let _unwrapped = Self::unwrap_reference(&struct_ty);

        // Find the field in the struct definition
        if let Some(field_def) = field
            .struct_info
            .fields
            .iter()
            .find(|f| f.name == field.field_name)
        {
            Ok(field_def.ty.clone())
        } else {
            Err(TypeInferError::FieldNotFound {
                field_name: field.field_name,
                span: field.span,
            })
        }
    }

    fn infer_tuple_indexing(
        &mut self,
        tuple_idx: &TupleIndexing,
    ) -> Result<Rc<Ty>, TypeInferError> {
        let tuple_ty = self.infer_expr(&tuple_idx.tuple)?;
        let unwrapped = Self::unwrap_reference(&tuple_ty);

        let span = tuple_idx.span;
        let bail_expected_tuple = || TypeInferError::NotATuple {
            index: tuple_idx.index,
            ty: tuple_ty.kind.to_string(),
            unwrapped_ty: unwrapped.kind.to_string(),
            span,
        };

        match unwrapped.kind.as_ref() {
            TyKind::Tuple(elem_tys) => {
                let idx = tuple_idx.index as usize;
                if idx < elem_tys.len() {
                    Ok(elem_tys[idx].clone())
                } else {
                    Err(TypeInferError::TupleIndexOutOfBounds {
                        index: tuple_idx.index as u64,
                        span,
                    })
                }
            }
            TyKind::Fundamental(fty) if matches!(fty.kind, FundamentalTypeKind::Str) => {
                // str is a tuple-like type: (ptr: *i8, len: u64)
                let element_ty = match tuple_idx.index {
                    0 => {
                        // str.0 is *i8
                        Rc::new(Ty::wrap_in_pointer(Rc::new(make_fundamental_type(
                            FundamentalTypeKind::I8,
                            Mutability::Not,
                        ))))
                    }
                    1 => {
                        // str.1 is u64
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::U64,
                            Mutability::Not,
                        ))
                    }
                    _ => {
                        return Err(TypeInferError::StrIndexOutOfBounds { span });
                    }
                };

                // Unify with the element type from semantic analysis
                self.context
                    .unify(&tuple_idx.element_ty, &element_ty, tuple_idx.span)?;
                Ok(element_ty)
            }
            TyKind::UserDefined(_) => {
                // Enums are internally represented as tuples
                // Index 0 is the discriminant (i32), index 1+ is variant data
                match tuple_idx.index {
                    0 => {
                        // Discriminant is always i32
                        Ok(Rc::new(make_fundamental_type(
                            FundamentalTypeKind::I32,
                            Mutability::Not,
                        )))
                    }
                    _ => {
                        // Use the provided element type for variant data
                        Ok(tuple_idx.element_ty.clone())
                    }
                }
            }
            TyKind::Var(_) => {
                // Use the provided element type
                Ok(tuple_idx.element_ty.clone())
            }
            _ => Err(bail_expected_tuple()),
        }
    }

    fn infer_indexing(&mut self, idx: &Indexing) -> Result<Rc<Ty>, TypeInferError> {
        let operand_ty = self.infer_expr(&idx.operand)?;
        let _index_ty = self.infer_expr(&idx.index)?;

        // Index should be an integer (i32 or u64 are both acceptable)
        // We don't enforce a specific integer type here, just check if it's numeric
        // The actual validation will be done in codegen if needed

        let unwrapped = Self::unwrap_reference(&operand_ty);

        match unwrapped.kind.as_ref() {
            TyKind::Array((elem_ty, _)) => Ok(elem_ty.clone()),
            TyKind::Pointer(ptr_ty) => Ok(ptr_ty.pointee_ty.clone()),
            TyKind::Fundamental(fty) if matches!(fty.kind, FundamentalTypeKind::Str) => {
                // str can be indexed to get individual characters
                Ok(Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Char,
                    Mutability::Not,
                )))
            }
            TyKind::Var(_) => {
                // Create a fresh type variable for element type
                let elem_ty = self.context.fresh();
                Ok(elem_ty)
            }
            _ => Err(TypeInferError::NotIndexable { span: idx.span }),
        }
    }

    fn infer_enum_variant(&mut self, enum_var: &EnumVariant) -> Result<Rc<Ty>, TypeInferError> {
        if let Some(value_expr) = &enum_var.value {
            let _value_ty = self.infer_expr(value_expr)?;
        }

        Ok(Rc::new(Ty {
            kind: TyKind::UserDefined(kaede_ir::ty::UserDefinedType::new(
                kaede_ir::ty::UserDefinedTypeKind::Enum(enum_var.enum_info.clone()),
            ))
            .into(),
            mutability: Mutability::Not,
        }))
    }

    // Function calls
    fn infer_fn_call(&mut self, fn_call: &FnCall) -> Result<Rc<Ty>, TypeInferError> {
        let decl = &fn_call.callee;

        // Check argument count
        if fn_call.args.0.len() != decl.params.len() && !decl.is_c_variadic {
            return Err(TypeInferError::ArgumentCountMismatch {
                fn_name: decl.name.clone(),
                expected: decl.params.len(),
                actual: fn_call.args.0.len(),
                span: fn_call.span,
            });
        }

        // Use bidirectional checking: check each argument against parameter type
        for (arg, param) in fn_call.args.0.iter().zip(decl.params.iter()) {
            self.check_expr(arg, &param.ty)?;
        }

        // For variadic functions, infer remaining arguments
        if decl.is_c_variadic && fn_call.args.0.len() > decl.params.len() {
            for arg in &fn_call.args.0[decl.params.len()..] {
                self.infer_expr(arg)?;
            }
        }

        // Return the function's return type
        Ok(decl
            .return_ty
            .clone()
            .unwrap_or_else(|| Rc::new(Ty::new_unit())))
    }

    fn infer_builtin_fn_call(
        &mut self,
        builtin_call: &BuiltinFnCall,
    ) -> Result<Rc<Ty>, TypeInferError> {
        // Infer argument types
        for arg in &builtin_call.args.0 {
            let _arg_ty = self.infer_expr(arg)?;
        }

        match builtin_call.kind {
            BuiltinFnCallKind::Unreachable => Ok(Rc::new(Ty::new_never())),
            BuiltinFnCallKind::Str => Ok(Rc::new(Ty::new_str(Mutability::Not))),
        }
    }

    // Control flow
    fn infer_if(&mut self, if_expr: &If) -> Result<Rc<Ty>, TypeInferError> {
        // Condition must be bool
        let cond_ty = self.infer_expr(&if_expr.cond)?;
        let bool_ty = Rc::new(make_fundamental_type(
            FundamentalTypeKind::Bool,
            Mutability::Not,
        ));
        self.context.unify(&cond_ty, &bool_ty, if_expr.cond.span)?;

        // Infer enum unpack if present
        if let Some(enum_unpack) = &if_expr.enum_unpack {
            let _enum_ty = self.infer_expr(&enum_unpack.enum_value)?;
        }

        // Infer then branch
        let then_ty = self.with_new_scope(|this| {
            if let Some(enum_unpack) = &if_expr.enum_unpack {
                // Register the unpacked variable only within the then-branch scope
                this.env
                    .insert(enum_unpack.name, enum_unpack.variant_ty.clone());
            }
            this.infer_expr(&if_expr.then)
        })?;

        // Infer else branch if present
        if let Some(else_branch) = &if_expr.else_ {
            let else_ty = self.with_new_scope(|this| match else_branch.as_ref() {
                kaede_ir::expr::Else::If(if_expr) => this.infer_if(if_expr),
                kaede_ir::expr::Else::Block(block_expr) => this.infer_expr(block_expr),
            })?;

            if matches!(then_ty.kind.as_ref(), TyKind::Never) {
                return Ok(self.context.apply(&else_ty));
            }

            if matches!(else_ty.kind.as_ref(), TyKind::Never) {
                return Ok(self.context.apply(&then_ty));
            }

            // Both branches should have the same type
            self.context.unify(&then_ty, &else_ty, if_expr.span)?;
            Ok(self.context.apply(&then_ty))
        } else {
            // No else branch
            // For match expressions (is_match = true), the last arm doesn't need an else branch
            // and should return the then branch type (match is exhaustive)
            if if_expr.is_match {
                Ok(then_ty)
            } else {
                // Regular if without else evaluates to unit
                Ok(Rc::new(Ty::new_unit()))
            }
        }
    }

    fn check_if(&mut self, if_expr: &If, expected_ty: &Rc<Ty>) -> Result<Rc<Ty>, TypeInferError> {
        // Condition must be bool
        let cond_ty = self.infer_expr(&if_expr.cond)?;
        let bool_ty = Rc::new(make_fundamental_type(
            FundamentalTypeKind::Bool,
            Mutability::Not,
        ));
        self.context.unify(&cond_ty, &bool_ty, if_expr.cond.span)?;

        // Infer enum unpack if present
        if let Some(enum_unpack) = &if_expr.enum_unpack {
            let _enum_ty = self.infer_expr(&enum_unpack.enum_value)?;
        }

        // Check then branch against expected type
        let then_ty = self.with_new_scope(|this| {
            if let Some(enum_unpack) = &if_expr.enum_unpack {
                this.env
                    .insert(enum_unpack.name, enum_unpack.variant_ty.clone());
            }
            this.check_expr(&if_expr.then, expected_ty)
        })?;

        // Check else branch if present
        if let Some(else_branch) = &if_expr.else_ {
            let else_ty = self.with_new_scope(|this| match else_branch.as_ref() {
                kaede_ir::expr::Else::If(if_expr) => this.check_if(if_expr, expected_ty),
                kaede_ir::expr::Else::Block(block_expr) => this.check_expr(block_expr, expected_ty),
            })?;

            if matches!(then_ty.kind.as_ref(), TyKind::Never) {
                return Ok(self.context.apply(expected_ty));
            }

            if matches!(else_ty.kind.as_ref(), TyKind::Never) {
                return Ok(self.context.apply(expected_ty));
            }

            // Both branches should match expected type
            self.context.unify(&then_ty, &else_ty, if_expr.span)?;
            Ok(self.context.apply(expected_ty))
        } else if if_expr.is_match {
            // Exhaustive match arms are lowered to if-chains without a trailing else.
            // The branches were already checked against `expected_ty`, so reuse it.
            Ok(self.context.apply(expected_ty))
        } else {
            // No else branch - if expression evaluates to unit
            let unit_ty = Rc::new(Ty::new_unit());
            self.context.unify(&unit_ty, expected_ty, if_expr.span)?;
            Ok(unit_ty)
        }
    }

    fn infer_loop(&mut self, loop_expr: &Loop) -> Result<Rc<Ty>, TypeInferError> {
        self.infer_block(&loop_expr.body)?;
        // Loop always returns never type (unless broken out of)
        Ok(Rc::new(Ty::new_never()))
    }

    fn infer_block(&mut self, block: &Block) -> Result<Rc<Ty>, TypeInferError> {
        self.with_new_scope(|this| {
            // Infer all statements
            let mut last_stmt_ty: Option<Rc<Ty>> = None;
            for stmt in &block.body {
                last_stmt_ty = Some(this.infer_stmt(stmt)?);
            }

            // Infer last expression if present
            if let Some(last_expr) = &block.last_expr {
                this.infer_expr(last_expr)
            } else {
                Ok(last_stmt_ty.unwrap_or_else(|| Rc::new(Ty::new_unit())))
            }
        })
    }

    fn check_block(
        &mut self,
        block: &Block,
        expected_ty: &Rc<Ty>,
    ) -> Result<Rc<Ty>, TypeInferError> {
        self.with_new_scope(|this| {
            // Infer all statements (they don't have expected types)
            let mut last_stmt_ty: Option<Rc<Ty>> = None;
            for stmt in &block.body {
                last_stmt_ty = Some(this.infer_stmt(stmt)?);
            }

            // Check last expression against expected type if present
            if let Some(last_expr) = &block.last_expr {
                this.check_expr(last_expr, expected_ty)
            } else if let Some(ty) = last_stmt_ty {
                if !matches!(ty.kind.as_ref(), TyKind::Never) {
                    this.context.unify(&ty, expected_ty, block.span)?;
                }
                Ok(ty)
            } else {
                let unit_ty = Rc::new(Ty::new_unit());
                this.context.unify(&unit_ty, expected_ty, block.span)?;
                Ok(unit_ty)
            }
        })
    }

    fn infer_return(
        &mut self,
        ret: &Option<Box<Expr>>,
        span: Span,
    ) -> Result<Rc<Ty>, TypeInferError> {
        if let Some(expr) = ret {
            let expected_return_ty = self.function_return_ty.clone();
            let ty = self.check_expr(expr, &expected_return_ty)?;
            self.context.unify(&ty, &expected_return_ty, expr.span)?;
        } else {
            // `return;` should only be used in functions that return unit
            self.context
                .unify(&self.function_return_ty, &Rc::new(Ty::new_unit()), span)?;
        }
        Ok(Rc::new(Ty::new_never()))
    }

    // Statement inference methods
    fn infer_let(&mut self, let_stmt: &Let) -> Result<Rc<Ty>, TypeInferError> {
        if let Some(init_expr) = &let_stmt.init {
            // Check if the type is a type variable (needs inference) or concrete (can use checking)
            let init_ty = match let_stmt.ty.kind.as_ref() {
                TyKind::Var(_) => {
                    // Type variable - use inference mode
                    let inferred = self.infer_expr(init_expr)?;
                    self.context
                        .unify(&inferred, &let_stmt.ty, init_expr.span)?;
                    self.context.apply(&let_stmt.ty)
                }
                _ => {
                    // Concrete type - use bidirectional checking mode
                    self.check_expr(init_expr, &let_stmt.ty)?
                }
            };

            // Unify let_stmt.ty with init_ty to ensure they're the same
            self.context.unify(&let_stmt.ty, &init_ty, let_stmt.span)?;

            // Register variable in environment with the final type
            let final_ty = self.context.apply(&let_stmt.ty);
            self.env.insert(let_stmt.name, final_ty.clone());
            Ok(final_ty)
        } else {
            // No initializer - just register the declared type
            self.env.insert(let_stmt.name, let_stmt.ty.clone());
            Ok(let_stmt.ty.clone())
        }
    }

    fn infer_tuple_unpack(&mut self, tuple_unpack: &TupleUnpack) -> Result<Rc<Ty>, TypeInferError> {
        let init_ty = self.infer_expr(&tuple_unpack.init)?;
        let unwrapped = Self::unwrap_reference(&init_ty);

        match unwrapped.kind.as_ref() {
            TyKind::Tuple(elem_tys) => {
                if elem_tys.len() != tuple_unpack.names.len() {
                    return Err(TypeInferError::TupleUnpackCountMismatch {
                        expected: tuple_unpack.names.len(),
                        actual: elem_tys.len(),
                        span: tuple_unpack.span,
                    });
                }

                // Register each variable with its corresponding type
                for (name_opt, elem_ty) in tuple_unpack.names.iter().zip(elem_tys.iter()) {
                    if let Some(name) = name_opt {
                        self.env.insert(*name, elem_ty.clone());
                    }
                }
            }
            TyKind::Var(_) => {
                // Type variable - create fresh type variables for each element
                for name in tuple_unpack.names.iter().flatten() {
                    let elem_ty = self.context.fresh();
                    self.env.insert(*name, elem_ty);
                }
            }
            _ => {
                return Err(TypeInferError::ExpectedTupleForUnpack {
                    span: tuple_unpack.span,
                });
            }
        }

        Ok(Rc::new(Ty::new_unit()))
    }

    fn infer_assign(&mut self, assign: &Assign) -> Result<Rc<Ty>, TypeInferError> {
        let lhs_ty = self.infer_expr(&assign.assignee)?;
        let rhs_ty = self.infer_expr(&assign.value)?;

        // Unify left and right types
        self.context.unify(&lhs_ty, &rhs_ty, assign.span)?;

        // Assignment expressions return unit
        Ok(Rc::new(Ty::new_unit()))
    }

    // ====== Apply inferred types to IR ======

    /// Apply the inferred types to an expression, replacing all type variables
    pub fn apply_expr(&mut self, expr: &mut Expr) -> Result<(), TypeInferError> {
        use ExprKind::*;

        // Apply to this expression's type
        expr.ty = self.context.apply(&expr.ty);

        // If this is an integer literal with an unconstrained type, default to i32
        if matches!(expr.kind, ExprKind::Int(_))
            && let TyKind::Var(id) = expr.ty.kind.as_ref()
        {
            let default_ty = Rc::new(Ty {
                kind: TyKind::Fundamental(kaede_ir::ty::FundamentalType {
                    kind: FundamentalTypeKind::I32,
                })
                .into(),
                mutability: kaede_ir::ty::Mutability::Not,
            });
            self.context.bind_var(*id, default_ty.clone());
            expr.ty = default_ty;
        }

        // Recursively apply to child expressions
        match &mut expr.kind {
            // Integer literals need type conversion based on inferred type
            Int(int) => {
                if let IntKind::Infer(value) = int.kind {
                    int.kind = match expr.ty.kind.as_ref() {
                        TyKind::Fundamental(fty) => match fty.kind {
                            FundamentalTypeKind::I8 => IntKind::I8(value as i8),
                            FundamentalTypeKind::U8 => IntKind::U8(value as u8),
                            FundamentalTypeKind::I32 => IntKind::I32(value as i32),
                            FundamentalTypeKind::U32 => IntKind::U32(value as u32),
                            FundamentalTypeKind::I64 => IntKind::I64(value),
                            FundamentalTypeKind::U64 => IntKind::U64(value as u64),
                            _ => {
                                return Err(TypeInferError::InvalidIntegerLiteralType {
                                    ty: fty.kind.to_string(),
                                    span: expr.span,
                                });
                            }
                        },
                        TyKind::Never => {
                            // Unreachable contexts default integer literals to i32
                            IntKind::I32(value as i32)
                        }
                        _ => {
                            return Err(TypeInferError::InvalidIntegerLiteralType {
                                ty: expr.ty.kind.to_string(),
                                span: expr.span,
                            });
                        }
                    };
                }
            }

            // Other literals have no child expressions
            StringLiteral(_) | CharLiteral(_) | BooleanLiteral(_) | Break | FnPointer(_) => {}

            // Structured literals with child expressions
            ArrayLiteral(arr_lit) => {
                for elem in &mut arr_lit.elements {
                    self.apply_expr(elem)?;
                }
                // Propagate element type into the array if it is still a type variable.
                if let TyKind::Reference(rty) = expr.ty.kind.as_ref()
                    && let TyKind::Array((elem_ty, _)) = rty.get_base_type().kind.as_ref()
                    && let Some(first) = arr_lit.elements.first()
                {
                    let first_ty = self.context.apply(&first.ty);
                    if let TyKind::Var(id) = elem_ty.kind.as_ref() {
                        self.context.bind_var(*id, first_ty);
                    }
                }
            }
            TupleLiteral(tuple_lit) => {
                for elem in &mut tuple_lit.elements {
                    self.apply_expr(elem)?;
                }
            }
            StructLiteral(struct_lit) => {
                for (_field_name, field_expr) in &mut struct_lit.values {
                    self.apply_expr(field_expr)?;
                }
            }

            // Variable has type field that needs updating
            Variable(var) => {
                var.ty = self.context.apply(&var.ty);

                // Error on unresolved type variables
                if matches!(var.ty.kind.as_ref(), TyKind::Var(_)) {
                    return Err(TypeInferError::CannotInferVariableType { span: expr.span });
                }
            }

            // Binary operations
            Binary(bin) => {
                self.apply_expr(std::rc::Rc::make_mut(&mut bin.lhs))?;
                self.apply_expr(std::rc::Rc::make_mut(&mut bin.rhs))?;
            }

            // Unary operations
            LogicalNot(not) => {
                self.apply_expr(&mut not.operand)?;
            }
            Cast(cast) => {
                self.apply_expr(&mut cast.operand)?;
                cast.target_ty = self.context.apply(&cast.target_ty);
            }

            // Field and index access
            FieldAccess(field) => {
                self.apply_expr(&mut field.operand)?;
            }
            TupleIndexing(tuple_idx) => {
                self.apply_expr(std::rc::Rc::make_mut(&mut tuple_idx.tuple))?;
                tuple_idx.element_ty = self.context.apply(&tuple_idx.element_ty);
            }
            Indexing(idx) => {
                self.apply_expr(std::rc::Rc::make_mut(&mut idx.operand))?;
                self.apply_expr(&mut idx.index)?;
                if matches!(expr.ty.kind.as_ref(), TyKind::Var(_)) {
                    let applied_operand_ty = self.context.apply(&idx.operand.ty);
                    let element_ty_opt = match applied_operand_ty.kind.as_ref() {
                        TyKind::Reference(rty) => match rty.get_base_type().kind.as_ref() {
                            TyKind::Array((elem_ty, _)) => Some(elem_ty.clone()),
                            TyKind::Pointer(ptr_ty) => Some(ptr_ty.pointee_ty.clone()),
                            _ => None,
                        },
                        TyKind::Array((elem_ty, _)) => Some(elem_ty.clone()),
                        TyKind::Pointer(ptr_ty) => Some(ptr_ty.pointee_ty.clone()),
                        _ => None,
                    };

                    if let Some(elem_ty) = element_ty_opt {
                        expr.ty = elem_ty;
                    }
                }
            }

            // Enum
            EnumVariant(enum_var) => {
                if let Some(value_expr) = &mut enum_var.value {
                    self.apply_expr(value_expr)?;
                }
            }

            // Function calls
            FnCall(fn_call) => {
                for arg in &mut fn_call.args.0 {
                    self.apply_expr(arg)?;
                }

                let mut applied_callee = (*fn_call.callee).clone();
                for param in applied_callee.params.iter_mut() {
                    param.ty = self.context.apply(&param.ty);
                }
                if let Some(ret) = applied_callee.return_ty.clone() {
                    applied_callee.return_ty = Some(self.context.apply(&ret));
                }
                fn_call.callee = applied_callee.into();
            }
            Closure(closure) => {
                for capture in &mut closure.captures {
                    self.apply_expr(capture)?;
                }
                self.apply_expr(&mut closure.body)?;
            }
            BuiltinFnCall(builtin_call) => {
                for arg in &mut builtin_call.args.0 {
                    self.apply_expr(arg)?;
                }
            }

            // Control flow
            If(if_expr) => {
                self.apply_expr(&mut if_expr.cond)?;
                self.apply_expr(&mut if_expr.then)?;
                if let Some(else_branch) = &mut if_expr.else_ {
                    self.apply_else(else_branch)?;
                }
                if let Some(enum_unpack) = &mut if_expr.enum_unpack {
                    self.apply_expr(std::rc::Rc::make_mut(&mut enum_unpack.enum_value))?;
                    enum_unpack.variant_ty = self.context.apply(&enum_unpack.variant_ty);
                }
            }
            Loop(loop_expr) => {
                self.apply_block(&mut loop_expr.body)?;
            }
            Block(block) => {
                self.apply_block(block)?;
            }
            Return(ret) => {
                if let Some(expr) = ret {
                    self.apply_expr(expr)?;
                }
            }
        }

        // Re-apply after processing children to pick up new substitutions
        expr.ty = self.context.apply(&expr.ty);

        if matches!(expr.ty.kind.as_ref(), TyKind::Var(_)) {
            return Err(TypeInferError::CannotInferType { span: expr.span });
        }

        Ok(())
    }

    /// Apply the inferred types to a statement
    pub fn apply_stmt(&mut self, stmt: &mut Stmt) -> Result<(), TypeInferError> {
        match stmt {
            Stmt::Expr(expr_rc) => {
                let expr = std::rc::Rc::make_mut(expr_rc);
                self.apply_expr(expr)?;
            }
            Stmt::Let(let_stmt) => {
                if let Some(init_expr) = &mut let_stmt.init {
                    self.apply_expr(init_expr)?;
                }
                let_stmt.ty = self.context.apply(&let_stmt.ty);

                // Error on unresolved type variables
                if matches!(let_stmt.ty.kind.as_ref(), TyKind::Var(_)) {
                    return Err(TypeInferError::CannotInferVariableType {
                        span: let_stmt.span,
                    });
                }

                if let Some(symbol) = self.symbol_table_view.lookup(&let_stmt.name)
                    && let SymbolTableValueKind::Variable(var_info) = &mut symbol.borrow_mut().kind
                {
                    var_info.ty = let_stmt.ty.clone();
                }
            }
            Stmt::TupleUnpack(tuple_unpack) => {
                self.apply_expr(&mut tuple_unpack.init)?;
            }
            Stmt::Assign(assign) => {
                self.apply_expr(&mut assign.assignee)?;
                self.apply_expr(&mut assign.value)?;
            }
        }
        Ok(())
    }

    /// Apply the inferred types to an else branch
    fn apply_else(
        &mut self,
        else_branch: &mut Box<kaede_ir::expr::Else>,
    ) -> Result<(), TypeInferError> {
        match else_branch.as_mut() {
            kaede_ir::expr::Else::If(nested_if) => {
                self.apply_expr(&mut nested_if.cond)?;
                self.apply_expr(&mut nested_if.then)?;
                if let Some(nested_else) = &mut nested_if.else_ {
                    self.apply_else(nested_else)?;
                }
                if let Some(enum_unpack) = &mut nested_if.enum_unpack {
                    if let Some(enum_value) = std::rc::Rc::get_mut(&mut enum_unpack.enum_value) {
                        self.apply_expr(enum_value)?;
                    }
                    enum_unpack.variant_ty = self.context.apply(&enum_unpack.variant_ty);
                }
            }
            kaede_ir::expr::Else::Block(block_expr) => {
                self.apply_expr(block_expr)?;
            }
        }
        Ok(())
    }

    /// Apply the inferred types to a block
    pub fn apply_block(&mut self, block: &mut Block) -> Result<(), TypeInferError> {
        for stmt in &mut block.body {
            self.apply_stmt(stmt)?;
        }
        if let Some(last_expr) = &mut block.last_expr {
            self.apply_expr(last_expr)?;
        }
        Ok(())
    }
}
