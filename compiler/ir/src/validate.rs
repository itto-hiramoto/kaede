use std::{collections::HashSet, error::Error, fmt, rc::Rc};

use crate::{
    expr::{Args, BuiltinFnCallKind, Else, Expr, ExprKind, ITable, InterfaceMethodCall, SelectArm},
    stmt::{Block, Stmt},
    top::{Enum, Fn, FnDecl, Interface, InterfaceMethod, Param, Struct, TopLevel},
    ty::{
        GenericInstanceInfo, GenericParamId, InferVarId, Ty, TyKind, UserDefinedType,
        UserDefinedTypeKind,
    },
    CompileUnit,
};

#[derive(Debug)]
pub struct ResolvedCompileUnit(CompileUnit);

impl ResolvedCompileUnit {
    fn new(compile_unit: CompileUnit) -> Self {
        Self(compile_unit)
    }

    pub fn as_compile_unit(&self) -> &CompileUnit {
        &self.0
    }

    pub fn into_compile_unit(self) -> CompileUnit {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationIssue {
    InferType(InferVarId),
    GenericParamType(GenericParamId),
    GenericFnCall,
    UnresolvedFieldAccess,
}

impl fmt::Display for ValidationIssue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InferType(id) => write!(f, "unresolved inference variable ?{id}"),
            Self::GenericParamType(param) => write!(
                f,
                "unresolved generic parameter {}#{}",
                param.origin.symbol(),
                param.index
            ),
            Self::GenericFnCall => write!(f, "unresolved generic function call"),
            Self::UnresolvedFieldAccess => write!(f, "unresolved field access"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
    issues: Vec<ValidationIssue>,
}

impl ValidationError {
    pub fn issues(&self) -> &[ValidationIssue] {
        &self.issues
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "compile unit contains unresolved IR")?;
        for issue in &self.issues {
            write!(f, ": {issue}")?;
        }
        Ok(())
    }
}

impl Error for ValidationError {}

pub fn validate_compile_unit(
    compile_unit: CompileUnit,
) -> Result<ResolvedCompileUnit, ValidationError> {
    let mut validator = Validator::default();
    validator.compile_unit(&compile_unit);

    if validator.issues.is_empty() {
        Ok(ResolvedCompileUnit::new(compile_unit))
    } else {
        Err(ValidationError {
            issues: validator.issues,
        })
    }
}

#[derive(Default)]
struct Validator {
    issues: Vec<ValidationIssue>,
    tys: HashSet<*const Ty>,
    fn_decls: HashSet<*const FnDecl>,
    structs: HashSet<*const Struct>,
    enums: HashSet<*const Enum>,
    interfaces: HashSet<*const Interface>,
}

impl Validator {
    fn compile_unit(&mut self, compile_unit: &CompileUnit) {
        for top_level in &compile_unit.top_levels {
            self.top_level(top_level);
        }
    }

    fn top_level(&mut self, top_level: &TopLevel) {
        match top_level {
            TopLevel::Fn(function) => self.function(function),
            TopLevel::Struct(struct_) => self.struct_(struct_),
            TopLevel::Enum(enum_) => self.enum_(enum_),
            TopLevel::Impl(impl_) => {
                for method in &impl_.methods {
                    self.function(method);
                }
            }
            TopLevel::Interface(interface) => self.interface(interface),
        }
    }

    fn function(&mut self, function: &Fn) {
        self.fn_decl(&function.decl);
        if let Some(body) = &function.body {
            self.block(body);
        }
    }

    fn fn_decl(&mut self, decl: &FnDecl) {
        if !self.fn_decls.insert(decl as *const FnDecl) {
            return;
        }

        for param in &decl.params {
            self.param(param);
        }
        self.ty(&decl.return_ty);
        self.generic_instance(decl.generic_instance.as_ref());
    }

    fn param(&mut self, param: &Param) {
        self.ty(&param.ty);
        if let Some(default) = &param.default {
            self.expr(default);
        }
    }

    fn struct_(&mut self, struct_: &Struct) {
        if !self.structs.insert(struct_ as *const Struct) {
            return;
        }

        self.generic_instance(struct_.generic_instance.as_ref());
        for field in &struct_.fields {
            self.ty(&field.ty);
        }
    }

    fn enum_(&mut self, enum_: &Enum) {
        if !self.enums.insert(enum_ as *const Enum) {
            return;
        }

        self.generic_instance(enum_.generic_instance.as_ref());
        for variant in &enum_.variants {
            if let Some(ty) = &variant.ty {
                self.ty(ty);
            }
        }
    }

    fn interface(&mut self, interface: &Interface) {
        if !self.interfaces.insert(interface as *const Interface) {
            return;
        }

        for method in &interface.methods {
            self.interface_method(method);
        }
    }

    fn interface_method(&mut self, method: &InterfaceMethod) {
        for param in &method.params {
            self.param(param);
        }
        self.ty(&method.return_ty);
    }

    fn generic_instance(&mut self, instance: Option<&GenericInstanceInfo>) {
        if let Some(instance) = instance {
            for arg in &instance.args {
                self.ty(arg);
            }
        }
    }

    fn ty(&mut self, ty: &Rc<Ty>) {
        if !self.tys.insert(Rc::as_ptr(ty)) {
            return;
        }

        match ty.kind.as_ref() {
            TyKind::Infer(id) => self.issues.push(ValidationIssue::InferType(*id)),
            TyKind::GenericParam(param) => self
                .issues
                .push(ValidationIssue::GenericParamType(param.clone())),
            TyKind::Pointer(pointer) => self.ty(&pointer.pointee_ty),
            TyKind::Reference(reference) => self.ty(&reference.refee_ty),
            TyKind::Slice(element) => self.ty(element),
            TyKind::Array((element, _)) => self.ty(element),
            TyKind::Tuple(elements) => {
                for element in elements {
                    self.ty(element);
                }
            }
            TyKind::Closure(closure) => {
                for param in &closure.param_tys {
                    self.ty(param);
                }
                self.ty(&closure.ret_ty);
                for capture in &closure.captures {
                    self.ty(capture);
                }
            }
            TyKind::UserDefined(udt) => self.user_defined_ty(udt),
            TyKind::Fundamental(_) | TyKind::Unit | TyKind::Never => {}
        }
    }

    fn user_defined_ty(&mut self, ty: &UserDefinedType) {
        self.generic_instance(ty.generic_instance.as_ref());
        match &ty.kind {
            UserDefinedTypeKind::Struct(struct_) => self.struct_(struct_),
            UserDefinedTypeKind::Enum(enum_) => self.enum_(enum_),
            UserDefinedTypeKind::Interface(interface) => self.interface(interface),
            UserDefinedTypeKind::Placeholder(_) => {}
        }
    }

    fn block(&mut self, block: &Block) {
        for stmt in &block.body {
            self.stmt(stmt);
        }
        if let Some(last_expr) = &block.last_expr {
            self.expr(last_expr);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Let(let_) => {
                self.ty(&let_.ty);
                if let Some(init) = &let_.init {
                    self.expr(init);
                }
            }
            Stmt::TupleUnpack(unpack) => self.expr(&unpack.init),
            Stmt::Assign(assign) => {
                self.expr(&assign.assignee);
                self.expr(&assign.value);
            }
        }
    }

    fn args(&mut self, args: &Args) {
        for arg in &args.0 {
            self.expr(arg);
        }
    }

    fn expr(&mut self, expr: &Expr) {
        self.ty(&expr.ty);

        match &expr.kind {
            ExprKind::StructLiteral(literal) => {
                self.struct_(&literal.struct_info);
                for (_, value) in &literal.values {
                    self.expr(value);
                }
            }
            ExprKind::ArrayLiteral(array) => {
                for element in &array.elements {
                    self.expr(element);
                }
            }
            ExprKind::ArrayRepeat(array) => self.expr(&array.value),
            ExprKind::TupleLiteral(tuple) => {
                for element in &tuple.elements {
                    self.expr(element);
                }
            }
            ExprKind::Variable(variable) => self.ty(&variable.ty),
            ExprKind::Binary(binary) => {
                self.expr(&binary.lhs);
                self.expr(&binary.rhs);
            }
            ExprKind::Cast(cast) => {
                self.expr(&cast.operand);
                self.ty(&cast.target_ty);
            }
            ExprKind::FieldAccess(access) => {
                self.struct_(&access.struct_info);
                self.expr(&access.operand);
            }
            ExprKind::UnresolvedFieldAccess(access) => {
                self.issues.push(ValidationIssue::UnresolvedFieldAccess);
                self.expr(&access.operand);
            }
            ExprKind::TupleIndexing(indexing) => {
                self.expr(&indexing.tuple);
                self.ty(&indexing.element_ty);
            }
            ExprKind::EnumVariant(variant) => {
                self.enum_(&variant.enum_info);
                if let Some(value) = &variant.value {
                    self.expr(value);
                }
            }
            ExprKind::Indexing(indexing) => {
                self.expr(&indexing.operand);
                self.expr(&indexing.index);
            }
            ExprKind::Slicing(slicing) => {
                self.expr(&slicing.operand);
                self.expr(&slicing.start);
                self.expr(&slicing.end);
                self.ty(&slicing.elem_ty);
            }
            ExprKind::LogicalNot(not) => self.expr(&not.operand),
            ExprKind::BitNot(not) => self.expr(&not.operand),
            ExprKind::FnCall(call) => {
                self.fn_decl(&call.callee);
                self.args(&call.args);
            }
            ExprKind::GenericFnCall(call) => {
                self.issues.push(ValidationIssue::GenericFnCall);
                self.fn_decl(&call.callee);
                for arg in &call.generic_args {
                    self.ty(arg);
                }
                self.args(&call.args);
            }
            ExprKind::Spawn(spawn) => {
                self.fn_decl(&spawn.callee);
                for arg in &spawn.args {
                    self.expr(arg);
                }
                for arg_ty in &spawn.arg_types {
                    self.ty(arg_ty);
                }
            }
            ExprKind::FnPointer(pointer) => self.fn_decl(&pointer.decl),
            ExprKind::Closure(closure) => {
                self.expr(&closure.body);
                for capture in &closure.captures {
                    self.expr(capture);
                }
            }
            ExprKind::Return(value) => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            ExprKind::If(if_) => self.if_expr(if_),
            ExprKind::Loop(loop_) => self.block(&loop_.body),
            ExprKind::Block(block) => self.block(block),
            ExprKind::BuiltinFnCall(call) => {
                if let BuiltinFnCallKind::TypeSize(ty) = &call.kind {
                    self.ty(ty);
                }
                self.args(&call.args);
            }
            ExprKind::InterfaceBox(interface_box) => {
                self.expr(&interface_box.value);
                self.itable(&interface_box.itable);
            }
            ExprKind::InterfaceMethodCall(call) => self.interface_method_call(call),
            ExprKind::Select(select) => {
                for arm in &select.arms {
                    self.select_arm(arm);
                }
                if let Some(default) = &select.default {
                    self.expr(default);
                }
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::StringLiteral(_)
            | ExprKind::ByteStringLiteral(_)
            | ExprKind::ByteLiteral(_)
            | ExprKind::CharLiteral(_)
            | ExprKind::BooleanLiteral(_)
            | ExprKind::Break => {}
        }
    }

    fn if_expr(&mut self, if_: &crate::expr::If) {
        self.expr(&if_.cond);
        self.expr(&if_.then);
        if let Some(else_) = &if_.else_ {
            match else_.as_ref() {
                Else::If(if_) => self.if_expr(if_),
                Else::Block(block) => self.expr(block),
            }
        }
        if let Some(unpack) = &if_.enum_unpack {
            self.user_defined_ty(&unpack.enum_ty);
            self.expr(&unpack.enum_value);
            self.ty(&unpack.variant_ty);
        }
    }

    fn itable(&mut self, itable: &ITable) {
        self.interface(&itable.interface);
        self.ty(&itable.concrete_ty);
        for method in &itable.methods {
            self.fn_decl(method);
        }
    }

    fn interface_method_call(&mut self, call: &InterfaceMethodCall) {
        self.expr(&call.receiver);
        self.interface_method(&call.method);
        self.args(&call.args);
    }

    fn select_arm(&mut self, arm: &SelectArm) {
        self.expr(&arm.channel);
        self.ty(&arm.elem_ty);
        if let Some(value) = &arm.value {
            self.expr(value);
        }
        if let Some(option_ty) = &arm.option_ty {
            self.ty(option_ty);
        }
        if let Some(option_enum) = &arm.option_enum_info {
            self.enum_(option_enum);
        }
        self.expr(&arm.body);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kaede_common::LangLinkage;
    use kaede_span::Span;
    use kaede_symbol::Symbol;

    use crate::{
        expr::{GenericFnCall, UnresolvedFieldAccess, Variable},
        module_path::ModulePath,
        qualified_symbol::QualifiedSymbol,
        top::{Fn, Param, StructField},
        ty::{ClosureType, GenericInstanceInfo, Mutability},
    };

    fn name(name: &str) -> crate::qualified_symbol::QualifiedSymbol {
        QualifiedSymbol::new(ModulePath::root(), Symbol::from(name.to_owned()))
    }

    fn unit_ty() -> Rc<Ty> {
        Rc::new(Ty::new_unit())
    }

    fn infer_ty(id: InferVarId) -> Rc<Ty> {
        Rc::new(Ty {
            kind: TyKind::Infer(id).into(),
            mutability: Mutability::Not,
        })
    }

    fn generic_param_ty(origin: &str, index: usize) -> Rc<Ty> {
        Rc::new(Ty {
            kind: TyKind::GenericParam(GenericParamId::new(name(origin), index)).into(),
            mutability: Mutability::Not,
        })
    }

    fn fn_decl(name_: &str) -> Rc<FnDecl> {
        Rc::new(FnDecl {
            lang_linkage: LangLinkage::Default,
            link_once: false,
            name: name(name_),
            params: vec![],
            is_c_variadic: false,
            return_ty: unit_ty(),
            generic_instance: None,
        })
    }

    fn variable(ty: Rc<Ty>) -> Expr {
        Expr {
            kind: ExprKind::Variable(Variable {
                name: Symbol::from("value".to_owned()),
                ty: ty.clone(),
                span: Span::dummy(),
            }),
            ty,
            span: Span::dummy(),
        }
    }

    #[test]
    fn wraps_a_fully_resolved_compile_unit() {
        let compile_unit = CompileUnit { top_levels: vec![] };

        let resolved = validate_compile_unit(compile_unit).unwrap();

        assert!(resolved.as_compile_unit().top_levels.is_empty());
        assert!(resolved.into_compile_unit().top_levels.is_empty());
    }

    #[test]
    fn reports_unresolved_types_and_nodes_nested_in_declarations() {
        let infer = infer_ty(7);
        let generic_param = generic_param_ty("identity", 0);
        let generic_call = Expr {
            kind: ExprKind::GenericFnCall(GenericFnCall {
                callee: fn_decl("identity"),
                generic_args: vec![infer.clone()],
                args: Args(vec![], Span::dummy()),
                span: Span::dummy(),
            }),
            ty: infer,
            span: Span::dummy(),
        };
        let unresolved_field = Expr {
            kind: ExprKind::UnresolvedFieldAccess(UnresolvedFieldAccess {
                operand: Box::new(generic_call),
                field_name: Symbol::from("field".to_owned()),
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        };
        let function = Fn {
            decl: FnDecl {
                lang_linkage: LangLinkage::Default,
                link_once: false,
                name: name("entry"),
                params: vec![Param {
                    name: Symbol::from("param".to_owned()),
                    ty: generic_param,
                    default: Some(Rc::new(unresolved_field)),
                }],
                is_c_variadic: false,
                return_ty: unit_ty(),
                generic_instance: None,
            },
            body: None,
        };

        let error = validate_compile_unit(CompileUnit {
            top_levels: vec![TopLevel::Fn(Rc::new(function))],
        })
        .unwrap_err();

        assert_eq!(
            error.issues(),
            &[
                ValidationIssue::GenericParamType(GenericParamId::new(name("identity"), 0)),
                ValidationIssue::UnresolvedFieldAccess,
                ValidationIssue::InferType(7),
                ValidationIssue::GenericFnCall,
            ]
        );
    }

    #[test]
    fn walks_nested_types_and_referenced_metadata() {
        let field_infer = infer_ty(11);
        let instance_param = generic_param_ty("Container", 0);
        let struct_ = Rc::new(Struct {
            name: name("Container_i32"),
            fields: vec![StructField {
                name: Symbol::from("callback".to_owned()),
                ty: Rc::new(Ty {
                    kind: TyKind::Closure(ClosureType {
                        param_tys: vec![unit_ty()],
                        ret_ty: unit_ty(),
                        captures: vec![field_infer],
                    })
                    .into(),
                    mutability: Mutability::Not,
                }),
                offset: 0,
            }],
            generic_instance: Some(GenericInstanceInfo::new(
                name("Container"),
                vec![instance_param],
            )),
        });
        let literal = Expr {
            kind: ExprKind::StructLiteral(crate::expr::StructLiteral {
                struct_info: struct_,
                values: vec![(Symbol::from("callback".to_owned()), variable(unit_ty()))],
                span: Span::dummy(),
            }),
            ty: unit_ty(),
            span: Span::dummy(),
        };

        let error = validate_compile_unit(CompileUnit {
            top_levels: vec![TopLevel::Fn(Rc::new(Fn {
                decl: (*fn_decl("entry")).clone(),
                body: Some(Block {
                    body: vec![Stmt::Expr(Rc::new(literal))],
                    last_expr: None,
                    span: Span::dummy(),
                }),
            }))],
        })
        .unwrap_err();

        assert_eq!(
            error.issues(),
            &[
                ValidationIssue::GenericParamType(GenericParamId::new(name("Container"), 0)),
                ValidationIssue::InferType(11),
            ]
        );
    }
}
