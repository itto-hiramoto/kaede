use std::collections::HashMap;

// ====== 型表現（単相） =======================================================

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Type {
    Int,
    Long,
    Bool,
    Ptr(Box<Type>),
    Fun(Vec<Type>, Box<Type>),
    Var(usize), // 型変数（単相推論用）
}

impl Type {
    fn fun(args: Vec<Type>, ret: Type) -> Self {
        Type::Fun(args, Box::new(ret))
    }
}

// ====== 推論コンテキスト（置換 & 型変数生成） ================================

#[derive(Default)]
struct InferCtx {
    next_var: usize,
    subst: HashMap<usize, Type>, // Var(id) -> Type
}

impl InferCtx {
    fn fresh(&mut self) -> Type {
        let id = self.next_var;
        self.next_var += 1;
        Type::Var(id)
    }

    fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Var(id) => {
                if let Some(tt) = self.subst.get(id) {
                    self.apply(tt)
                } else {
                    Type::Var(*id)
                }
            }
            Type::Ptr(elem) => Type::Ptr(Box::new(self.apply(elem))),
            Type::Fun(args, ret) => {
                let na = args.iter().map(|a| self.apply(a)).collect::<Vec<_>>();
                let nr = self.apply(ret);
                Type::fun(na, nr)
            }
            x => x.clone(),
        }
    }

    fn occurs(&self, var_id: usize, t: &Type) -> bool {
        match self.apply(t) {
            Type::Var(id) => id == var_id,
            Type::Ptr(elem) => self.occurs(var_id, &elem),
            Type::Fun(args, ret) => {
                args.iter().any(|a| self.occurs(var_id, a)) || self.occurs(var_id, &ret)
            }
            _ => false,
        }
    }

    fn unify(&mut self, a: &Type, b: &Type) -> Result<(), String> {
        let a = self.apply(a);
        let b = self.apply(b);
        if a == b {
            return Ok(());
        }
        match (a, b) {
            (Type::Var(id), t) | (t, Type::Var(id)) => {
                if self.occurs(id, &t) {
                    return Err(format!("occurs check failed: α{} occurs in {:?}", id, t));
                }
                self.subst.insert(id, t);
                Ok(())
            }
            (Type::Int, Type::Int) => Ok(()),
            (Type::Long, Type::Long) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Ptr(ae), Type::Ptr(be)) => self.unify(&ae, &be),
            (Type::Fun(a_args, a_ret), Type::Fun(b_args, b_ret)) => {
                if a_args.len() != b_args.len() {
                    return Err("arity mismatch in function types".into());
                }
                for (x, y) in a_args.iter().zip(b_args.iter()) {
                    self.unify(x, y)?;
                }
                self.unify(&a_ret, &b_ret)
            }
            (x, y) => Err(format!("cannot unify {:?} with {:?}", x, y)),
        }
    }
}

// ====== 環境（変数・関数） ====================================================

#[derive(Default)]
struct Env {
    vars: HashMap<String, Type>, // 変数は単相
    funs: HashMap<String, Type>, // 関数シグネチャは既知 (注釈必須)
}

// ====== AST（最小限） =========================================================

#[derive(Clone, Debug)]
enum Expr {
    IntLit(i64),
    BoolLit(bool),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Assign(String, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Clone, Debug)]
enum Stmt {
    AutoLet { name: String, init: Expr },
    ExprStmt(Expr),
}

// ====== 双方向型付け：infer_expr(expected) ===================================

fn infer_expr(ctx: &mut InferCtx, env: &mut Env, e: &Expr, expected: Option<Type>) -> Result<Type, String> {
    use Expr::*;
    match e {
        IntLit(_n) => {
            // 整数リテラルは期待型が long なら long、int なら int、
            // 未指定なら int に既定化。
            match expected.as_ref().map(|t| ctx.apply(t)) {
                Some(Type::Long) => Ok(Type::Long),
                Some(Type::Int) => Ok(Type::Int),
                _ => Ok(Type::Int),
            }
        }
        BoolLit(_) => Ok(Type::Bool),

        Var(name) => {
            let t = env.vars.get(name)
                .ok_or_else(|| format!("unbound variable: {}", name))?
                .clone();
            if let Some(exp) = expected {
                ctx.unify(&t, &exp)?;
            }
            Ok(ctx.apply(&t))
        }

        Add(a, b) => {
            // “整数族の同型”として扱う最小版：
            // 期待型が int/long ならそれに合わせる。なければ int に既定化。
            let want_num = match expected.as_ref().map(|t| ctx.apply(t)) {
                Some(Type::Long) => Type::Long,
                _ => Type::Int,
            };
            let ta = infer_expr(ctx, env, a, Some(want_num.clone()))?;
            let tb = infer_expr(ctx, env, b, Some(want_num.clone()))?;
            ctx.unify(&ta, &want_num)?;
            ctx.unify(&tb, &want_num)?;
            Ok(want_num)
        }

        Assign(name, rhs) => {
            let lt = env.vars.get(name)
                .ok_or_else(|| format!("unbound lvalue: {}", name))?
                .clone();
            let rt = infer_expr(ctx, env, rhs, Some(lt.clone()))?;
            ctx.unify(&lt, &rt)?;
            if let Some(exp) = expected {
                ctx.unify(&lt, &exp)?;
            }
            Ok(ctx.apply(&lt))
        }

        Call(fname, args) => {
            let ft = env.funs.get(fname)
                .ok_or_else(|| format!("unknown function: {}", fname))?
                .clone();
            match ctx.apply(&ft) {
                Type::Fun(fargs, fret) => {
                    if fargs.len() != args.len() {
                        return Err(format!("arity mismatch: {} expects {}, got {}", fname, fargs.len(), args.len()));
                    }
                    for (i, (arg_e, arg_t)) in args.iter().zip(fargs.iter()).enumerate() {
                        let got = infer_expr(ctx, env, arg_e, Some(arg_t.clone()))?;
                        ctx.unify(&got, arg_t).map_err(|e| format!("arg {} to {}: {}", i, fname, e))?;
                    }
                    if let Some(exp) = expected {
                        ctx.unify(&fret, &exp)?;
                    }
                    Ok(ctx.apply(&fret))
                }
                other => Err(format!("{} is not a function type: {:?}", fname, other)),
            }
        }
    }
}

// ====== 文の型付け ============================================================

fn infer_stmt(ctx: &mut InferCtx, env: &mut Env, s: &Stmt) -> Result<(), String> {
    match s {
        Stmt::AutoLet { name, init } => {
            // 変数側に先に新鮮型変数 α を置き、初期化式へ expected として下ろす
            let tv = ctx.fresh();
            env.vars.insert(name.clone(), tv.clone());
            let it = infer_expr(ctx, env, init, Some(tv.clone()))?;
            ctx.unify(&tv, &it)?;
            Ok(())
        }
        Stmt::ExprStmt(e) => {
            let _ = infer_expr(ctx, env, e, None)?;
            Ok(())
        }
    }
}

// ====== デモ：質問のケースを再現 ==============================================

fn main() -> Result<(), String> {
    let mut ctx = InferCtx::default();
    let mut env = Env::default();

    // 関数 f のシグネチャ: (long) -> int
    env.funs.insert("f".into(), Type::fun(vec![Type::Long], Type::Int));

    // プログラム:
    //   auto n = 123;
    //   f(n);
    let prog = vec![
        Stmt::AutoLet { name: "n".into(), init: Expr::IntLit(123) },
        Stmt::ExprStmt(Expr::Call("f".into(), vec![Expr::Var("n".into())])),
    ];

    for s in &prog {
        infer_stmt(&mut ctx, &mut env, s)?;
    }

    // 推論後の n の型を適用して表示
    let n_ty = env.vars.get("n").cloned().unwrap();
    let n_ty = ctx.apply(&n_ty);
    println!("inferred type of n = {:?}", n_ty); // => Long になる

    // 追加で確認：n を使って加算（long を期待して流す）
    let _ = infer_expr(
        &mut ctx,
        &mut env,
        &Expr::Add(Box::new(Expr::Var("n".into())), Box::new(Expr::IntLit(1))),
        Some(Type::Long),
    )?;

    Ok(())
}
