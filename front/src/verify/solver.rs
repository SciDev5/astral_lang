use std::collections::{HashMap, HashSet};
use std::iter::once;

use crate::errors::AVerifyError;

use super::super::post::*;
use super::pre::*;

#[derive(Debug, Clone, Copy)]
enum GlobalRefData<TL, TN> {
    Local(TL),
    Nonlocal(TN),
}
impl<TL, TN> GlobalRefData<TL, TN> {
    fn map<RL, RN>(
        self,
        map_local: impl Fn(TL) -> RL,
        map_nonlocal: impl Fn(TN) -> RN,
    ) -> GlobalRefData<RL, RN> {
        match self {
            GlobalRefData::Local(v) => GlobalRefData::Local(map_local(v)),
            GlobalRefData::Nonlocal(v) => GlobalRefData::Nonlocal(map_nonlocal(v)),
        }
    }
    fn consume<R>(self, map_local: impl Fn(TL) -> R, map_nonlocal: impl Fn(TN) -> R) -> R {
        match self {
            GlobalRefData::Local(v) => map_local(v),
            GlobalRefData::Nonlocal(v) => map_nonlocal(v),
        }
    }
}
type GlobalRef<'a, TL, TN> = GlobalRefData<(&'a PreModule, &'a TL), (&'a AModule, &'a TN)>;

fn get_module(module: &PreModule, id: ModuleId) -> GlobalRefData<&PreModule, &AModule> {
    if module.global_id == id {
        GlobalRefData::Local(module)
    } else {
        GlobalRefData::Nonlocal(module.ref_recursive_deps[&id].as_ref())
    }
}
fn get_function(
    module: &PreModule,
    GlobalId { module_id, id }: FunctionId,
) -> GlobalRef<PreFunction, AFunction> {
    get_module(module, module_id).map(|m| (m, &m.functions[id]), |m| (m, &m.functions[id]))
}
fn get_data(
    module: &PreModule,
    GlobalId { module_id, id }: FunctionId,
) -> GlobalRef<PreData, AData> {
    get_module(module, module_id).map(|m| (m, &m.datas[id]), |m| (m, &m.datas[id]))
}
fn get_metatype(
    module: &PreModule,
    GlobalId { module_id, id }: FunctionId,
) -> GlobalRef<PreMetatype, AMetatype> {
    get_module(module, module_id).map(|m| (m, &m.metatypes[id]), |m| (m, &m.metatypes[id]))
}
fn get_metatype_function(
    module: &PreModule,
    GlobalId { module_id, id }: FunctionId,
    function_id: usize,
) -> GlobalRef<PreFunction, AFunction> {
    get_module(module, module_id)
        .map(|m| (m, &m.metatypes[id]), |m| (m, &m.metatypes[id]))
        .map(
            |(m, mt)| (m, &m.functions[mt.fns[function_id]]),
            |(m, mt)| (m, &m.functions[mt.fns[function_id]]),
        )
}
fn get_where(
    module: &PreModule,
    GlobalId { module_id, id }: FunctionId,
) -> GlobalRef<PreWhere, AWhere> {
    get_module(module, module_id).map(|m| (m, &m.wheres[id]), |m| (m, &m.wheres[id]))
}
trait _GlobalRefWhere<'a> {
    fn get_where(self) -> GlobalRef<'a, PreWhere, AWhere>;
    fn get_where_id(self) -> WhereIdGlobal;
}
macro_rules! impl_globalref_getwhere {
    ($l:ty,$n:ty) => {
        impl<'a> _GlobalRefWhere<'a> for GlobalRef<'a, $l, $n> {
            fn get_where(self) -> GlobalRef<'a, PreWhere, AWhere> {
                self.map(
                    |(m, v)| (m, &m.wheres[v.where_id]),
                    |(m, v)| (m, &m.wheres[v.where_id]),
                )
            }
            fn get_where_id(self) -> WhereIdGlobal {
                self.consume(
                    |(m, v)| WhereIdGlobal {
                        module_id: m.global_id,
                        id: v.where_id,
                    },
                    |(m, v)| WhereIdGlobal {
                        module_id: m.global_id,
                        id: v.where_id,
                    },
                )
            }
        }
    };
}
impl_globalref_getwhere!(PreFunction, AFunction);
impl_globalref_getwhere!(PreData, AData);
impl_globalref_getwhere!(PreMetatype, AMetatype);
impl<'a> GlobalRef<'a, PreMetatype, AMetatype> {
    fn get_function(self, i: usize) -> GlobalRef<'a, PreFunction, AFunction> {
        match self {
            GlobalRefData::Local((module, metatype_v)) => {
                GlobalRefData::Local((module, &module.functions[metatype_v.fns[i]]))
            }
            GlobalRefData::Nonlocal((module, metatype_v)) => {
                GlobalRefData::Nonlocal((module, &module.functions[metatype_v.fns[i]]))
            }
        }
    }
    fn n_function(self) -> usize {
        match self {
            GlobalRefData::Local((module, metatype_v)) => metatype_v.fns.len(),
            GlobalRefData::Nonlocal((module, metatype_v)) => metatype_v.fns.len(),
        }
    }
}
impl<'a> GlobalRef<'a, PreWhere, AWhere> {
    fn get_parent(self) -> Option<GlobalRef<'a, PreWhere, AWhere>> {
        Some(match self {
            GlobalRefData::Local((m, v)) => GlobalRefData::Local((m, &m.wheres[v.parent_id?])),
            GlobalRefData::Nonlocal((m, v)) => {
                GlobalRefData::Nonlocal((m, &m.wheres[v.parent_id?]))
            }
        })
    }
    fn count_bindings(self) -> usize {
        (match &self {
            GlobalRefData::Local((_, v)) => v.n_vars,
            GlobalRefData::Nonlocal((_, v)) => v.n_vars,
        }) + self.get_parent().map(|v| v.count_bindings()).unwrap_or(0)
    }
    fn constraints(self) -> Vec<(GlobalId, Vec<GlobalRefData<SymbolId, &'a AType>>)> {
        let mut s = match self {
            GlobalRefData::Local((_, v)) => v
                .constraints
                .iter()
                .map(|(id, v)| {
                    (
                        *id,
                        v.iter()
                            .map(|v| GlobalRefData::Local(*v))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>(),
            GlobalRefData::Nonlocal((_, v)) => v
                .constraints
                .iter()
                .map(|(id, v)| {
                    (
                        *id,
                        v.iter()
                            .map(|v| GlobalRefData::Nonlocal(v))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>(),
        };
        if let Some(r) = self.get_parent() {
            s.extend(r.constraints().into_iter());
        }
        s
    }
}

/// Solve the undefined types in the module.
///
/// Requires all static "seed" declarations to be in place
/// already, such as data field types and metatype function
/// types, but expressions should not yet be typed.
pub fn solve_module(module: PreModule) -> (PreModule, Vec<AVerifyError>) {
    let symbols = module.symbols;
    let mut module = PreModule {
        symbols: Vec::new(),
        ..module
    };
    let mut state = Solver {
        solved_functions: std::iter::repeat_n(false, module.functions.len()).collect(),
        errors: Vec::new(),
        solver_requested_constraints: Vec::new(),
        symbols,
    };
    for i in 0..module.metatypes.len() {
        state.verify_metatype(&module, i);
    }
    for i in 0..module.metatype_impls.len() {
        state.verify_metatypeimpl(&module, i);
    }
    while let Some((i, _)) = state
        .solved_functions
        .iter()
        .enumerate()
        .find(|(_, solved)| !*solved)
    {
        state.solve_function(&mut module, i);
    }

    state.verify_solver_wheres(&module);

    for symbol_id in 0..state.symbols.len() {
        state.simplify_subs(symbol_id);
    }

    (
        PreModule {
            symbols: state.symbols,
            ..module
        },
        state.errors,
    )
}

struct Solver {
    symbols: Vec<Symbol>,
    solved_functions: Vec<bool>,
    solver_requested_constraints: Vec<(MetatypeId, Vec<SymbolId>)>,
    errors: Vec<AVerifyError>,
}

/*
metatype AB(A,B) { ... }
fn x[A,B](x: A): B where AB(A,B) = ...
fn y() {
    let w = x
    // instantiate :: typeof w = x[temp1, temp2]; local where: AB(temp1, temp2)

    let z = w(3)
    // unify[{int}, temp1] :: typeof z = temp2; temp1 -> {int}; local where: AB({int}, temp2)
}
*/
/*
metatype AB(A,B) { ... }
fn y[C](c:C) where AB(C, i32) = {
    fn x[D](c:C):D where AB(C,D) = ...

    let w = x
    // instantiate :: typeof w = x[C, temp1]; local where: AB(C, temp1)

    let z = w(c)
    // unify[C, C]; typeof z = temp1

    let k: i32 = z
    // unify[temp1, i32] :: where AB(C, i32)/SATISFIED

    return w
    // unify[ret, x[C, temp1/i32]] :: ret = x[C, i32]
}
*/

impl Solver {
    fn solve_function(&mut self, module: &mut PreModule, function_id: LocalId) {
        if self.solved_functions[function_id] {
            return;
        } else {
            self.solved_functions[function_id] = true;
        }
        if let Some(PreBody { locals, expr }) = module.functions[function_id].body.take() {
            let body = Some(PreBody {
                expr: self.solve_expr(&module, &module.functions[function_id], &locals, expr),
                locals,
            });
            module.functions[function_id].body = body;
        }
    }
    /*
    - need an error type which indicates it would be impossible to access this data without erroring first.
    - compiled modules have everything inline, can be instantiated into new data.
    - PreModule stores immutable references to its dependencies
     */

    /// Resolves the types of the expressions and intermediate values through repeated
    /// application of instantiate and unify.
    fn solve_expr(
        &mut self,
        module: &PreModule,
        function: &PreFunction,
        locals: &Vec<SymbolId>,
        expr: PreExpr,
    ) -> PreExpr {
        let ret_ty = expr.ret_ty;
        PreExpr {
            ret_ty,
            eval: match self.solve_expr_eval(module, function, locals, ret_ty, expr.eval) {
                Ok(v) => v,
                Err(_) => PreExprEval::Error {},
            },
        }
    }
    fn solve_expr_functioncall_types(
        &mut self,
        module: &PreModule,
        ret_ty: usize,
        arguments: &Vec<PreExpr>,
        fun: GlobalRef<PreFunction, AFunction>,
    ) -> Result<(), ()> {
        let subs = self.instantiate_gen_subs(module, Vec::new(), fun.get_where_id());

        let (fn_ret_ty, fn_args_ty) = match fun {
            GlobalRefData::Local((m, f)) => {
                let fn_ret_ty = self
                    .instantiate_substitute_local(m, f.return_ty, &subs)
                    .map(|sym| self.add_symbol(sym))
                    .unwrap_or(f.return_ty);
                let fn_args_ty = self
                    .instantiate_substitute_local_all(m, f.args_ty.clone(), &subs)
                    .unwrap_or(f.args_ty.clone());
                (fn_ret_ty, fn_args_ty)
            }
            GlobalRefData::Nonlocal((_, f)) => {
                let fn_ret_ty = self.instantiate_substitute_remote(&f.return_ty, &subs);
                let fn_ret_ty = self.add_symbol(fn_ret_ty);
                let fn_args_ty = self.instantiate_substitute_remote_all(&f.args_ty, &subs);
                (fn_ret_ty, fn_args_ty)
            }
        };

        let ret_ok = self.unify(ret_ty, fn_ret_ty);
        if fn_args_ty.len() != arguments.len() {
            return Err(());
        }
        let args_ok = fn_args_ty
            .into_iter()
            .zip(arguments.iter())
            .map(|(fn_arg_ty, arg)| self.unify(arg.ret_ty, fn_arg_ty))
            .collect::<Vec<_>>();
        ret_ok?;
        for arg_ok in args_ok {
            arg_ok?;
        }
        Ok(())
    }
    fn solve_expr_liter(
        &mut self,
        module: &PreModule,
        ret_ty: usize,
        literal: &PreExprLiteral,
    ) -> Result<(), ()> {
        let sym = match literal {
            PreExprLiteral::FunctionRef { function_id } => Symbol::Function {
                function_id: *function_id,
                bindings: std::iter::repeat_with(|| self.add_unbound())
                    .take(
                        get_function(module, *function_id)
                            .get_where()
                            .count_bindings(),
                    )
                    .collect(),
            },
            PreExprLiteral::MetatypeFunctionRef {
                metatype_id,
                function_id,
            } => Symbol::MetatypeFunction {
                metatype_id: *metatype_id,
                function_id: *function_id,
                bindings: std::iter::repeat_with(|| self.add_unbound())
                    .take(
                        get_metatype_function(module, *metatype_id, *function_id)
                            .get_where()
                            .count_bindings(),
                    )
                    .collect(),
            },
            PreExprLiteral::Integer(_) => {
                dbg!("todo: switching between int type variants (currently assuming everything is i32)");
                Symbol::Data {
                    data_id: module.ref_core.d_i32,
                    bindings: Vec::new(),
                }
            }
            PreExprLiteral::Bool(_) => Symbol::Data {
                data_id: module.ref_core.d_bool,
                bindings: Vec::new(),
            },
        };
        let sym = self.add_symbol(sym);
        self.unify(ret_ty, sym)?;
        Ok(())
    }
    fn solve_expr_eval(
        &mut self,
        module: &PreModule,
        function: &PreFunction,
        locals: &Vec<SymbolId>,
        ret_ty: usize,
        expr: PreExprEval,
    ) -> Result<PreExprEval, ()> {
        Ok(match expr {
            PreExprEval::Literal { value } => {
                self.solve_expr_liter(module, ret_ty, &value)?;
                PreExprEval::Literal { value }
            }
            PreExprEval::Call {
                callable,
                arguments,
            } => {
                let callable = self.solve_expr(module, function, locals, *callable);
                match self.resolve(callable.ret_ty) {
                    Some(Symbol::Function { function_id, .. }) => {
                        let function_id = *function_id;
                        self.solve_expr_eval(
                            module,
                            function,
                            locals,
                            ret_ty,
                            PreExprEval::CallFunction {
                                function_id,
                                arguments,
                            },
                        )?
                    }
                    Some(Symbol::MetatypeFunction {
                        metatype_id,
                        function_id,
                        ..
                    }) => {
                        let metatype_id = *metatype_id;
                        let function_id = *function_id;
                        self.solve_expr_eval(
                            module,
                            function,
                            locals,
                            ret_ty,
                            PreExprEval::CallMetatypeFunction {
                                metatype_id,
                                function_id,
                                arguments,
                            },
                        )?
                    }
                    _ if arguments.len() + 1 < module.ref_core.mt_call.len() => self
                        .solve_expr_eval(
                            module,
                            function,
                            locals,
                            ret_ty,
                            PreExprEval::CallMetatypeFunction {
                                metatype_id: module.ref_core.mt_call[arguments.len() + 1],
                                function_id: 0,
                                arguments: once(callable).chain(arguments.into_iter()).collect(),
                            },
                        )?,
                    _ => todo!("error[not callable (too many arguments)]"),
                }
            }
            PreExprEval::CallFunction {
                function_id,
                arguments,
            } => {
                let arguments = arguments
                    .into_iter()
                    .map(|expr| self.solve_expr(module, function, locals, expr))
                    .collect::<Vec<_>>();
                self.solve_expr_functioncall_types(
                    module,
                    ret_ty,
                    &arguments,
                    get_function(module, function_id),
                )?;
                PreExprEval::CallFunction {
                    function_id,
                    arguments,
                }
            }
            PreExprEval::CallMetatypeFunction {
                metatype_id,
                function_id,
                arguments,
            } => {
                let arguments = arguments
                    .into_iter()
                    .map(|expr| self.solve_expr(module, function, locals, expr))
                    .collect::<Vec<_>>();
                self.solve_expr_functioncall_types(
                    module,
                    ret_ty,
                    &arguments,
                    get_metatype_function(module, metatype_id, function_id),
                )?;
                PreExprEval::CallMetatypeFunction {
                    metatype_id,
                    function_id,
                    arguments,
                }
            }
            PreExprEval::Block { inner } => {
                let inner = inner
                    .into_iter()
                    .map(|v| self.solve_expr(module, function, locals, v))
                    .collect::<Vec<_>>();
                let sym_ret = inner.last().map(|v| v.ret_ty).unwrap_or_else(|| {
                    self.add_symbol(Symbol::Data {
                        data_id: module.ref_core.d_void,
                        bindings: Vec::new(),
                    })
                });
                self.unify(ret_ty, sym_ret)?;
                PreExprEval::Block { inner }
            }
            PreExprEval::LocalRef { local_ref_id } => {
                let sym = if local_ref_id < function.args_ty.len() {
                    function.args_ty[local_ref_id]
                } else {
                    locals[local_ref_id - function.args_ty.len()]
                };
                self.unify(ret_ty, sym)?;
                PreExprEval::LocalRef { local_ref_id }
            }
            PreExprEval::Deref { reference } => {
                let reference = self.solve_expr(module, function, locals, *reference);
                let sym = self.add_symbol(Symbol::Reference {
                    inner_data: reference.ret_ty,
                });
                self.unify(ret_ty, sym)?;
                PreExprEval::Deref {
                    reference: Box::new(reference),
                }
            }
            PreExprEval::Assign { receiver, value } => {
                let value = self.solve_expr(module, function, locals, *value);
                self.verify_intermediate_solver_wheres(module);
                let sym = match &receiver {
                    PreExprPattern::Var { local_ref_id } => {
                        if *local_ref_id < function.args_ty.len() {
                            function.args_ty[*local_ref_id]
                        } else {
                            locals[*local_ref_id - function.args_ty.len()]
                        }
                    }
                    _ => todo!("kinda a long impl, come back to this"),
                };
                self.unify(ret_ty, sym)?;
                PreExprEval::Assign {
                    receiver,
                    value: Box::new(value),
                }
            }
            PreExprEval::DataInit { data_id, value } => PreExprEval::DataInit {
                data_id,
                value: match value {
                    PreExprDataInitContent::IntersectionNamed { mut map } => {
                        map.values_mut().for_each(|expr| {
                            let mut expr_tmp = PreExpr {
                                ret_ty: 0,
                                eval: PreExprEval::Block { inner: Vec::new() },
                            };
                            std::mem::swap(&mut expr_tmp, expr);
                            *expr = self.solve_expr(module, function, locals, expr_tmp);
                        });
                        PreExprDataInitContent::IntersectionNamed { map }
                    }
                    PreExprDataInitContent::IntersectionOrdered { mut map } => {
                        map.iter_mut().for_each(|expr| {
                            let mut expr_tmp = PreExpr {
                                ret_ty: 0,
                                eval: PreExprEval::Block { inner: Vec::new() },
                            };
                            std::mem::swap(&mut expr_tmp, expr);
                            *expr = self.solve_expr(module, function, locals, expr_tmp);
                        });
                        PreExprDataInitContent::IntersectionOrdered { map }
                    }
                    PreExprDataInitContent::Union { variant_name, val } => {
                        PreExprDataInitContent::Union {
                            variant_name,
                            val: Box::new(self.solve_expr(module, function, locals, *val)),
                        }
                    }
                },
            },
            PreExprEval::DataAccess { value, field } => {
                let value = self.solve_expr(module, function, locals, *value);
                self.verify_intermediate_solver_wheres(module);
                let sym = match self.resolve(value.ret_ty) {
                    Some(Symbol::Data { data_id, bindings }) => {
                        let data_v = get_data(module, *data_id);
                        let bindings = bindings.clone();
                        let subs =
                            self.instantiate_gen_subs(module, bindings, data_v.get_where_id());
                        match data_v {
                            GlobalRefData::Local((_, data_v)) => match (&data_v.fields, &field) {
                                (
                                    PreDataFields::IntersectionNamed { map },
                                    PreExprField::StructIsh(name),
                                ) => map.iter().find_map(|(name_key, sym)| {
                                    if name_key == name {
                                        Some(sym)
                                    } else {
                                        None
                                    }
                                }),
                                (
                                    PreDataFields::IntersectionOrdered { map },
                                    PreExprField::TupleIsh(index),
                                ) => map.get(*index),
                                _ => None,
                            }
                            .map(|sym| {
                                self.instantiate_substitute_local(module, *sym, &subs)
                                    .map(|sym| self.add_symbol(sym))
                                    .unwrap_or(*sym)
                            }),
                            GlobalRefData::Nonlocal((_, data_v)) => (match (&data_v.fields, &field)
                            {
                                (
                                    ADataFields::IntersectionNamed { fields },
                                    PreExprField::StructIsh(name),
                                ) => fields.iter().find_map(|(name_key, sym)| {
                                    if name_key == name {
                                        Some(sym)
                                    } else {
                                        None
                                    }
                                }),
                                (
                                    ADataFields::IntersectionOrdered { fields },
                                    PreExprField::TupleIsh(index),
                                ) => fields.get(*index),

                                _ => None,
                            })
                            .map(|ty| {
                                let sym = self.instantiate_substitute_remote(ty, &subs);
                                self.add_symbol(sym)
                            }),
                        }
                    }
                    _ => todo!("error[cannot access field of non-data]"),
                };
                match sym {
                    Some(sym) => {
                        self.unify(ret_ty, sym)?;
                        PreExprEval::DataAccess {
                            value: Box::new(value),
                            field,
                        }
                    }
                    None => todo!("error[field does not exist]"),
                }
            }
            PreExprEval::Return { value } => {
                self.unify(ret_ty, value.ret_ty)?;
                PreExprEval::Return {
                    value: Box::new(self.solve_expr(module, function, locals, *value)),
                }
            }
            PreExprEval::Error {} => PreExprEval::Error {},
        })
    }

    fn verify_metatype(&mut self, module: &PreModule, metatype_id: LocalId) {
        /*
        metatypes must have:
            - all types of associated functions predefined
        */
        let metatype_v = &module.metatypes[metatype_id];
        for (fn_i, function_v) in metatype_v
            .fns
            .iter()
            .copied()
            .map(|id| &module.functions[id])
            .enumerate()
        {
            for (arg_i, arg_ty) in function_v.args_ty.iter().enumerate() {
                if !self.is_fully_specified(*arg_ty) {
                    dbg!(
                        "todo: error[metatype function argument not fully specified]",
                        (metatype_id, fn_i, arg_i)
                    );
                }
            }
            if !self.is_fully_specified(function_v.return_ty) {
                dbg!(
                    "todo: error[metatype function return not fully specified]",
                    (metatype_id, fn_i)
                );
            }
        }
    }
    fn verify_metatypeimpl(&mut self, module: &PreModule, impl_id: LocalId) {
        /*
        metatype impls must have:
            - all types and wheres unifiable with metatype definitions
            - function bodies for all function implementations not specified in the metatype
         */

        let impl_v = &module.metatype_impls[impl_id];

        debug_assert!(module.wheres[impl_v.where_id].constraints.len() >= 1);
        let (metatype_id, bindings) = &module.wheres[impl_v.where_id].constraints[0];
        let metatype_v = get_metatype(module, *metatype_id);
        debug_assert_eq!(impl_v.fns.len(), metatype_v.n_function());

        let subs = self.instantiate_gen_subs(module, bindings.clone(), metatype_v.get_where_id());

        for i in 0..impl_v.fns.len() {
            let fn_v = metatype_v.get_function(i);
            let Some(fn_impl_v) = impl_v.fns[i].map(|id| &module.functions[id]) else {
                if fn_v.consume(|(_, v)| v.body.is_none(), |(_, v)| v.body.is_none()) {
                    // if there is no default body, there must be one in the impl block
                    dbg!("todo: error[if there is no default body, there must be one in the impl block]");
                }
                continue;
            };

            let (args_ty, ret_ty) = match fn_v {
                GlobalRefData::Local((m, fn_v)) => (
                    self.instantiate_substitute_local_all(m, fn_v.args_ty.clone(), &subs)
                        .unwrap_or_else(|| fn_v.args_ty.clone()),
                    {
                        self.instantiate_substitute_local(m, fn_v.return_ty, &subs)
                            .map(|sym| self.add_symbol(sym))
                            .unwrap_or(fn_v.return_ty)
                    },
                ),
                GlobalRefData::Nonlocal((m, fn_v)) => (
                    self.instantiate_substitute_remote_all(&fn_v.args_ty, &subs),
                    {
                        let sym = self.instantiate_substitute_remote(&fn_v.return_ty, &subs);
                        self.add_symbol(sym)
                    },
                ),
            };

            if args_ty.len() != fn_impl_v.args_ty.len() {
                dbg!("todo: error[metatype impl function args count mismatched]");
                continue;
            }

            for (arg_a, arg_b) in args_ty.into_iter().zip(fn_impl_v.args_ty.iter().copied()) {
                if self.unify(arg_a, arg_b).is_err() {
                    todo!("error[metatype impl argument unify failed]")
                }
            }
            if self.unify(ret_ty, fn_impl_v.return_ty).is_err() {
                todo!("error[metatype impl return type unify failed]")
            }
        }
    }

    fn verify_intermediate_solver_wheres(&mut self, module: &PreModule) {
        self.verify_solver_wheres(module)
    }
    fn verify_solver_wheres(&mut self, module: &PreModule) {
        let mut req_constraints = Vec::new();
        std::mem::swap(&mut self.solver_requested_constraints, &mut req_constraints);
        req_constraints.retain(|(metatype_id, bindings)| {
            self.verify_solver_where(module, *metatype_id, bindings)
                .is_ok()
        });
    }
    fn verify_solver_where(
        &mut self,
        module: &PreModule,
        metatype_id: GlobalId,
        bindings: &Vec<SymbolId>,
    ) -> Result<(), ()> {
        let local_impls_from_wheres = bindings
            .iter()
            .copied()
            .flat_map(|sym| self.get_locally_bound_wheres(sym))
            .collect::<HashSet<_>>();
        let local_impl_matches = if local_impls_from_wheres.is_empty() {
            // global impl
            dbg!("todo: this is so slow and so awful make it not so slow and not so awful");
            let candidate_impls_remote = module.ref_recursive_deps.values().flat_map(|m| {
                m.metatype_impls.iter().map(|impl_v| GlobalId {
                    module_id: m.global_id,
                    id: impl_v.where_id,
                })
            });
            let candidate_impls_local = module.metatype_impls.iter().map(|v| GlobalId {
                module_id: module.global_id,
                id: v.where_id,
            });
            self.gen_impl_matches(
                module,
                metatype_id,
                bindings,
                candidate_impls_local.chain(candidate_impls_remote),
            )
        } else {
            // local impl
            dbg!("todo: required constraints can be satisfied recursively by the constraints of the implemented metatype");
            self.gen_impl_matches(
                module,
                metatype_id,
                bindings,
                local_impls_from_wheres.into_iter(),
            )
        };
        if local_impl_matches.is_empty() {
            Err(todo!("error[no matching impl found]"))
        } else if local_impl_matches.len() == 1 {
            Ok(())
        } else {
            dbg!("todo: error[too many matching impls found] !! only if is final pass not intermediate");
            Ok(())
        }
    }
    fn gen_impl_matches(
        &mut self,
        module: &PreModule,
        metatype_id: GlobalId,
        bindings: &Vec<SymbolId>,
        impl_wheres: impl Iterator<Item = GlobalId>,
    ) -> Vec<HashMap<SymbolId, SymbolId>> {
        impl_wheres
            .flat_map(|id| get_where(module, id).constraints())
            .filter(|(impl_metatype_id, _)| metatype_id == *impl_metatype_id)
            .filter_map(|(_, impl_bindings)| {
                let impl_bindings = impl_bindings
                    .into_iter()
                    .map(|v| match v {
                        GlobalRefData::Local(v) => v,
                        GlobalRefData::Nonlocal(v) => {
                            let sym = self.instantiate_substitute_remote(v, &HashMap::new());
                            self.add_symbol(sym)
                        }
                    })
                    .collect::<Vec<_>>();
                if !bindings
                    .iter()
                    .chain(impl_bindings.iter())
                    .all(|sym| self.simplify_subs(*sym))
                {
                    todo!("deal with recursive types");
                }
                self.unify_bindings(bindings, &impl_bindings, HashMap::new())
                    .ok()
            })
            .collect()
    }
    // fn is_satisfied_by(&self, ) {}
    fn get_locally_bound_wheres(&self, sym: SymbolId) -> HashSet<WhereIdGlobal> {
        // so inefficient lmao
        match &self.symbols[sym] {
            Symbol::Data { bindings, .. }
            | Symbol::Function { bindings, .. }
            | Symbol::MetatypeFunction { bindings, .. } => bindings
                .iter()
                .copied()
                .flat_map(|sym| self.get_locally_bound_wheres(sym))
                .collect(),
            Symbol::FunctionPointer { args, ret } => args
                .iter()
                .chain(once(ret))
                .copied()
                .flat_map(|sym| self.get_locally_bound_wheres(sym))
                .collect(),
            Symbol::Subs { to: sym } | Symbol::Reference { inner_data: sym } => {
                self.get_locally_bound_wheres(*sym)
            }
            Symbol::Metavar {
                where_id,
                var_id: _,
            } => HashSet::from([*where_id]),
            Symbol::Error {} => HashSet::new(),
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //// HELPERS ////////////////////////////////////////////////////////////////////////////

    /// Generate the substitutions that would be applied when instatiating this type.
    fn instantiate_gen_subs(
        &mut self,
        module: &PreModule,
        bindings: Vec<SymbolId>,
        where_id: WhereIdGlobal,
    ) -> HashMap<(WhereIdGlobal, usize), SymbolId> {
        match get_module(module, where_id.module_id) {
            GlobalRefData::Local(m) => self.instantiate_gen_subs_local(m, bindings, where_id.id),
            GlobalRefData::Nonlocal(m) => {
                self.instantiate_gen_subs_remote(m, bindings, where_id.id)
            }
        }
    }

    /// Generate the substitutions that would be applied when instatiating this type.
    fn instantiate_gen_subs_local(
        &mut self,
        module: &PreModule,
        bindings: Vec<SymbolId>,
        mut where_id: WhereId,
    ) -> HashMap<(WhereIdGlobal, usize), SymbolId> {
        let mut where_ids = Vec::from([where_id]);
        while let Some(next_where_id) = module.wheres[where_id].parent_id {
            where_ids.push(next_where_id);
            where_id = next_where_id;
        }
        where_ids.reverse(); // outermost first

        let map = where_ids
            .iter()
            .copied()
            .flat_map(|where_id| {
                (0..module.wheres[where_id].n_vars).map(move |i| {
                    (
                        GlobalId {
                            module_id: module.global_id,
                            id: where_id,
                        },
                        i,
                    )
                })
            })
            .zip(
                bindings
                    .into_iter()
                    .chain(std::iter::from_fn(|| Some(self.add_unbound()))),
            )
            .collect::<HashMap<_, _>>();

        for where_id in where_ids {
            for (metatype_id, bindings) in module.wheres[where_id].constraints.clone() {
                let bindings = self
                    .instantiate_substitute_local_all(module, bindings.clone(), &map)
                    .unwrap_or(bindings);
                self.solver_requested_constraints
                    .push((metatype_id, bindings));
            }
        }

        map
    }

    /// Generate the substitutions that would be applied when instatiating this type.
    fn instantiate_gen_subs_remote(
        &mut self,
        module: &AModule,
        bindings: Vec<SymbolId>,
        mut where_id: WhereId,
    ) -> HashMap<(WhereIdGlobal, usize), SymbolId> {
        let mut where_ids = Vec::from([where_id]);
        while let Some(next_where_id) = module.wheres[where_id].parent_id {
            where_ids.push(next_where_id);
            where_id = next_where_id;
        }
        where_ids.reverse(); // outermost first

        let map = where_ids
            .iter()
            .copied()
            .flat_map(|where_id| {
                (0..module.wheres[where_id].n_vars).map(move |i| {
                    (
                        GlobalId {
                            module_id: module.global_id,
                            id: where_id,
                        },
                        i,
                    )
                })
            })
            .zip(
                bindings
                    .into_iter()
                    .chain(std::iter::from_fn(|| Some(self.add_unbound()))),
            )
            .collect::<HashMap<_, _>>();

        for where_id in where_ids {
            for (metatype_id, bindings) in &module.wheres[where_id].constraints {
                let bindings = self.instantiate_substitute_remote_all(bindings, &map);
                self.solver_requested_constraints
                    .push((*metatype_id, bindings));
            }
        }

        map
    }

    /// Substitute types into the place of fields of generic entities in order
    /// to instantiate them.
    fn instantiate_substitute_remote(
        &mut self,
        ty: &AType,
        map: &HashMap<(WhereIdGlobal, usize), SymbolId>,
    ) -> Symbol {
        match ty {
            AType::Data { data_id, bindings } => Symbol::Data {
                data_id: *data_id,
                bindings: self.instantiate_substitute_remote_all(bindings, map),
            },
            AType::FunctionPointer { args, ret } => Symbol::FunctionPointer {
                args: self.instantiate_substitute_remote_all(args, map),
                ret: {
                    let sym = self.instantiate_substitute_remote(ret, map);
                    self.add_symbol(sym)
                },
            },
            AType::Metavar { where_id, var_id } => map
                .get(&(*where_id, *var_id))
                .map(|sym_id| Symbol::Subs { to: *sym_id })
                .unwrap_or_else(|| self.instantiate_substitute_remote(ty, map)),
            AType::Reference { inner_data } => Symbol::Reference {
                inner_data: {
                    let sym = self.instantiate_substitute_remote(inner_data.as_ref(), map);
                    self.add_symbol(sym)
                },
            },
            AType::Error {} => Symbol::Error {},
        }
    }
    /// Helper for [`Self::instantiate_substitute_remote`] which runs it on each of the
    /// given input symbols. Substitutes for origional symbol id if some are
    /// unchanged, if all unchanged, returns `None`.
    fn instantiate_substitute_remote_all(
        &mut self,
        ty: &Vec<AType>,
        map: &HashMap<(WhereIdGlobal, usize), SymbolId>,
    ) -> Vec<SymbolId> {
        ty.iter()
            .map(|ty| {
                let sym = self.instantiate_substitute_remote(ty, map);
                self.add_symbol(sym)
            })
            .collect()
    }

    /// Substitute types into the place of fields of generic entities in order
    /// to instantiate them.
    ///
    /// Returns `None` if it does not need to be changed.
    fn instantiate_substitute_local(
        &mut self,
        module: &PreModule,
        symbol: SymbolId,
        map: &HashMap<(WhereIdGlobal, usize), SymbolId>,
    ) -> Option<Symbol> {
        /*
        data TheData[A] struct { y: V[A] }
        let x: TheData[T]
        let y = x.y
        // typeof y = instantiate_substitute[A -> T]{ TheData.y[A] } = V[T]
         */

        match &self.symbols[symbol] {
            Symbol::Data { data_id, bindings } => Some(Symbol::Data {
                data_id: *data_id,
                bindings: self.instantiate_substitute_local_all(module, bindings.clone(), map)?,
            }),
            Symbol::Function {
                function_id,
                bindings,
            } => Some(Symbol::Function {
                function_id: *function_id,
                bindings: self.instantiate_substitute_local_all(module, bindings.clone(), map)?,
            }),
            Symbol::MetatypeFunction {
                metatype_id,
                function_id,
                bindings,
            } => Some(Symbol::MetatypeFunction {
                function_id: *function_id,
                metatype_id: *metatype_id,
                bindings: self.instantiate_substitute_local_all(module, bindings.clone(), map)?,
            }),
            Symbol::Metavar { where_id, var_id } => Some(Symbol::Subs {
                to: map.get(&(*where_id, *var_id)).copied()?,
            }),
            Symbol::Reference { inner_data } => Some(Symbol::Reference {
                inner_data: {
                    let sym = self.instantiate_substitute_local(module, *inner_data, map)?;
                    self.add_symbol(sym)
                },
            }),
            Symbol::FunctionPointer { args, ret } => {
                let ret = *ret;
                let args = args.clone();
                Some(Symbol::FunctionPointer {
                    args: self.instantiate_substitute_local_all(module, args, map)?,
                    ret: {
                        let ret = self.instantiate_substitute_local(module, ret, map)?;
                        self.add_symbol(ret)
                    },
                })
            }
            Symbol::Error {} => Some(Symbol::Error {}),
            Symbol::Subs { to } => {
                if *to == symbol {
                    dbg!("not sure if this is correct, reevaluate later");
                    None
                } else {
                    self.instantiate_substitute_local(module, *to, map)
                }
            }
        }
    }
    /// Helper for [`Self::instantiate_substitute_local`] which runs it on each of the
    /// given input symbols. Substitutes for origional symbol id if some are
    /// unchanged, if all unchanged, returns `None`.
    fn instantiate_substitute_local_all(
        &mut self,
        module: &PreModule,
        symbols_in: Vec<SymbolId>,
        map: &HashMap<(WhereIdGlobal, usize), SymbolId>,
    ) -> Option<Vec<SymbolId>> {
        let bindings = symbols_in
            .iter()
            .map(|symbol| self.instantiate_substitute_local(module, *symbol, map))
            .collect::<Vec<_>>();
        if bindings.iter().all(|v| v.is_none()) {
            return None;
        }
        let bindings = (bindings.into_iter().zip(symbols_in.into_iter()))
            .map(|(sym, old_sym)| sym.map(|sym| self.add_symbol(sym)).unwrap_or(old_sym))
            .collect();
        Some(bindings)
    }
    fn add_symbol(&mut self, sym: Symbol) -> SymbolId {
        let id = self.symbols.len();
        self.symbols.push(sym);
        id
    }
    fn add_unbound(&mut self) -> SymbolId {
        let id = self.symbols.len();
        self.symbols.push(Symbol::Subs { to: id });
        id
    }

    /// Flatten substitution symbols to minimize hops, and reduce loops to their most trivial form.
    ///
    /// Returns false if a self-referential concrete data-type is detected.
    fn simplify_subs(&mut self, sym: SymbolId) -> bool {
        fn simplify(symbols: &mut Vec<Symbol>, sym: SymbolId, trace: &mut Vec<SymbolId>) -> bool {
            if trace.contains(&sym) && !matches!(&symbols[sym], Symbol::Subs { .. }) {
                return false;
            }
            trace.push(sym);
            let ok = match &symbols[sym] {
                Symbol::Data { bindings, .. }
                | Symbol::Function { bindings, .. }
                | Symbol::MetatypeFunction { bindings, .. } => bindings
                    .clone()
                    .into_iter()
                    .all(|sym| simplify(symbols, sym, trace)),
                Symbol::Reference { inner_data } => simplify(symbols, *inner_data, trace),
                Symbol::FunctionPointer { args, ret } => args
                    .clone()
                    .into_iter()
                    .chain(once(*ret))
                    .all(|sym| simplify(symbols, sym, trace)),
                Symbol::Metavar { .. } | Symbol::Error {} => true,
                Symbol::Subs { .. } => simplify_flatten_subs(symbols, sym, trace),
            };
            trace.pop();
            ok
        }
        fn simplify_flatten_subs(
            symbols: &mut Vec<Symbol>,
            mut sym: SymbolId,
            trace: &mut Vec<SymbolId>,
        ) -> bool {
            let mut subs_trace = Vec::new();
            let ok = loop {
                match &symbols[sym] {
                    Symbol::Subs { to } => {
                        subs_trace.push(sym);
                        sym = *to;
                        if subs_trace.contains(&to) {
                            // unresolved type, break to point all above to last (including itself,
                            // to create the trivial loop, representing the unknown)
                            break true;
                        }
                    }
                    _ => {
                        break simplify(symbols, sym, trace);
                    }
                }
            };
            for sym_from in subs_trace {
                symbols[sym_from] = Symbol::Subs { to: sym };
            }
            ok
        }
        let mut trace = Vec::new();
        return simplify(&mut self.symbols, sym, &mut trace);
    }

    fn resolve(&mut self, mut sym: SymbolId) -> Option<&Symbol> {
        if !self.simplify_subs(sym) {
            todo!("deal with recursive types");
        }
        Some(loop {
            match &self.symbols[sym] {
                Symbol::Subs { to } => {
                    if sym == *to {
                        return None;
                    } else {
                        sym = *to
                    }
                }
                sym => break sym,
            }
        })
    }

    fn is_fully_specified(&self, sym: SymbolId) -> bool {
        match &self.symbols[sym] {
            Symbol::Data { bindings, .. }
            | Symbol::MetatypeFunction { bindings, .. }
            | Symbol::Function { bindings, .. } => {
                bindings.iter().all(|sym| self.is_fully_specified(*sym))
            }
            Symbol::FunctionPointer { args, ret } => {
                args.iter().all(|sym| self.is_fully_specified(*sym))
                    && self.is_fully_specified(*ret)
            }
            Symbol::Reference { inner_data: sym } | Symbol::Subs { to: sym } => {
                self.is_fully_specified(*sym)
            }
            Symbol::Metavar { .. } | Symbol::Error {} => true,
        }
    }

    fn apply_unify_subs(&mut self, subs: HashMap<SymbolId, SymbolId>) {
        for (from, to) in subs {
            self.symbols[from] = Symbol::Subs { to };
        }
    }

    /// Makes two symbols represent the same type if possible, otherwise returns `Err`.
    fn unify(&mut self, sym_a: SymbolId, sym_b: SymbolId) -> Result<(), ()> {
        if !self.simplify_subs(sym_a) || !self.simplify_subs(sym_b) {
            todo!("deal with recursive types");
        }
        let subs = self
            .unify_immut(sym_a, sym_b, HashMap::new())
            .map_err(|err| {
                self.errors.push(err);
            })?;
        self.apply_unify_subs(subs);
        Ok(())
    }

    fn unify_bindings(
        &self,
        bindings_0: &Vec<SymbolId>,
        bindings_1: &Vec<SymbolId>,
        existing_subs: HashMap<SymbolId, SymbolId>,
    ) -> Result<HashMap<SymbolId, SymbolId>, AVerifyError> {
        assert_eq!(bindings_0.len(), bindings_1.len());
        let bindings_0 = bindings_0.iter().copied();
        let bindings_1 = bindings_1.iter().copied();
        (bindings_0.zip(bindings_1)).fold(Ok(existing_subs), |subs, (binding_0, binding_1)| {
            subs.and_then(|subs| self.unify_immut(binding_0, binding_1, subs))
        })
    }
    /// Makes two symbols represent the same type if possible, otherwise returns
    /// `Err`.
    ///
    /// May only be called after sym_a and sym_b have been passed through [`Self::simplify_subs`].
    fn unify_immut(
        &self,
        sym_a: SymbolId,
        sym_b: SymbolId,
        existing_subs: HashMap<SymbolId, SymbolId>,
    ) -> Result<HashMap<SymbolId, SymbolId>, AVerifyError> {
        /*
        - [done] unify needs to be able to backtrack on contradiction, (returns hashmap of ids to ids for overridden symbols?)
        - [done] want some function to simplify recursive substitution loops into the trivial representation of unbound (subs to self), would need to collect impls
        - [done by default] need some function to move metatype impls to the types they point to
        - [done in final verify step] unify needs to be able to ensure merge metatype impls are valid or can be validated (this can be done at the end after all unification complete)
         */

        match (&self.symbols[sym_a], &self.symbols[sym_b]) {
            (
                Symbol::Data {
                    data_id: data_id_0,
                    bindings: bindings_0,
                },
                Symbol::Data {
                    data_id: data_id_1,
                    bindings: bindings_1,
                },
            ) => {
                if data_id_0 == data_id_1 {
                    self.unify_bindings(bindings_0, bindings_1, existing_subs)
                } else {
                    Err(todo!("error[unify]"))
                }
            }
            (
                Symbol::Function {
                    function_id: function_id_0,
                    bindings: bindings_0,
                },
                Symbol::Function {
                    function_id: function_id_1,
                    bindings: bindings_1,
                },
            ) => {
                if function_id_0 == function_id_1 {
                    self.unify_bindings(bindings_0, bindings_1, existing_subs)
                } else {
                    Err(todo!("error[unify]"))
                }
            }
            (
                Symbol::MetatypeFunction {
                    metatype_id: metatype_id_0,
                    function_id: function_id_0,
                    bindings: bindings_0,
                },
                Symbol::MetatypeFunction {
                    metatype_id: metatype_id_1,
                    function_id: function_id_1,
                    bindings: bindings_1,
                },
            ) => {
                if metatype_id_0 == metatype_id_1 && function_id_0 == function_id_1 {
                    self.unify_bindings(bindings_0, bindings_1, existing_subs)
                } else {
                    Err(todo!("error[unify]"))
                }
            }
            (
                Symbol::Metavar {
                    where_id: where_id_0,
                    var_id: var_id_0,
                },
                Symbol::Metavar {
                    where_id: where_id_1,
                    var_id: var_id_1,
                },
            ) => {
                if where_id_0 == where_id_1 && var_id_0 == var_id_1 {
                    Ok(existing_subs)
                } else {
                    Err(todo!("error[unify]"))
                }
            }
            (
                Symbol::Reference {
                    inner_data: inner_data_0,
                },
                Symbol::Reference {
                    inner_data: inner_data_1,
                },
            ) => self.unify_immut(*inner_data_0, *inner_data_1, existing_subs),
            (Symbol::Subs { to: to_0 }, Symbol::Subs { to: to_1 }) if *to_0 == *to_1 => {
                Ok(existing_subs)
            }
            (Symbol::Subs { to }, _) => match *existing_subs.get(to).unwrap_or(to) {
                to if sym_a == to => Ok({
                    let mut subs = existing_subs;
                    subs.insert(sym_a, sym_b); // [a:unk]->[b:concrete]
                    subs
                }),
                sym_a => self.unify_immut(sym_a, sym_b, existing_subs),
            },
            (_, Symbol::Subs { to }) => match *existing_subs.get(to).unwrap_or(to) {
                to if sym_b == to => Ok({
                    let mut subs = existing_subs;
                    subs.insert(sym_b, sym_a); // [b:unk]->[a:concrete]
                    subs
                }),
                sym_b => self.unify_immut(sym_a, sym_b, existing_subs),
            },
            _ => Err(todo!("error[unify]")),
        }
    }
    /*
    is it possible to end up with infinite recursive structure {A->B, B->A} from simplified structure via only unify steps spawned of other unify calls.

    X[A,B] & X[B,A]
    => [A & B], [B & A]
    => {A->B}, [B & A]
    => {A->B} + [B & B]
    => {A->B} + {}
    => {A->B}



     */

    /*
    fn factorial(x) =
        x == 0
            ? panic() // no constraint
            : factorial(x) * x   // ret = temp2 = ret :: where Mul(temp2)

     */
    /*

    // ret = nullable[ret]
    fn x() =
        if rand_probability(0.5) {
            some(x())
        } else {
            null
        }
    */
    /*

    fn a[T](t:T) {
        fn b[R](t:T, r:T) {

        }

        b[i32](t, r)
    }


    */
}

#[test]
fn test_simplify_subs() {
    let data_symbol = Symbol::Data {
        data_id: GlobalId {
            module_id: 0,
            id: 0,
        },
        bindings: Vec::new(),
    };
    for i in 0..3 {
        let mut solver = Solver {
            errors: Vec::new(),
            solved_functions: Vec::new(),
            solver_requested_constraints: Vec::new(),
            symbols: Vec::from([
                Symbol::Subs { to: 1 },
                Symbol::Subs { to: 2 },
                data_symbol.clone(),
            ]),
        };
        solver.simplify_subs(i);
        if i == 0 {
            assert_eq!(solver.symbols[0], Symbol::Subs { to: 2 });
        } else {
            assert_eq!(solver.symbols[0], Symbol::Subs { to: 1 });
        }
        assert_eq!(solver.symbols[1], Symbol::Subs { to: 2 });
        assert_eq!(solver.symbols[2], data_symbol);
    }
}
