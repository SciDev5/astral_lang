use std::collections::HashMap;

use errorhandling::AVerifyError;

use super::pre::*;

/// Solve the undefined types in the module.
///
/// Requires all static "seed" declarations to be in place
/// already, such as data field types and metatype function
/// types, but expressions should not yet be typed.
fn solve_module(module: PreModule) -> (PreModule, Vec<AVerifyError>) {
    let mut state = Solver {
        solved_functions: std::iter::repeat_n(false, module.functions.locals.len()).collect(),
        errors: Vec::new(),
        solver_requested_constraints: Vec::new(),
        module,
    };
    for i in 0..state.module.metatype_impls.locals.len() {
        state.verify_metatypeimpl_args(i);
    }
    while let Some((i, _)) = state
        .solved_functions
        .iter()
        .enumerate()
        .find(|(_, solved)| !*solved)
    {
        state.solve_function(i);
    }
    for i in 0..state.module.metatype_impls.locals.len() {
        state.verify_metatypeimpl_ret(i);
    }

    state.verify_solver_wheres();

    (state.module, state.errors)
}

struct Solver {
    module: PreModule,
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
    fn solve_function(&mut self, function_id: FunctionId) {
        todo!()
    }

    fn solve_expr(&mut self, function_id: FunctionId, expr: PreExpr) -> PreExpr {
        todo!()
    }

    fn verify_metatypeimpl_args(&mut self, impl_id: usize) {
        let v_impl = &self.module.metatype_impls.locals[impl_id];
        let v_metatype = self.module.metatypes.get(v_impl.metatype_id);
        todo!()
    }
    fn verify_metatypeimpl_ret(&mut self, impl_id: usize) {
        let v_impl = &self.module.metatype_impls.locals[impl_id];
        let v_metatype = self.module.metatypes.get(v_impl.metatype_id);
        todo!()
    }

    fn verify_solver_wheres(&mut self) {
        todo!()
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //// HELPERS ////////////////////////////////////////////////////////////////////////////

    /// Generate the substitutions that would be applied when instatiating this type.
    fn instantiate_gen_subs(
        &mut self,
        bindings: Vec<SymbolId>,
        mut where_id: WhereId,
    ) -> HashMap<(WhereId, usize), SymbolId> {
        let mut where_ids = Vec::from([where_id]);
        while let Some(next_where_id) = self.module.wheres[where_id].parent_id {
            where_ids.push(next_where_id);
            where_id = next_where_id;
        }
        where_ids.reverse(); // outermost first

        let map = where_ids
            .iter()
            .copied()
            .flat_map(|where_id| {
                (0..self.module.wheres[where_id].n_vars).map(move |i| (where_id, i))
            })
            .zip(bindings.into_iter())
            .collect::<HashMap<_, _>>();

        for where_id in where_ids {
            for (metatype_id, bindings) in self.module.wheres[where_id].constraints.clone() {
                let bindings = self
                    .instantiate_substitute_all(bindings.clone(), &map)
                    .unwrap_or(bindings);
                self.solver_requested_constraints
                    .push((metatype_id, bindings));
            }
        }

        map
    }

    /// Substitute types into the place of fields of generic entities in order
    /// to instantiate them.
    ///
    /// Returns `None` if it does not need to be changed.
    fn instantiate_substitute(
        &mut self,
        symbol: SymbolId,
        map: &HashMap<(WhereId, usize), SymbolId>,
    ) -> Option<Symbol> {
        /*
        data TheData[A] struct { y: V[A] }
        let x: TheData[T]
        let y = x.y
        // typeof y = instantiate_substitute[A -> T]{ TheData.y[A] } = V[T]
         */

        match &self.module.symbols[symbol] {
            Symbol::Data { data_id, bindings } => Some(Symbol::Data {
                data_id: *data_id,
                bindings: self.instantiate_substitute_all(bindings.clone(), map)?,
            }),
            Symbol::Function {
                function_id,
                bindings,
            } => Some(Symbol::Function {
                function_id: *function_id,
                bindings: self.instantiate_substitute_all(bindings.clone(), map)?,
            }),
            Symbol::MetatypeFunction {
                metatype_id,
                function_id,
                bindings,
            } => Some(Symbol::MetatypeFunction {
                function_id: *function_id,
                metatype_id: *metatype_id,
                bindings: self.instantiate_substitute_all(bindings.clone(), map)?,
            }),
            Symbol::Metavar { where_id, var_id } => Some(Symbol::Subs {
                to: map.get(&(*where_id, *var_id)).copied()?,
            }),
            Symbol::Reference { inner_data } => Some(Symbol::Reference {
                inner_data: {
                    let sym = self.instantiate_substitute(*inner_data, map)?;
                    self.add_symbol(sym)
                },
            }),
            Symbol::Subs { to } => self.instantiate_substitute(*to, map),
        }
    }
    /// Helper for [`Self::instantiate_substitute`] which runs it on each of the
    /// given input symbols. Substitutes for origional symbol id if some are
    /// unchanged, if all unchanged, returns `None`.
    fn instantiate_substitute_all(
        &mut self,
        symbols_in: Vec<SymbolId>,
        map: &HashMap<(WhereId, usize), SymbolId>,
    ) -> Option<Vec<SymbolId>> {
        let bindings = symbols_in
            .iter()
            .map(|symbol| self.instantiate_substitute(*symbol, map))
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
        let id = self.module.symbols.len();
        self.module.symbols.push(sym);
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
                Symbol::Metavar { .. } => todo!(),
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
            let mut subs_trace = Vec::from([sym]);
            let ok = loop {
                match &symbols[sym] {
                    Symbol::Subs { to } => {
                        sym = *to;
                        subs_trace.push(sym);
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
            for sym in subs_trace {
                symbols[sym] = Symbol::Subs { to: sym };
            }
            ok
        }
        let mut trace = Vec::new();
        return simplify(&mut self.module.symbols, sym, &mut trace);
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
        for (from, to) in subs {
            self.module.symbols[from] = Symbol::Subs { to };
        }
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

        match (&self.module.symbols[sym_a], &self.module.symbols[sym_b]) {
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
                    Err(todo!("unify error"))
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
                    Err(todo!("unify error"))
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
                    Err(todo!("unify error"))
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
                    Err(todo!("unify error"))
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
            _ => Err(todo!("unify error")),
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
