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
    fn solve_function(&mut self, function_id: FunctionId) {}

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
}
