use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use front::{resolve::extract::Scope, GlobalId};

fn main() {
    let (ref_core, mod_core) = front::core::gen_core();
    let mod_builtins = interpreter::interpreter_builtins(ref_core);

    // let test_code = "fn main() = {}";
    let test_code = r"

    fn main() = {
        let x = a()
        x
    }

    fn a() = {} // <- returns void

    ";

    let test_code = front::parse::parse_top_level(test_code);

    let modid_test = 12345;
    let mut mod_test = front::resolve::extract::UnresolvedModule {
        global_id: modid_test,
        deps: HashSet::new(),
        ref_recursive_deps: [mod_core.clone(), mod_builtins.clone()]
            .into_iter()
            .map(|v| (v.global_id, v))
            .collect(),
        ref_core,
        scopes: Vec::from([Scope {
            ..Default::default()
        }]),
        datas: Vec::new(),
        functions: Vec::new(),
        metatypes: Vec::new(),
        metatype_impls: Vec::new(),
        imports: Vec::new(),
        namespaces: Vec::from([0]),
    };
    front::resolve::extract::extract_statics(&mut mod_test, 0, test_code);
    let mod_test = front::resolve::resolve::resolve(mod_test);
    let (mod_test, _) = front::verify::solver::solve_module(mod_test);
    let mod_test = front::finish::finish_module(mod_test);

    let modules = [mod_core, mod_builtins, Arc::new(mod_test)]
        .into_iter()
        .map(|v| (v.global_id, v))
        .collect::<HashMap<_, _>>();
    let return_value = interpreter::interpret(
        &ref_core,
        &modules,
        GlobalId {
            module_id: modid_test,
            id: 0,
        },
    );
    dbg!(return_value); // should be Value::CoreVoid
}
