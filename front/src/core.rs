use std::{collections::HashSet, sync::Arc};

use crate::post::{
    AData, ADataFields, AFunction, AMetatype, AModule, AType, AWhere, GlobalId, LocalId,
};

#[derive(Debug, Clone, Copy)]
pub struct CoreRefs {
    // pub mt_add: GlobalId,
    // pub mt_sub: GlobalId,
    // pub mt_mul: GlobalId,
    // pub mt_div: GlobalId,
    pub mt_call: [GlobalId; 32],
    pub d_void: GlobalId,
    pub d_bool: GlobalId,
    pub d_i8: GlobalId,
    pub d_i16: GlobalId,
    pub d_i32: GlobalId,
    pub d_i64: GlobalId,
    pub d_u8: GlobalId,
    pub d_u16: GlobalId,
    pub d_u32: GlobalId,
    pub d_u64: GlobalId,
}

pub fn gen_core() -> (CoreRefs, Arc<AModule>) {
    let mut core = AModule {
        global_id: 0,
        deps: HashSet::new(),
        wheres: Vec::new(),
        datas: Vec::new(),
        functions: Vec::new(),
        metatypes: Vec::new(),
        metatype_impls: Vec::new(),
        namespaces: Vec::new(),
    };
    fn gen_insert<T>(l: &mut Vec<T>, v: impl FnOnce(LocalId) -> T) -> LocalId {
        let i = l.len();
        l.push(v(i));
        i
    }
    let globalize = |id: LocalId| GlobalId {
        module_id: core.global_id,
        id,
    };
    let gen_data_elemental = |datas: &mut Vec<AData>, wheres: &mut Vec<AWhere>| {
        globalize(gen_insert(datas, |_| AData {
            where_id: gen_insert(wheres, |_| AWhere {
                parent_id: None,
                n_vars: 0,
                constraints: Vec::new(),
            }),
            fields: ADataFields::Elemental,
        }))
    };
    (
        CoreRefs {
            mt_call: std::array::from_fn(|i| {
                globalize(gen_insert(&mut core.metatypes, |mt_id| {
                    let where_id = gen_insert(&mut core.wheres, |where_id| AWhere {
                        n_vars: i + 1,
                        constraints: Vec::from([(
                            globalize(mt_id),
                            (0..=i)
                                .map(|var_id| AType::Metavar {
                                    where_id: globalize(where_id),
                                    var_id,
                                })
                                .collect(),
                        )]),
                        parent_id: None,
                    });
                    AMetatype {
                        where_id,
                        fns: Vec::from([gen_insert(&mut core.functions, |_| AFunction {
                            where_id,
                            body: None,
                            args_ty: (1..=i)
                                .map(|var_id| AType::Metavar {
                                    where_id: globalize(where_id),
                                    var_id,
                                })
                                .collect(),
                            return_ty: AType::Metavar {
                                where_id: globalize(where_id),
                                var_id: 0,
                            },
                        })]),
                    }
                }))
            }),

            d_void: globalize(gen_insert(&mut core.datas, |_| AData {
                where_id: gen_insert(&mut core.wheres, |_| AWhere {
                    parent_id: None,
                    n_vars: 0,
                    constraints: Vec::new(),
                }),
                fields: ADataFields::IntersectionOrdered { fields: Vec::new() },
            })),

            d_bool: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_i8: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_i16: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_i32: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_i64: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_u8: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_u16: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_u32: gen_data_elemental(&mut core.datas, &mut core.wheres),
            d_u64: gen_data_elemental(&mut core.datas, &mut core.wheres),
        },
        Arc::new(core),
    )
}
