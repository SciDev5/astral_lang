use std::collections::HashMap;

use crate::{
    parse::{
        ast::{ASTExpr, ASTFunction, ASTType},
        loc,
    },
    post::{ANamespace, GlobalId, ModuleId, NamespaceId, WhereId},
    resolve::extract::{Scope, UnresolvedModule},
    verify::pre::{
        PreBody, PreExpr, PreExprEval, PreExprLiteral, PreExprPattern, PreFunction, PreModule,
        PreWhere, Symbol, SymbolId,
    },
};

fn resolve(module: UnresolvedModule) -> PreModule {
    let mut symbols = Vec::new();
    let mut add_symbol = |sym: Option<Symbol>| {
        symbols.push(sym.unwrap_or(Symbol::Subs { to: symbols.len() }));
        symbols.len() - 1
    };
    let mut wheres = Vec::new();
    let mut add_where = |v_where| {
        wheres.push(v_where);
        wheres.len() - 1
    };

    let functions = module
        .functions
        .into_iter()
        .map(|(function, scope_id)| {
            resolve_function(
                &mut add_symbol,
                &module.scopes,
                function,
                scope_id,
                &mut add_where,
            )
        })
        .collect();
    let datas = module
        .datas
        .into_iter()
        .map(|(data, scope_id)| todo!())
        .collect();
    let metatypes = module
        .metatypes
        .into_iter()
        .map(|(metatype, scope_id)| todo!())
        .collect();
    let metatype_impls = module
        .metatype_impls
        .into_iter()
        .map(|(metatype_impl, scope_id)| todo!())
        .collect();
    let namespaces = module
        .namespaces
        .into_iter()
        .map(|scope_id| {
            dbg!("TODO translate namespaces from scopes lol");
            ANamespace {}
        })
        .collect();

    PreModule {
        global_id: module.global_id,
        deps: module.deps,
        ref_recursive_deps: module.ref_recursive_deps,
        ref_core: module.ref_core,
        symbols,
        wheres,
        datas,
        functions,
        metatypes,
        metatype_impls,
        namespaces,
    }
}

fn resolve_function(
    add_symbol: &mut impl FnMut(Option<Symbol>) -> SymbolId,
    scopes: &Vec<Scope>,
    function: ASTFunction,
    scope: usize,
    add_where: &mut impl FnMut(PreWhere) -> WhereId,
) -> PreFunction {
    let mut locals = Vec::new();
    let mut locals_lut = Vec::from([HashMap::new()]);
    let mut create_local = |symbol_id| {
        locals.push(symbol_id);
        locals.len() - 1
    };
    let n_args = function.arguments.len();

    let body_expr = resolve_expr(
        add_symbol,
        &mut create_local,
        &mut locals_lut,
        scopes,
        *function.body,
        scope,
    )
    .unwrap_or_else(|| empty_block(add_symbol));

    PreFunction {
        args_ty: locals.iter().copied().take(n_args).collect(),
        body: Some(PreBody {
            expr: body_expr,
            locals,
        }),
        return_ty: resolve_type(add_symbol, scopes, function.return_ty, scope),
        where_id: add_where({
            dbg!("TODO wheres");
            PreWhere {
                parent_id: None,
                n_vars: 0,
                constraints: Vec::new(),
            }
        }),
    }
}
fn empty_block(add_symbol: &mut impl FnMut(Option<Symbol>) -> SymbolId) -> PreExpr {
    PreExpr {
        ret_ty: add_symbol(None),
        eval: PreExprEval::Block { inner: Vec::new() },
    }
}
fn resolve_expr(
    add_symbol: &mut impl FnMut(Option<Symbol>) -> SymbolId,
    create_local: &mut impl FnMut(SymbolId) -> usize,
    locals: &mut Vec<HashMap<String, usize>>,
    scopes: &Vec<Scope>,
    expr: ASTExpr,
    scope: usize,
) -> Option<PreExpr> {
    Some(PreExpr {
        ret_ty: add_symbol(None),
        eval: match expr {
            ASTExpr::Block { scope, exprs } => {
                locals.push(HashMap::new());
                let res = PreExprEval::Block {
                    inner: exprs
                        .into_iter()
                        .filter_map(|expr| {
                            resolve_expr(
                                add_symbol,
                                create_local,
                                locals,
                                scopes,
                                expr,
                                scope.unwrap(),
                            )
                        })
                        .collect(),
                };
                locals.pop();
                res
            }
            ASTExpr::Call { callee, arguments } => PreExprEval::Call {
                callable: Box::new(
                    resolve_expr(add_symbol, create_local, locals, scopes, *callee, scope)
                        .unwrap_or_else(|| empty_block(add_symbol)),
                ),
                arguments: arguments
                    .into_iter()
                    .map(|expr| {
                        resolve_expr(add_symbol, create_local, locals, scopes, expr, scope)
                            .unwrap_or_else(|| empty_block(add_symbol))
                    })
                    .collect(),
            },
            ASTExpr::Error {} => PreExprEval::Error {},
            ASTExpr::NoOp => return None,
            ASTExpr::LetTEMP { var_name, ty, expr } => {
                let locals_scope = locals.last_mut().unwrap();
                let var_id = create_local(resolve_type(add_symbol, scopes, ty, scope));
                locals_scope.insert(var_name, var_id);
                PreExprEval::Assign {
                    receiver: PreExprPattern::Var {
                        local_ref_id: var_id,
                    },
                    value: Box::new(
                        resolve_expr(add_symbol, create_local, locals, scopes, *expr, scope)
                            .unwrap_or_else(|| empty_block(add_symbol)),
                    ),
                }
            }
            ASTExpr::Ident { var_name } => {
                let mut iter_locals = locals.iter_mut().rev();
                let mut iter_scopes = iter_scopes(scope, scopes);
                loop {
                    match (
                        iter_locals.next().map(|v| v.get(&var_name)),
                        iter_scopes.next().map(|v| v.functions.get(&var_name)),
                    ) {
                        (Some(Some(var_id)), _) => {
                            break PreExprEval::LocalRef {
                                local_ref_id: *var_id,
                            }
                        }
                        (_, Some(Some(function_id))) => {
                            break PreExprEval::Literal {
                                value: PreExprLiteral::FunctionRef {
                                    function_id: *function_id,
                                },
                            }
                        }
                        (None, None) => break PreExprEval::Error {},
                        _ => continue,
                    };
                }
            }
            ASTExpr::Access { name, from } => PreExprEval::DataAccess {
                value: Box::new(
                    resolve_expr(add_symbol, create_local, locals, scopes, *from, scope)
                        .unwrap_or_else(|| empty_block(add_symbol)),
                ),
                field: crate::verify::pre::PreExprField::StructIsh(name),
            },
            ASTExpr::OperatorBinary {
                expr_first,
                expr_second,
                op,
            } => todo!(),
            ASTExpr::OperatorUnary { expr, op } => todo!(),
            ASTExpr::Static(_) => {
                unreachable!("should be consumed by extract and turned to ASTExpr::NoOp")
            }
        },
    })
}

fn resolve_type(
    add_symbol: &mut impl FnMut(Option<Symbol>) -> SymbolId,
    scopes: &Vec<Scope>,
    ty: ASTType,
    scope: usize,
) -> SymbolId {
    match ty {
        ASTType::Data { name, arguments } => {
            let sym = match lookup_x(&name, |scope| &scope.datas, scopes, scope) {
                ResolveLookupResult::Error {} => Symbol::Error {},
                ResolveLookupResult::Namespace { id } => todo!(),
                ResolveLookupResult::Match { id } => Symbol::Data {
                    data_id: id,
                    bindings: arguments
                        .into_iter()
                        .map(|ty| resolve_type(add_symbol, scopes, ty, scope))
                        .collect(),
                },
            };
            add_symbol(Some(sym))
        }
    }
}

enum ResolveLookupResult {
    Error {},
    Namespace { id: NamespaceId },
    Match { id: GlobalId },
}

fn lookup_x(
    name: &str,
    get_x: impl Fn(&Scope) -> &HashMap<String, GlobalId>,
    scopes: &Vec<Scope>,
    scope: usize,
) -> ResolveLookupResult {
    for scope in iter_scopes(scope, scopes) {
        if let Some(id) = get_x(scope).get(name) {
            return ResolveLookupResult::Match { id: *id };
        }
    }
    ResolveLookupResult::Error {}
}
// fn lookup_chain_x(
//     this_module_id: ModuleId,
//     name_in: &str,
//     namespace_in: NamespaceId,
//     get_x_local: impl Fn(&Scope) -> &Vec<(String, GlobalId)>,
//     get_x_remote: impl Fn(&ANamespace) -> &Vec<(String, GlobalId)>,
//     scopes: &Vec<Scope>,
// ) -> ResolveLookupResult {
//     let lut = if this_module_id == namespace_in.module_id {
//         get_x_local(&scopes[namespace_in.id])
//     } else {
//         get_x_remote()
//     };
//     ResolveLookupResult::Error {}
// }

fn iter_scopes(scope: usize, scopes: &Vec<Scope>) -> impl Iterator<Item = &Scope> {
    std::iter::successors(Some(scope), |prev_scope| scopes[*prev_scope].parent)
        .map(|scope_id| &scopes[scope_id])
}
