use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    core::CoreRefs,
    parse::ast::{
        ASTData, ASTExpr, ASTFunction, ASTImportTree, ASTMetatype, ASTMetatypeImpl, ASTStatic,
        ASTTopLevel, ASTWhere,
    },
    post::{AModule, DataId, FunctionId, GlobalId, LocalId, MetatypeId, ModuleId, NamespaceId},
};

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub functions: HashMap<String, FunctionId>,
    pub datas: HashMap<String, DataId>,
    pub metatypes: HashMap<String, MetatypeId>,
    pub namespaces: HashMap<String, NamespaceId>,
}
type ScopeId = usize;
pub struct UnresolvedModule {
    pub global_id: ModuleId,
    pub deps: HashSet<ModuleId>,
    pub ref_recursive_deps: HashMap<ModuleId, Arc<AModule>>,
    pub ref_core: CoreRefs,
    pub scopes: Vec<Scope>,
    pub datas: Vec<(ASTData, ScopeId)>,
    pub functions: Vec<(ASTFunction, ScopeId)>,
    pub metatypes: Vec<(ASTMetatype, ScopeId)>,
    pub metatype_impls: Vec<(ASTMetatypeImpl, ScopeId)>,
    pub imports: Vec<(ASTImportTree, ScopeId)>,
    pub namespaces: Vec<ScopeId>,
}

pub fn resolve_imports(module: &mut UnresolvedModule) {
    // extracts information from ImportTrees and converts them into references in the parent scope
    // module.imports
    // module.scopes
    if module.imports.len() > 0 {
        todo!()
    }
}

pub fn extract_statics(module: &mut UnresolvedModule, file_id: usize, mut ast: ASTTopLevel) {
    let top_level_scope = module.namespaces[file_id];
    ast.scope = Some(top_level_scope);
    let mut scopes = Vec::from([top_level_scope]);
    for ast_def in ast.defs {
        extract_statics_statics(module, &mut scopes, ast_def);
    }
}

fn extract_statics_expr(module: &mut UnresolvedModule, scopes: &mut Vec<usize>, ast: &mut ASTExpr) {
    let mut ast_ = ASTExpr::NoOp;
    std::mem::swap(&mut ast_, ast);

    let ast_ = match ast_ {
        ASTExpr::LetTEMP {
            var_name,
            ty,
            mut expr,
        } => {
            extract_statics_expr(module, scopes, &mut expr);
            ASTExpr::LetTEMP { var_name, ty, expr }
        }
        ASTExpr::Access { name, mut from } => {
            extract_statics_expr(module, scopes, &mut from);
            ASTExpr::Access { name, from }
        }
        ASTExpr::Call {
            mut callee,
            mut arguments,
        } => {
            extract_statics_expr(module, scopes, &mut callee);
            for argument in &mut arguments {
                extract_statics_expr(module, scopes, argument);
            }
            ASTExpr::Call { callee, arguments }
        }
        ASTExpr::Block {
            mut scope,
            mut exprs,
        } => {
            extract_statics_block(module, scopes, &mut scope, &mut exprs);
            ASTExpr::Block { scope, exprs }
        }
        ASTExpr::OperatorBinary {
            mut expr_first,
            mut expr_second,
            op,
        } => {
            extract_statics_expr(module, scopes, &mut expr_first);
            extract_statics_expr(module, scopes, &mut expr_second);
            ASTExpr::OperatorBinary {
                expr_first,
                expr_second,
                op,
            }
        }
        ASTExpr::OperatorUnary { mut expr, op } => {
            extract_statics_expr(module, scopes, &mut expr);
            ASTExpr::OperatorUnary { expr, op }
        }
        ASTExpr::Static(aststatic) => {
            extract_statics_statics(module, scopes, aststatic);
            ASTExpr::NoOp
        }
        v => v,
    };

    *ast = ast_;
}
fn extract_statics_statics(
    module: &mut UnresolvedModule,
    scopes: &mut Vec<usize>,
    mut ast: ASTStatic,
) {
    match &mut ast {
        ASTStatic::Function(astfunction) => {
            extract_statics_expr(module, scopes, &mut astfunction.body);
        }
        _ => {}
    }

    let scope_id = *scopes.last().unwrap();
    let scope = &mut module.scopes[scope_id];

    let local_as_global_id = |id: LocalId| GlobalId {
        id,
        module_id: module.global_id,
    };
    fn push_with_id<T>(arr: &mut Vec<T>, val: T) -> LocalId {
        arr.push(val);
        arr.len() - 1
    }

    match ast {
        ASTStatic::Function(astfunction) => {
            scope.functions.insert(
                astfunction.name.clone(),
                local_as_global_id(push_with_id(&mut module.functions, (astfunction, scope_id))),
            );
        }
        ASTStatic::Data(astdata) => {
            scope.datas.insert(
                astdata.name.clone(),
                local_as_global_id(push_with_id(&mut module.datas, (astdata, scope_id))),
            );
        }
        ASTStatic::Metatype(astmetatype) => {
            scope.metatypes.insert(
                astmetatype.name.clone(),
                local_as_global_id(push_with_id(&mut module.metatypes, (astmetatype, scope_id))),
            );
        }
        ASTStatic::MetatypeImpl(astmetatype_impl) => {
            module.metatype_impls.push((astmetatype_impl, scope_id));
        }
        ASTStatic::Import(astimport_tree) => {
            module.imports.push((astimport_tree, scope_id));
        }
    }
}

fn extract_statics_block(
    module: &mut UnresolvedModule,
    scopes: &mut Vec<usize>,
    block_scope: &mut Option<usize>,
    ast: &mut Vec<ASTExpr>,
) {
    block_scope.get_or_insert_with(|| {
        let n = scopes.len();
        module.scopes.push(Scope {
            parent: scopes.last().copied(),
            ..Default::default()
        });
        scopes.push(n);
        n
    });
    for ast_expr in ast {
        extract_statics_expr(module, scopes, ast_expr);
    }
}
