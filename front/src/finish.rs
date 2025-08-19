use crate::{
    verify::pre::{
        PreData, PreExpr, PreExprEval, PreFunction, PreMetatype, PreMetatypeImpl, PreModule,
        PreWhere, Symbol,
    },
    ABody, AData, AExpr, AExprEval, AFunction, AMetatype, AMetatypeImpl, AModule, AType, AWhere,
};

pub fn finish_module(module: PreModule) -> AModule {
    let symbols = &module.symbols;
    AModule {
        global_id: module.global_id,
        deps: module.deps,
        wheres: module
            .wheres
            .into_iter()
            .map(|v| finish_where(v, symbols))
            .collect(),
        datas: module
            .datas
            .into_iter()
            .map(|v| finish_data(v, symbols))
            .collect(),
        functions: module
            .functions
            .into_iter()
            .map(|v| finish_function(v, symbols))
            .collect(),
        metatypes: module
            .metatypes
            .into_iter()
            .map(|v| finish_metatype(v, symbols))
            .collect(),
        metatype_impls: module
            .metatype_impls
            .into_iter()
            .map(|v| finish_metatype_impl(v, symbols))
            .collect(),
        namespaces: module.namespaces,
    }
}

fn finish_where(where_: PreWhere, symbols: &Vec<Symbol>) -> AWhere {
    if where_.constraints.is_empty() {
        return AWhere {
            constraints: Vec::new(),
            n_vars: where_.n_vars,
            parent_id: where_.parent_id,
        };
    }
    todo!()
}
fn finish_data(data: PreData, symbols: &Vec<Symbol>) -> AData {
    todo!()
}
fn finish_function(function: PreFunction, symbols: &Vec<Symbol>) -> AFunction {
    AFunction {
        where_id: function.where_id,
        args_ty: function
            .args_ty
            .into_iter()
            .map(|id| symbol_to_type(id, symbols))
            .collect(),
        return_ty: symbol_to_type(function.return_ty, symbols),
        body: function.body.map(|body| ABody {
            locals: body
                .locals
                .into_iter()
                .map(|id| symbol_to_type(id, symbols))
                .collect(),
            expr: finish_expr(body.expr, symbols),
        }),
    }
}
fn finish_metatype(metatype: PreMetatype, symbols: &Vec<Symbol>) -> AMetatype {
    todo!()
}
fn finish_metatype_impl(metatype_impl: PreMetatypeImpl, symbols: &Vec<Symbol>) -> AMetatypeImpl {
    todo!()
}

fn finish_expr(expr: PreExpr, symbols: &Vec<Symbol>) -> AExpr {
    let eval = match expr.eval {
        PreExprEval::Literal { value } => todo!(),
        PreExprEval::Call {
            callable,
            arguments,
        } => unreachable!(),
        PreExprEval::CallFunction {
            function_id,
            arguments,
        } => AExprEval::CallFunction {
            function_id,
            arguments: arguments
                .into_iter()
                .map(|expr| finish_expr(expr, symbols))
                .collect(),
        },
        PreExprEval::CallMetatypeFunction {
            metatype_id,
            function_id,
            arguments,
        } => todo!(),
        PreExprEval::Block { inner } => AExprEval::Block {
            inner: inner
                .into_iter()
                .map(|expr| finish_expr(expr, symbols))
                .collect(),
        },
        PreExprEval::LocalRef { local_ref_id } => AExprEval::LocalRef { local_ref_id },
        PreExprEval::Deref { reference } => todo!(),
        PreExprEval::Assign { receiver, value } => todo!(),
        PreExprEval::DataInit { data_id, value } => todo!(),
        PreExprEval::DataAccess { value, field } => todo!(),
        PreExprEval::Return { value } => AExprEval::Return {
            value: Box::new(finish_expr(*value, symbols)),
        },
        PreExprEval::Error {} => todo!(),
    };
    AExpr {
        ret_ty: symbol_to_type(expr.ret_ty, symbols),
        eval,
    }
}

fn symbol_to_type(mut symbol_id: usize, symbols: &Vec<Symbol>) -> AType {
    loop {
        break match &symbols[symbol_id] {
            Symbol::Data { data_id, bindings } => AType::Data {
                data_id: *data_id,
                bindings: bindings
                    .iter()
                    .map(|id| symbol_to_type(*id, symbols))
                    .collect(),
            },
            Symbol::FunctionPointer { args, ret } => todo!(),
            Symbol::Function {
                function_id,
                bindings,
            } => todo!(),
            Symbol::MetatypeFunction {
                metatype_id,
                function_id,
                bindings,
            } => todo!(),
            Symbol::Metavar { where_id, var_id } => todo!(),
            Symbol::Reference { inner_data } => todo!(),
            Symbol::Subs { to } => {
                if symbol_id == *to {
                    AType::Error {}
                } else {
                    symbol_id = *to;
                    continue;
                }
            }
            Symbol::Error {} => todo!(),
        };
    }
}
