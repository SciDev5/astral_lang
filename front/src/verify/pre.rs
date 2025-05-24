//! Input types to the verify stage.

// Type symbols :
//      global (allows correlation across function borders (lambdas have
//      externally driven types)) (also inference of data entry types
//      with is spicy may or may not use)
// Reference symbols :
//      represents an instance of data that will be in memory (follows
//      ownership model)

// where do metatype impls come from:
//      - metavar from its where
//      - data by checking all impls [for efficiency should be lut for now no]

use std::collections::HashMap;

use crate::misc::ArbitraryInt;

pub struct PreModule {
    symbols: Vec<Symbol>,
    wheres: Vec<PreWhere>,
    datas: LocalsAndExternalRefs<PreData>,
    functions: LocalsAndExternalRefs<PreFunction>,
    metatypes: LocalsAndExternalRefs<PreMetatype>,
    metatype_impls: LocalsAndExternalRefs<PreMetatypeImpl>,
}

struct LocalsAndExternalRefs<T> {
    locals: Vec<T>,
    /// `Vec<{ package_id, thing_id }>`
    external_refs: Vec<(usize, usize)>,
}

pub type SymbolId = usize;
pub type DataId = usize;
pub type FunctionId = usize;
pub type MetatypeId = usize;
pub type WhereId = usize;

pub struct PreData {
    where_id: WhereId,
    fields: PreDataFields,
}
pub enum PreDataFields {
    Union {
        map: HashMap<String, PreDataFieldEntry>,
    },
    StructIsh {
        map: HashMap<String, PreDataFieldEntry>,
    },
    TupleIsh {
        map: Vec<PreDataFieldEntry>,
    },
}
pub enum PreDataFieldEntry {
    SubData(DataId),
    Field(SymbolId),
}
pub struct PreFunctionCommon {
    where_id: WhereId,
    args_ty: Vec<SymbolId>,
    return_ty: SymbolId,
}
pub struct PreFunction {
    com: PreFunctionCommon,
    body: PreBody,
}
pub struct PreMetatypeFunction {
    com: PreFunctionCommon,
    body: Option<PreBody>,
}
pub struct PreMetatype {
    where_id: WhereId,
    fns: Vec<PreMetatypeFunction>,
}
pub struct PreMetatypeImpl {
    where_id: WhereId,
    fns: Vec<Option<FunctionId>>,
}
pub struct PreWhere {
    /// outer scope where clause to concatonate before this
    parent_id: Option<WhereId>,
    n_vars: usize,
    constraints: Vec<(MetatypeId, Vec<SymbolId>)>,
}
pub enum Symbol {
    Data {
        data_id: DataId,
        bindings: Vec<SymbolId>,
    },
    Function {
        function_id: FunctionId,
        bindings: Vec<SymbolId>,
    },
    MetatypeFunction {
        metatype_id: MetatypeId,
        function_id: usize,
        bindings: Vec<SymbolId>,
    },
    Metavar {
        where_id: WhereId,
        var_id: usize,
    },
    Reference {
        inner_data: SymbolId,
    },
    Subs {
        to: SymbolId,
    },
}
pub struct PreBody {
    /// Note: the first locals are bound to function arguments.
    locals: Vec<SymbolId>,
    expr: PreExpr,
}
pub struct PreExpr {
    ret_ty: SymbolId,
    eval: PreExprEval,
}
pub enum PreExprEval {
    Literal {
        value: PreExprLiteral,
    },
    Call {
        callable: Box<PreExpr>,
        arguments: Vec<PreExpr>,
    },
    Block {
        inner: Vec<PreExpr>,
    },
    Possibilities {
        possibilities: Vec<PreExpr>,
    },
    LocalRef {
        local_ref_id: usize,
    },
    Deref {
        reference: Box<PreExpr>,
    },
    Assign {
        receiver: PreExprPattern,
        value: Box<PreExpr>,
    },
    DataInit {
        data_id: Option<DataId>,
        value: PreExprDataInit,
    },
}
pub enum PreExprLiteral {
    FunctionRef {
        function_id: FunctionId,
    },
    MetatypeFunctionRef {
        metatype_id: MetatypeId,
        function_id: usize,
    },
    Unit {
        data_id: DataId,
    },
    Integer(ArbitraryInt),
    Bool(bool),
}
pub enum PreExprDataInit {
    StructIsh { map: HashMap<String, PreExpr> },
    TupleIsh { map: Vec<PreExpr> },
}
pub enum PreExprPattern {
    Literal { literal: PreExprLiteral },
    Var { local_ref_id: usize },
    StructIsh { map: HashMap<String, PreExpr> },
    TupleIsh { map: Vec<PreExpr> },
}
