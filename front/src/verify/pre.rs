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
    pub symbols: Vec<Symbol>,
    pub wheres: Vec<PreWhere>,
    pub datas: LocalsAndExternalRefs<PreData>,
    pub functions: LocalsAndExternalRefs<PreFunction>,
    pub metatypes: LocalsAndExternalRefs<PreMetatype>,
    pub metatype_impls: LocalsAndExternalRefs<PreMetatypeImpl>,
}

pub struct LocalsAndExternalRefs<T> {
    pub locals: Vec<T>,
    /// `Vec<{ package_id, thing_id }>`
    pub external_refs: Vec<(usize, usize)>,
}
impl<T> LocalsAndExternalRefs<T> {
    pub fn get(&self, i: usize) -> &T {
        if i < self.locals.len() {
            &self.locals[i]
        } else {
            todo!()
        }
    }
}

pub type SymbolId = usize;
pub type DataId = usize;
pub type FunctionId = usize;
pub type MetatypeId = usize;
pub type WhereId = usize;

pub struct PreData {
    pub where_id: WhereId,
    pub fields: PreDataFields,
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
pub struct PreFunction {
    pub where_id: WhereId,
    pub args_ty: Vec<SymbolId>,
    pub return_ty: SymbolId,
    pub body: Option<PreBody>,
}

pub struct PreMetatype {
    pub where_id: WhereId,
    pub fns: Vec<FunctionId>,
}
pub struct PreMetatypeImpl {
    pub where_id: WhereId,
    pub metatype_id: MetatypeId,
    pub fns: Vec<Option<FunctionId>>,
}
pub struct PreWhere {
    /// outer scope where clause to concatonate before this
    pub parent_id: Option<WhereId>,
    pub n_vars: usize,
    pub constraints: Vec<(MetatypeId, Vec<SymbolId>)>,
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
    pub locals: Vec<SymbolId>,
    pub expr: PreExpr,
}
pub struct PreExpr {
    pub ret_ty: SymbolId,
    pub eval: PreExprEval,
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
    DataAccess {
        value: Box<PreExpr>,
        field: PreExprField,
    },
    Return {
        value: Box<PreExpr>,
    },
    // TODO: branch, loop
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
    Literal {
        literal: PreExprLiteral,
    },
    Var {
        local_ref_id: usize,
    },
    StructIsh {
        map: HashMap<String, PreExprPattern>,
    },
    TupleIsh {
        map: Vec<PreExprPattern>,
    },
}
pub enum PreExprField {
    StructIsh(String),
    TupleIsh(usize),
}
