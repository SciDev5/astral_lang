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

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    core::CoreRefs,
    misc::ArbitraryInt,
    post::{
        AModule, ANamespace, DataId, FunctionId, FunctionIdLocal, LocalId, MetatypeId, ModuleId,
        WhereId, WhereIdGlobal,
    },
};

pub struct PreModule {
    pub global_id: ModuleId,
    pub deps: HashSet<ModuleId>,
    pub ref_recursive_deps: HashMap<ModuleId, Arc<AModule>>,
    pub ref_core: CoreRefs,
    pub symbols: Vec<Symbol>,
    pub wheres: Vec<PreWhere>,
    pub datas: Vec<PreData>,
    pub functions: Vec<PreFunction>,
    pub metatypes: Vec<PreMetatype>,
    pub metatype_impls: Vec<PreMetatypeImpl>,
    pub namespaces: Vec<ANamespace>,
}

pub type SymbolId = LocalId;

pub struct PreData {
    pub where_id: WhereId,
    pub fields: PreDataFields,
}
pub enum PreDataFields {
    Elemental,
    Union { map: Vec<(String, SymbolId)> },
    IntersectionNamed { map: Vec<(String, SymbolId)> },
    IntersectionOrdered { map: Vec<SymbolId> },
}
pub struct PreFunction {
    pub where_id: WhereId,
    pub args_ty: Vec<SymbolId>,
    pub return_ty: SymbolId,
    pub body: Option<PreBody>,
}

pub struct PreMetatype {
    /// First constraint should be this metatype, and it should bind all
    /// the metavars it creates and in strict ascending order without skips.
    /// (this makes the functions behave as expected.)
    pub where_id: WhereId,
    pub fns: Vec<FunctionIdLocal>,
}
pub struct PreMetatypeImpl {
    /// First constraint is the metatype to implement.
    pub where_id: WhereId,
    pub fns: Vec<Option<FunctionIdLocal>>,
}
pub struct PreWhere {
    /// outer scope where clause to concatonate before this
    pub parent_id: Option<WhereId>,
    pub n_vars: usize,
    pub constraints: Vec<(MetatypeId, Vec<SymbolId>)>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    Data {
        data_id: DataId,
        bindings: Vec<SymbolId>,
    },
    FunctionPointer {
        args: Vec<SymbolId>,
        ret: SymbolId,
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
        where_id: WhereIdGlobal,
        var_id: usize,
    },
    Reference {
        inner_data: SymbolId,
    },
    Subs {
        to: SymbolId,
    },
    Error {},
}
pub struct PreBody {
    /// Note: the first locals are bound to function arguments.
    /// The zero'th local is at index `args.len()`
    pub locals: Vec<SymbolId>,
    pub expr: PreExpr,
}
#[derive(Debug)]
pub struct PreExpr {
    pub ret_ty: SymbolId,
    pub eval: PreExprEval,
}
#[derive(Debug)]
pub enum PreExprEval {
    Literal {
        value: PreExprLiteral,
    },
    Call {
        callable: Box<PreExpr>,
        arguments: Vec<PreExpr>,
    },
    CallFunction {
        function_id: FunctionId,
        arguments: Vec<PreExpr>,
    },
    CallMetatypeFunction {
        metatype_id: MetatypeId,
        function_id: usize,
        arguments: Vec<PreExpr>,
    },
    Block {
        inner: Vec<PreExpr>,
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
        value: PreExprDataInitContent,
    },
    DataAccess {
        value: Box<PreExpr>,
        field: PreExprField,
    },
    Return {
        value: Box<PreExpr>,
    },
    // TODO: branch, loop
    Error {},
}
#[derive(Debug)]
pub enum PreExprLiteral {
    FunctionRef {
        function_id: FunctionId,
    },
    MetatypeFunctionRef {
        metatype_id: MetatypeId,
        function_id: usize,
    },
    Integer(ArbitraryInt),
    Bool(bool),
}
#[derive(Debug)]
pub enum PreExprDataInitContent {
    Union {
        variant_name: String,
        val: Box<PreExpr>,
    },
    IntersectionNamed {
        map: HashMap<String, PreExpr>,
    },
    IntersectionOrdered {
        map: Vec<PreExpr>,
    },
}
#[derive(Debug)]
pub enum PreExprPattern {
    Literal {
        literal: PreExprLiteral,
    },
    Var {
        local_ref_id: usize,
    },
    Union {
        variant_name: String,
        sub_pat: Box<PreExprPattern>,
    },
    IntersectionNamed {
        fields: HashMap<String, PreExprPattern>,
    },
    IntersectionOrdered {
        fields: Vec<PreExprPattern>,
    },
}
#[derive(Debug)]
pub enum PreExprField {
    StructIsh(String),
    TupleIsh(usize),
}
