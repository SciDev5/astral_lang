// Type symbols : global (allows correlation across function borders (lambdas have externally driven types)) (also inference of data entry types with is spicy may or may not use)
// Reference symbols : represents an instance of data that will be in memory (follows ownership model)

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

pub struct AModule {
    pub global_id: ModuleId,
    pub deps: HashSet<ModuleId>,
    pub wheres: Vec<AWhere>,
    pub datas: Vec<AData>,
    pub functions: Vec<AFunction>,
    pub metatypes: Vec<AMetatype>,
    pub metatype_impls: Vec<AMetatypeImpl>,
}

pub type LocalId = usize;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId {
    pub module_id: ModuleId,
    pub id: usize,
}

pub type ModuleId = u128;
pub type DataId = GlobalId;
pub type FunctionId = GlobalId;
pub type FunctionIdLocal = LocalId;
pub type MetatypeId = GlobalId;
pub type WhereId = LocalId;
pub type WhereIdGlobal = GlobalId;

pub struct AData {
    pub where_id: WhereId,
    pub fields: ADataFields,
}
pub enum ADataFields {
    Union { variants: Vec<(String, AType)> },
    IntersectionNamed { fields: Vec<(String, AType)> },
    IntersectionOrdered { fields: Vec<AType> },
}
pub struct AFunction {
    pub where_id: WhereId,
    pub args_ty: Vec<AType>,
    pub return_ty: AType,
    pub body: Option<ABody>,
}

pub struct AMetatype {
    /// First constraint should be this metatype, and it should bind all
    /// the metavars it creates and in strict ascending order without skips.
    /// (this makes the functions behave as expected.)
    pub where_id: WhereId,
    pub fns: Vec<FunctionIdLocal>,
}
pub struct AMetatypeImpl {
    /// First constraint is the metatype to implement.
    pub where_id: WhereId,
    pub fns: Vec<Option<FunctionIdLocal>>,
}
pub struct AWhere {
    /// outer scope where clause to concatonate before this
    pub parent_id: Option<WhereId>,
    pub n_vars: usize,
    pub constraints: Vec<(MetatypeId, Vec<AType>)>,
}
pub enum AType {
    Data {
        data_id: DataId,
        bindings: Vec<AType>,
    },
    FunctionPointer {
        args: Vec<AType>,
        ret: Box<AType>,
    },
    Metavar {
        where_id: WhereIdGlobal,
        var_id: usize,
    },
    Reference {
        inner_data: Box<AType>,
    },
    Error {},
}
pub struct ABody {
    /// Note: the first locals are bound to function arguments.
    pub locals: Vec<AType>,
    pub expr: AExpr,
}
pub struct AExpr {
    pub ret_ty: AType,
    pub eval: AExprEval,
}
pub enum AExprEval {
    Literal {
        value: AExprLiteral,
    },
    CallFunction {
        function_id: FunctionId,
        arguments: Vec<AExpr>,
    },
    CallMetatypeFunction {
        metatype_id: MetatypeId,
        function_id: usize,
        arguments: Vec<AExpr>,
    },
    Block {
        inner: Vec<AExpr>,
    },
    LocalRef {
        local_ref_id: usize,
    },
    Deref {
        reference: Box<AExpr>,
    },
    Assign {
        receiver: AExprPattern,
        value: Box<AExpr>,
    },
    DataInit {
        data_id: Option<DataId>,
        value: AExprDataInitContent,
    },
    DataAccess {
        value: Box<AExpr>,
        field: usize,
    },
    Return {
        value: Box<AExpr>,
    },
    // TODO: branch, loop
}
pub enum AExprLiteral {
    FunctionRef {
        function_id: FunctionId,
    },
    MetatypeFunctionRef {
        metatype_id: MetatypeId,
        function_id: usize,
    },
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Bool(bool),
}
pub enum AExprDataInitContent {
    Union {
        variant_id: usize,
        content: Box<AExpr>,
    },
    Intersection {
        fields: Vec<AExpr>,
    },
}
pub enum AExprPattern {
    Void,
    Literal {
        literal: AExprLiteral,
    },
    VarConstrain {
        local_ref_id: usize,
    },
    VarBind {
        local_ref_id: usize,
    },
    Union {
        variant_id: usize,
        sub_pat: Box<AExprPattern>,
    },
    Intersection {
        fields: Vec<AExprPattern>,
    },
}
