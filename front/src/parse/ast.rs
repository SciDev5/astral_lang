use crate::{
    parse::loc::FileLoc,
    post::{DataId, FunctionId, MetatypeId},
};

// pub struct ASTMappedRegion<T> {
//     pub region: FileLoc,
//     pub data: T,
// }

pub struct ASTTopLevel {
    pub scope: Option<usize>,
    pub defs: Vec<ASTStatic>,
}

pub enum ASTOperatorBinary {
    Add,
    Sub,
}
pub enum ASTOperatorUnary {
    Negate,
}

pub enum ASTExpr {
    Error {},
    LetTEMP {
        var_name: String,
        ty: Option<ASTType>,
        expr: Box<ASTExpr>,
    },
    Ident {
        var_name: String,
    },
    Access {
        name: String,
        from: Box<ASTExpr>,
    },
    Call {
        callee: Box<ASTExpr>,
        arguments: Vec<ASTExpr>,
    },
    Block {
        scope: Option<usize>,
        exprs: Vec<ASTExpr>,
    },
    OperatorBinary {
        expr_first: Box<ASTExpr>,
        expr_second: Box<ASTExpr>,
        op: ASTOperatorUnary,
    },
    OperatorUnary {
        expr: Box<ASTExpr>,
        op: ASTOperatorUnary,
    },
    Static(ASTStatic),
    NoOp,
}
pub enum ASTType {
    Data {
        name: String,
        arguments: Vec<ASTType>,
    },
}
pub enum ASTStatic {
    Function(ASTFunction),
    Data(ASTData),
    Metatype(ASTMetatype),
    MetatypeImpl(ASTMetatypeImpl),
    Import(ASTImportTree),
}

pub struct ASTFunction {
    pub name: String,
    pub arguments: Vec<(String, Option<ASTType>)>,
    pub return_ty: Option<ASTType>,
    pub where_ty: ASTWhere,
    pub body: Box<ASTExpr>,
}
pub struct ASTData {
    pub name: String,
}
pub struct ASTMetatype {
    pub name: String,
}
pub struct ASTMetatypeImpl {
    pub name: String,
}
pub struct ASTWhere {
    // TODO
}
pub struct ASTImportTree {
    // TODO
}

pub struct ASTSep {
    pub has_newline: bool,
    pub doc_comments: Vec<()>, // TODO: doc comments
}
