use nom::{branch::alt, bytes::complete::tag, combinator::opt, IResult, Parser};

use crate::parse::{
    ast::{ASTFunction, ASTStatic, ASTWhere},
    parsers_expr::expr,
    parsers_type::type_annotation,
    parsers_util::{bracket_many_comma_sep, ident, sep, BracketType},
    State,
};

/// Parse a static definiton
pub fn def_static(state: State) -> IResult<State, ASTStatic> {
    // todo: parse other types of static definitions
    // alt((def_data, def_function)).parse(state)
    def_function(state)
}

fn def_data(state: State) -> IResult<State, ASTStatic> {
    todo!()
}

fn def_function(state: State) -> IResult<State, ASTStatic> {
    (
        sep,
        tag("fn"),
        sep,
        ident.map(|name| name.as_inner().to_string()),
        sep,
        bracket_many_comma_sep(
            BracketType::Paren,
            (sep, ident, opt(type_annotation))
                .map(|(_, name, ty)| (name.as_inner().to_string(), ty)),
        ),
        opt(type_annotation),
        sep,
        tag("="),
        expr.map(Box::new),
    )
        .map(
            |(_, _, _, name, _, arguments, return_ty, _, _, body)| ASTFunction {
                name,
                arguments,
                return_ty,
                where_ty: ASTWhere {},
                body,
            },
        )
        .map(ASTStatic::Function)
        .parse(state)
}
