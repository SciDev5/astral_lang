use nom::{branch::alt, bytes::complete::tag, combinator::opt, IResult, Parser};

use crate::parse::{
    ast::ASTType,
    parsers_util::{bracket_many_comma_sep, ident, sep, BracketType},
    State,
};

pub fn type_annotation(state: State) -> IResult<State, ASTType> {
    (sep, tag(":"), type_expr).map(|(_, _, ty)| ty).parse(state)
}

pub fn type_expr(state: State) -> IResult<State, ASTType> {
    alt((type_expr_data,)).parse(state)
}

fn type_expr_data(state: State) -> IResult<State, ASTType> {
    (
        sep,
        ident,
        sep,
        opt(bracket_many_comma_sep(BracketType::Square, type_expr)),
    )
        .map(|(_, ident, _, args)| ASTType::Data {
            name: ident.as_inner().to_string(),
            arguments: args.unwrap_or(Vec::new()),
        })
        .parse(state)
}
