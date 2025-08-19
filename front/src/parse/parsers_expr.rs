use nom::{branch::alt, bytes::complete::tag, combinator::opt, multi::many0, IResult, Parser};

use crate::parse::{
    ast::{ASTExpr, ASTSep},
    parsers_static::def_static,
    parsers_type::type_annotation,
    parsers_util::{bracket_many_comma_sep, ident, sep, BracketType},
    Span, State,
};

/// Parse expressions, including operators, but does not greedily consume expression pieces.
pub fn expr_thrifty(state: State) -> IResult<State, ASTExpr> {
    expr_outer::</* thrifty whitespace */ true>(state)
}
/// Parse expressions, including operators, greedily consuming valid expression pieces after newlines.
pub fn expr(state: State) -> IResult<State, ASTExpr> {
    expr_outer::</* greedy whitespace */ false>(state)
}

/// Parse expressions, including operators
pub fn expr_outer<const THRIFTY: bool>(state: State) -> IResult<State, ASTExpr> {
    // todo: operators
    expr_inner::<THRIFTY>(state)
}

/*
   s, i, p -> [s]uffix, [i]nfix, [p]refix

   examples:
   s  : $
   si : *
    i : ==
    ip: ~
     p: #

si i ip -> s i p
si si s ip -> s s s i
si ip si ip -> ERR, unresolvable
si ip -> s i / i p => ERR, indeterminate
si s ip ip ->  s s i p


*/

/// Parse expressions, except operators.
fn expr_inner<const THRIFTY: bool>(state: State) -> IResult<State, ASTExpr> {
    (
        alt((expr_let_TEMP, expr_name, expr_block, expr_static)),
        many0(alt((post_expr_call::<THRIFTY>, post_expr_dotaccess))),
    )
        .map(|(mut expr, post_exprs)| {
            for post_expr in post_exprs {
                expr = post_expr.apply(expr);
            }
            expr
        })
        .parse(state)
}

fn expr_let_TEMP(state: State) -> IResult<State, ASTExpr> {
    (
        sep,
        tag("let"),
        sep,
        ident,
        opt(type_annotation),
        sep,
        tag("="),
        expr,
    )
        .map(|(_, _, _, ident, ty, _, _, expr)| ASTExpr::LetTEMP {
            var_name: ident.as_inner().to_string(),
            ty,
            expr: Box::new(expr),
        })
        .parse(state)
}
fn expr_name(state: State) -> IResult<State, ASTExpr> {
    (sep, ident)
        .map(|(_, span): (ASTSep, Span)| ASTExpr::Ident {
            var_name: span.as_inner().to_string(),
        })
        .parse(state)
}
fn expr_block(state: State) -> IResult<State, ASTExpr> {
    (
        sep,
        bracket_many_comma_sep(BracketType::Curly, expr_thrifty),
    )
        .map(|(_, exprs)| ASTExpr::Block { scope: None, exprs })
        .parse(state)
}
fn expr_static(state: State) -> IResult<State, ASTExpr> {
    def_static.map(ASTExpr::Static).parse(state)
}

fn post_expr_dotaccess(state: State) -> IResult<State, PostExpr> {
    (sep, tag("."), sep, ident)
        .map(|(_, _, _, ident)| PostExpr::DotAccess(ident.as_inner().to_string()))
        .parse(state)
}
fn post_expr_call<const THRIFTY: bool>(state: State) -> IResult<State, PostExpr> {
    (
        sep.map_opt(|v| {
            if THRIFTY && v.has_newline {
                None
            } else {
                Some(v)
            }
        }),
        bracket_many_comma_sep(BracketType::Paren, expr),
    )
        .map(|(_, exprs)| PostExpr::Call(exprs))
        .parse(state)
}
enum PostExpr {
    DotAccess(String),
    Call(Vec<ASTExpr>),
}
impl PostExpr {
    fn apply(self, expr: ASTExpr) -> ASTExpr {
        match self {
            PostExpr::DotAccess(name) => ASTExpr::Access {
                name,
                from: Box::new(expr),
            },
            PostExpr::Call(arguments) => ASTExpr::Call {
                callee: Box::new(expr),
                arguments,
            },
        }
    }
}
