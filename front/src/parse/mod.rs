//! Module :: frontend/parser
//!
//! Responsible for transforming source files into an AST.
//!
//! PARSER CONVENTIONS:
//!  - parsers may not parse trailing whitespace, only leading

use nom::{multi::many0, IResult, Parser};
use state::LocatedSpan;

use crate::parse::{ast::ASTTopLevel, parsers_static::def_static, parsers_util::BracketState};

pub mod ast;
pub mod loc;
mod parsers_expr;
mod parsers_static;
mod parsers_type;
mod parsers_util;
mod state;

type Span<'a> = LocatedSpan<&'a str, ()>;
type State<'a> = LocatedSpan<&'a str, StateData>;

#[derive(Debug, Clone, Copy)]
struct StateData {
    bracket: BracketState,
}

fn top_level(state: State) -> IResult<State, ASTTopLevel> {
    many0(def_static)
        .map(|defs| ASTTopLevel { scope: None, defs })
        .parse(state)
}

pub fn parse_top_level(file: &str) -> ASTTopLevel {
    top_level(State::from_inner(
        file,
        StateData {
            bracket: BracketState::new(),
        },
    ))
    .unwrap()
    .1
}
