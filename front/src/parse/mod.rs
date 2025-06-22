//! Module :: frontend/parser
//!
//! Responsible for transforming source files into an AST.

use std::rc::Rc;

use loc::FileLoc;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::one_of,
    combinator::opt,
    multi::many,
    IResult, Parser,
};
use state::LocatedSpan;

pub mod ast;
pub mod loc;
mod state;

type Span<'a> = LocatedSpan<&'a str, ()>;
type State<'a> = LocatedSpan<&'a str, StateData>;

#[derive(Debug, Clone, Copy)]
struct StateData {
    bracket: BracketState,
}

type One<T> = Rc<T>;
type Many<T> = Rc<[T]>;

#[derive(Debug, Clone, Copy)]
struct BracketState {
    level: u16,
    curly: u16,
    square: u16,
    paren: u16,
}
#[derive(Debug, Clone, Copy)]
enum BracketType {
    Curly,
    Square,
    Paren,
}
impl BracketType {
    fn str(self) -> (&'static str, &'static str) {
        match self {
            Self::Curly => ("{", "}"),
            Self::Square => ("[", "]"),
            Self::Paren => ("(", ")"),
        }
    }
    fn apply_to(self, mut state: BracketState) -> BracketState {
        state.level += 1;
        match self {
            Self::Curly => state.curly = state.level,
            Self::Square => state.square = state.level,
            Self::Paren => state.paren = state.level,
        }
        state
    }
}

fn not_strong_bracket(ch: char, state: BracketState) -> bool {
    const STRONG_LEVELS: u16 = 2;
    dbg!(state);
    match ch {
        '}' if state.curly + STRONG_LEVELS >= state.level => false,
        ']' if state.square + STRONG_LEVELS >= state.level => false,
        ')' if state.paren + STRONG_LEVELS >= state.level => false,
        _ => true,
    }
}
fn match_strong_bracket(src: State) -> IResult<State, ()> {
    take_while1(|ch: char| !not_strong_bracket(ch, src.inner_state().bracket))
        .map(|_| ())
        .parse(src)
}

fn bracket<'a, T>(
    ty: BracketType,
    mut inner: impl Parser<
        State<'a>,
        Output = T,
        Error = nom::error::Error<LocatedSpan<&'a str, StateData>>,
    >,
) -> impl Parser<
    State<'a>,
    Output = (Span<'a>, T, Option<Span<'a>>, Option<Span<'a>>),
    Error = nom::error::Error<LocatedSpan<&'a str, StateData>>,
> {
    fn brackets_inner<'a, T>(
        src: State<'a>,
        ty: BracketType,
        inner: &mut impl Parser<
            State<'a>,
            Output = T,
            Error = nom::error::Error<LocatedSpan<&'a str, StateData>>,
        >,
    ) -> IResult<State<'a>, (Span<'a>, T, Option<Span<'a>>, Option<Span<'a>>)> {
        // this parser functioning properly is dependent on all other parsers only
        // consuming closing brackets under the conditions this parser expects.
        let (b0, b1) = ty.str();
        let bracket_state_before = src.inner_state().bracket;
        let bracket_state = ty.apply_to(bracket_state_before);
        (
            tag::<&'static str, State, _>(b0),
            |s: State<'a>| inner.parse(s),
            opt(take_while1(move |ch: char| {
                dbg!(ch);
                dbg!(not_strong_bracket(ch, bracket_state))
            })),
            opt(tag(b1)),
        )
            .map(|(open, value, extra, close)| {
                (
                    open.as_span(),
                    value,
                    extra.map(|v| v.as_span()),
                    close.map(|v| v.as_span()),
                )
            })
            .parse(src.map_state(|mut s| {
                s.bracket = bracket_state;
                s
            }))
            .map(|(s, v)| {
                (
                    s.map_state(|mut s| {
                        s.bracket = bracket_state_before;
                        s
                    }),
                    v,
                )
            })
    }
    move |src: State<'a>| brackets_inner(src, ty, &mut inner)
}

fn literal(src: State) -> IResult<State, ASTLiteral> {
    fn literal_int(src: State) -> IResult<State, Result<u128, State>> {
        take_while1(|ch: char| ch.is_ascii_digit())
            .map_res(|v: State| v.as_inner().parse::<u128>().map(Ok).map_err(|_| v))
            .parse(src)
    }
    fn literal_float(src: State) -> IResult<State, Result<f64, State>> {
        (
            take_while(|ch: char| ch.is_ascii_digit()),
            tag::<&'static str, State, _>("."),
            take_while1(|ch: char| ch.is_ascii_digit()),
            opt((
                one_of("eE"),
                opt(one_of("+-")),
                take_while(|ch: char| ch.is_ascii_digit()),
            )),
        )
            .map_res(|(int, _, frac, exp)| {
                let v = int.merge(exp.map(|(_, _, exponent)| exponent).unwrap_or(frac));
                v.as_inner().parse::<f64>().map(Ok).map_err(|_| v)
            })
            .parse(src)
    }
    fn literal_bool(src: State) -> IResult<State, bool> {
        tag("true")
            .map(|_| true)
            .or(tag("false").map(|_| false))
            .parse(src)
    }

    literal_int
        .map(|v| ASTLiteral::Int(v.map_err(|v| v.as_span())))
        .or(literal_float.map(|v| ASTLiteral::Float(v.map_err(|v| v.as_span()))))
        .or(literal_bool.map(ASTLiteral::Bool))
        .parse(src)
}

fn ident(src: State) -> IResult<State, State> {
    take_while1(|ch: char| matches!(ch,'a'..='z'|'A'..='Z'|'0'..='9'|'_'))
        .map_res(|v: State| {
            if v.as_inner().starts_with(|ch| matches!(ch, '0'..='9')) {
                Err(())
            } else {
                Ok(v)
            }
        })
        .parse(src)
}

fn sep(src: State) -> IResult<State, Vec<State>> {
    fn space(src: State) -> IResult<State, ()> {
        take_while(|ch: char| ch.is_whitespace())
            .map(|_| ())
            .parse(src)
    }

    fn comment(src: State) -> IResult<State, Option<State>> {
        fn comment_inline(src: State) -> IResult<State, Option<State>> {
            (tag("//"), opt(tag("/")), take_until("\n"))
                .map(|(_, slash_opt, content)| slash_opt.map(|_| content))
                .parse(src)
        }
        fn comment_multiline(src: State) -> IResult<State, Option<State>> {
            (tag("/*"), opt(tag("*")), take_until("*/"), tag("*/"))
                .map(|(_, slash_opt, content, _)| slash_opt.map(|_| content))
                .parse(src)
        }
        comment_inline(src).or_else(|_| comment_multiline(src))
    }

    (many(.., (space, comment)), space)
        .map(|(c, _): (Vec<((), Option<State>)>, ())| {
            c.into_iter().filter_map(|(_, v)| v).collect()
        })
        .parse(src)
}

fn expr(src: State) -> IResult<State, ASTExpr> {
    alt((expr_if, expr_paren, expr_block, expr_ident)).parse(src)
}
fn expr_else_block(src: State) -> IResult<State, Option<One<ASTExpr>>> {
    (sep, tag("else"), opt(expr_block))
        .map(|_| todo!())
        .parse(src)
}
fn expr_if(src: State) -> IResult<State, ASTExpr> {
    (sep, tag("if"), opt(expr), opt(expr_block))
        .map(|_| todo!())
        .parse(src)
}
fn expr_block<'a>(src: State<'a>) -> IResult<State<'a>, ASTExpr> {
    bracket(BracketType::Curly, |src: State<'a>| {
        many(.., (sep, expr))
            .map(|v: Vec<(_, ASTExpr)>| {
                Many::<_>::from(v.into_iter().map(|(_, expr)| expr).collect::<Vec<_>>())
            })
            .parse(src)
    })
    .map(|(open, body, extra, close)| {
        dbg!(&extra, &close);

        let (loc, err_loc) = match (extra, close) {
            (None, Some(close)) => (open.loc().merge(close.loc()), None),

            (None, None) => {
                let loc = open.loc().merge_opt(body.last().map(|v| v.loc()));
                (loc, Some(loc.from_end()))
            }
            (Some(extra), None) => (
                open.loc().merge_opt(body.last().map(|v| v.loc())),
                Some(extra.loc()),
            ),
            (Some(extra), Some(close)) => (open.loc().merge(close.loc()), Some(extra.loc())),
        };
        ASTExpr::Block {
            loc,
            body,
            error: err_loc.map(|loc| {
                format!(
                    "!!! expected closing bracket [range {} to {}]",
                    loc.start_byte, loc.end_byte
                )
                .into()
            }),
        }
    })
    .parse(src)
}
fn expr_paren<'a>(src: State<'a>) -> IResult<State<'a>, ASTExpr> {
    bracket(BracketType::Paren, expr)
        .map(|(before, expr, extra, after)| todo!())
        .parse(src)
}
fn expr_ident<'a>(src: State<'a>) -> IResult<State<'a>, ASTExpr> {
    ident
        .map(|v| ASTExpr::Ident {
            loc: v.loc(),
            name: v.as_inner().into(),
        })
        .parse(src)
}

#[test]
fn was_main() {
    _ = dbg!(
        expr(Span::from_inner("{helo world}", ()).with_state(StateData {
            bracket: BracketState {
                level: 10000,
                curly: 0,
                square: 0,
                paren: 0
            }
        }))
        .map(|v| v.1)
    );
    todo!("make this into a test");
}

pub enum ASTLiteral<'a> {
    Int(Result<u128, Span<'a>>),
    Float(Result<f64, Span<'a>>),
    Bool(bool),
}

pub type ASTError = One<str>;
#[derive(Debug, Clone)]
pub enum ASTExpr {
    Error {
        loc: FileLoc,
        before: Many<ASTExpr>,
        error: ASTError,
    },
    If {
        loc: FileLoc,
        condition: One<ASTExpr>,
        body: One<ASTExpr>,
        else_block: Option<One<ASTExpr>>,
    },
    Ident {
        loc: FileLoc,
        name: One<str>,
    },
    Block {
        loc: FileLoc,
        body: Many<ASTExpr>,
        error: Option<ASTError>,
    },
}
impl ASTExpr {
    pub fn loc(&self) -> FileLoc {
        match self {
            Self::Error { loc, .. }
            | Self::If { loc, .. }
            | Self::Ident { loc, .. }
            | Self::Block { loc, .. } => *loc,
        }
    }
}
