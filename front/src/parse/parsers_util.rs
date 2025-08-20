use crate::parse::{ast::ASTSep, state::LocatedSpan, Span, State, StateData};
use nom::{
    bytes::complete::{tag, take_until, take_while, take_while1},
    combinator::opt,
    multi::{many, many0},
    IResult, Parser,
};

// ---- brackets ---- //

#[derive(Debug, Clone, Copy)]
pub struct BracketState {
    level: u16,
    curly: u16,
    square: u16,
    paren: u16,
}
impl BracketState {
    pub fn new() -> Self {
        Self {
            level: 10,
            curly: 0,
            square: 0,
            paren: 0,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum BracketType {
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

pub fn bracket<'a, T>(
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

// ---- misc parsers ---- //

pub fn sep(src: State) -> IResult<State, ASTSep> {
    fn space(src: State) -> IResult<State, bool> {
        take_while(|ch: char| ch.is_whitespace())
            .map(|text: State| text.as_inner().contains("\n"))
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
        .map(|(c, mut has_newline): (Vec<(bool, Option<State>)>, bool)| {
            let doc_comments = c
                .into_iter()
                .filter_map(|(section_has_newline, comment)| {
                    has_newline |= section_has_newline;
                    comment.map(|_| ())
                })
                .collect::<Vec<_>>();
            ASTSep {
                doc_comments,
                has_newline,
            }
        })
        .parse(src)
}

pub fn ident(state: State) -> IResult<State, Span> {
    take_while1(|ch: char| ch == '_' || ch.is_ascii_alphanumeric())
        .map_opt(|state: State| {
            if state
                .as_inner()
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_digit())
            {
                None
            } else {
                Some(state.as_span())
            }
        })
        .parse(state)
}

// ---- misc utilities ---- //

pub fn span_of<'a, T, E: nom::error::ParseError<State<'a>>>(
    state: State<'a>,
    parser: impl Parser<State<'a>, Output = State<'a>, Error = E>,
) -> IResult<State<'a>, Span<'a>, E> {
    let span_start = state.as_start();
    parser
        .map(|span_last| span_last.as_span().merge(span_start))
        .parse(state)
}

pub fn bracket_many_comma_sep<'a, T>(
    bracket_ty: BracketType,
    inner: impl Parser<
        State<'a>,
        Output = T,
        Error = nom::error::Error<LocatedSpan<&'a str, StateData>>,
    >,
) -> impl Parser<State<'a>, Output = Vec<T>, Error = nom::error::Error<LocatedSpan<&'a str, StateData>>>
{
    bracket(bracket_ty, many0((inner, sep, opt(tag(","))))).map(|(_, inner, extra, closing)| {
        if extra.is_some() || closing.is_none() {
            todo!("bracket_many_comma_sep extra before close or no close")
        }
        if !inner
            .iter()
            .take(inner.len().saturating_sub(1))
            .all(|(_, _, v)| v.is_some())
        {
            todo!("bracket_many_comma_sep: missing commas")
        }
        inner.into_iter().map(|(v, _, _)| v).collect()
    })
}

pub fn many_flexible<'a, T>(
    inner: impl Parser<
        State<'a>,
        Output = T,
        Error = nom::error::Error<LocatedSpan<&'a str, StateData>>,
    >,
) -> impl Parser<State<'a>, Output = Vec<T>, Error = nom::error::Error<LocatedSpan<&'a str, StateData>>>
{
    many0(inner) // todo add the flexibility (ie error recovery)
}
