use std::rc::Rc;

use loc::LocatedSpan;
use nom::{
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::one_of,
    combinator::opt,
    multi::many,
    IResult, Parser,
};

mod loc;

type Span<'a> = LocatedSpan<&'a str>;
enum ASTLiteral<'a> {
    Int(Result<u128, Span<'a>>),
    Float(Result<f64, Span<'a>>),
    Bool(bool),
}

fn literal(src: Span) -> IResult<Span, ASTLiteral> {
    fn literal_int(src: Span) -> IResult<Span, Result<u128, Span>> {
        take_while1(|ch: char| ch.is_ascii_digit())
            .map_res(|v: Span| v.as_inner().parse::<u128>().map(Ok).map_err(|_| v))
            .parse(src)
    }
    fn literal_float(src: Span) -> IResult<Span, Result<f64, Span>> {
        (
            take_while(|ch: char| ch.is_ascii_digit()),
            tag::<&'static str, Span, _>("."),
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
    fn literal_bool(src: Span) -> IResult<Span, bool> {
        tag("true")
            .map(|_| true)
            .or(tag("false").map(|_| false))
            .parse(src)
    }

    literal_int
        .map(ASTLiteral::Int)
        .or(literal_float.map(ASTLiteral::Float))
        .or(literal_bool.map(ASTLiteral::Bool))
        .parse(src)
}

fn ident(src: Span) -> IResult<Span, Span> {
    take_while1(|ch: char| matches!(ch,'a'..='z'|'A'..='Z'|'0'..='9'|'_'))
        .map_res(|v: Span| {
            if v.as_inner().starts_with(|ch| matches!(ch, '0'..='9')) {
                Err(())
            } else {
                Ok(v)
            }
        })
        .parse(src)
}

fn sep(src: Span) -> IResult<Span, Vec<Span>> {
    fn space(src: Span) -> IResult<Span, ()> {
        take_while(|ch: char| ch.is_whitespace())
            .map(|_| ())
            .parse(src)
    }

    fn comment(src: Span) -> IResult<Span, Option<Span>> {
        fn comment_inline(src: Span) -> IResult<Span, Option<Span>> {
            (tag("//"), opt(tag("/")), take_until("\n"))
                .map(|(_, slash_opt, content)| slash_opt.map(|_| content))
                .parse(src)
        }
        fn comment_multiline(src: Span) -> IResult<Span, Option<Span>> {
            (tag("/*"), opt(tag("*")), take_until("*/"), tag("*/"))
                .map(|(_, slash_opt, content, _)| slash_opt.map(|_| content))
                .parse(src)
        }
        comment_inline(src).or_else(|_| comment_multiline(src))
    }

    (many(.., (space, comment)), space)
        .map(|(c, _): (Vec<((), Option<Span>)>, ())| c.into_iter().filter_map(|(_, v)| v).collect())
        .parse(src)
}

fn expr(src: Span) -> IResult<Span, ()> {
    todo!()
}
fn expr_else_block(src: Span) -> IResult<Span, ()> {
    (sep, tag("else"), opt(expr_block))
        .map(|_| todo!())
        .parse(src)
}
fn expr_if(src: Span) -> IResult<Span, ()> {
    (sep, tag("if"), opt(expr), opt(expr_block))
        .map(|_| todo!())
        .parse(src)
}
fn expr_block(src: Span) -> IResult<Span, ()> {
    todo!("add more state grah")
}

fn main() {
    _ = dbg!(
        sep(Span::from_inner("  /**hellowowow*/ /// the \n 20131hello")).map(|v| v
            .1
            .into_iter()
            .map(|v| v.as_inner())
            .collect::<Vec<_>>())
    );
}
