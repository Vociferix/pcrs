use pcrs::{
    basic::{
        all_consuming, alt, delimited, flat_map, many0, many1, map, prefix, seq, suffix, verify,
        with_value,
    },
    compile,
    unicode::{char as uchar, Error, PResult, UnicodeInput as UInput},
    PResultExt, Parse,
};

fn ws<I: UInput>(input: I) -> PResult<(), I> {
    compile!(many0(verify(uchar, |ch: &char| ch.is_whitespace()), |_| ()))(input)
}

const fn ws_delim<P, I>(parser: P) -> impl Fn(I) -> PResult<P::Parsed, I>
where
    P: Parse<I, Error = Error<I>>,
    I: UInput,
{
    let p = prefix(ws, parser);
    move |input| p.parse(input)
}

fn digit<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(map(
        verify(uchar, |ch: &char| *ch >= '0' && *ch <= '9'),
        |ch| { ((ch as u32 as u8) - b'0') as i64 }
    ))(input)
}

fn number<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(ws_delim(many1(digit, |iter| -> i64 {
        let mut ret = 0i64;
        for d in iter {
            ret = (ret * 10) + d;
        }
        ret
    })))(input)
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Op {
    fn calc(self, lhs: i64, rhs: i64) -> i64 {
        match self {
            Op::Add => lhs + rhs,
            Op::Sub => lhs - rhs,
            Op::Mul => lhs * rhs,
            Op::Div => lhs / rhs,
            Op::Mod => lhs % rhs,
        }
    }
}

fn plus<I: UInput>(input: I) -> PResult<Op, I> {
    compile!(ws_delim(with_value(
        verify(uchar, |ch| *ch == '+'),
        Op::Add
    )))(input)
}

fn minus<I: UInput>(input: I) -> PResult<Op, I> {
    compile!(ws_delim(with_value(
        verify(uchar, |ch| *ch == '-'),
        Op::Sub
    )))(input)
}

fn mult<I: UInput>(input: I) -> PResult<Op, I> {
    compile!(ws_delim(with_value(
        verify(uchar, |ch| *ch == '*'),
        Op::Mul
    )))(input)
}

fn div<I: UInput>(input: I) -> PResult<Op, I> {
    compile!(ws_delim(with_value(
        verify(uchar, |ch| *ch == '/'),
        Op::Div
    )))(input)
}

fn modulus<I: UInput>(input: I) -> PResult<Op, I> {
    compile!(ws_delim(with_value(
        verify(uchar, |ch| *ch == '%'),
        Op::Mod
    )))(input)
}

fn lparen<I: UInput>(input: I) -> PResult<(), I> {
    compile!(ws_delim(with_value(verify(uchar, |ch| *ch == '('), ())))(input)
}

fn rparen<I: UInput>(input: I) -> PResult<(), I> {
    compile!(ws_delim(with_value(verify(uchar, |ch| *ch == ')'), ())))(input)
}

fn primary_expr<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(alt!(number, delimited(lparen, expr, rparen)))(input)
}

fn unary_expr<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(alt!(
        prefix(plus, unary_expr),
        map(prefix(minus, unary_expr), |val| -val),
        primary_expr,
    ))(input)
}

fn term_expr<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(flat_map(unary_expr, |init: i64| {
        many0(seq!(alt!(mult, div, modulus), unary_expr), move |iter| {
            let mut ret = init;
            for (op, val) in iter {
                ret = op.calc(ret, val);
            }
            ret
        })
    }))(input)
}

fn expr<I: UInput>(input: I) -> PResult<i64, I> {
    compile!(flat_map(term_expr, |init: i64| {
        many0(seq!(alt!(plus, minus), term_expr), move |iter| {
            let mut ret = init;
            for (op, val) in iter {
                ret = op.calc(ret, val);
            }
            ret
        })
    }))(input)
}

fn eval<I: UInput>(input: I) -> Result<i64, Error<I>> {
    compile!(all_consuming(suffix(expr, ws)))(input).extract().0
}

fn main() -> std::io::Result<()> {
    use std::io::BufRead;
    use std::io::Write;

    let mut out = std::io::stdout();
    write!(out, "Enter 'q', 'quit', or 'exit' to exit.\n")?;
    write!(out, "> ")?;
    out.flush()?;

    for input in std::io::stdin().lock().lines() {
        let input = input?;

        if input == "q" || input == "quit" || input == "exit" {
            break;
        }

        if input.trim().is_empty() {
            write!(out, "> ")?;
            out.flush()?;
            continue;
        }

        if let Ok(value) = eval(&input[..]) {
            write!(out, "{value}\n")?;
        } else {
            write!(out, "invalid expression\n")?;
        }

        write!(out, "> ")?;
        out.flush()?;
    }

    Ok(())
}
