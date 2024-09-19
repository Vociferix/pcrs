use pcrs::{
    basic::{
        all_consuming, alt, delimited, flat_map, many0, many1, map, prefix, seq, suffix, verify,
        with_value,
    },
    unicode::{char as uchar, Error, UnicodeInput as UInput},
    PResult, PResultExt, Parse,
};

fn ws<I: UInput>(input: I) -> PResult<(), I, Error<I>> {
    const { &many0(verify(uchar, |ch: &char| ch.is_whitespace()), |_| ()) }.parse(input)
}

const fn ws_delim<P, I>(parser: P) -> impl Fn(I) -> PResult<P::Parsed, I, Error<I>>
where
    P: Parse<I, Error = Error<I>>,
    I: UInput,
{
    let p = prefix(ws, parser);
    move |input| p.parse(input)
}

fn digit<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const {
        &map(verify(uchar, |ch: &char| *ch >= '0' && *ch <= '9'), |ch| {
            ((ch as u32 as u8) - b'0') as i64
        })
    }
    .parse(input)
}

fn number<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const {
        &ws_delim(many1(digit, |iter| -> i64 {
            let mut ret = 0i64;
            for d in iter {
                ret = (ret * 10) + d;
            }
            ret
        }))
    }
    .parse(input)
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

fn plus<I: UInput>(input: I) -> PResult<Op, I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '+'), Op::Add)) }.parse(input)
}

fn minus<I: UInput>(input: I) -> PResult<Op, I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '-'), Op::Sub)) }.parse(input)
}

fn mult<I: UInput>(input: I) -> PResult<Op, I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '*'), Op::Mul)) }.parse(input)
}

fn div<I: UInput>(input: I) -> PResult<Op, I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '/'), Op::Div)) }.parse(input)
}

fn modulus<I: UInput>(input: I) -> PResult<Op, I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '%'), Op::Mod)) }.parse(input)
}

fn lparen<I: UInput>(input: I) -> PResult<(), I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == '('), ())) }.parse(input)
}

fn rparen<I: UInput>(input: I) -> PResult<(), I, Error<I>> {
    const { &ws_delim(with_value(verify(uchar, |ch| *ch == ')'), ())) }.parse(input)
}

fn primary_expr<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const { &alt!(number, delimited(lparen, expr, rparen)) }.parse(input)
}

fn unary_expr<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const {
        &alt!(
            prefix(plus, unary_expr),
            map(prefix(minus, unary_expr), |val| -val),
            primary_expr,
        )
    }
    .parse(input)
}

fn term_expr<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const {
        &flat_map(unary_expr, |init: i64| {
            many0(seq!(alt!(mult, div, modulus), unary_expr), move |iter| {
                let mut ret = init;
                for (op, val) in iter {
                    ret = op.calc(ret, val);
                }
                ret
            })
        })
    }
    .parse(input)
}

fn expr<I: UInput>(input: I) -> PResult<i64, I, Error<I>> {
    const {
        &flat_map(term_expr, |init: i64| {
            many0(seq!(alt!(plus, minus), term_expr), move |iter| {
                let mut ret = init;
                for (op, val) in iter {
                    ret = op.calc(ret, val);
                }
                ret
            })
        })
    }
    .parse(input)
}

fn eval<I: UInput>(input: I) -> Result<i64, Error<I>> {
    const { &all_consuming(suffix(expr, ws)) }
        .parse(input)
        .extract()
        .0
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
