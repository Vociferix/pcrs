// TODO: Switch to `core::ascii::Char` if it ever gets stablized

use crate::{Failure, Input, PResult, Success};
use ascii::{AsciiChar, ToAsciiChar};

pub trait AsciiSymbol {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = Self>;
}

pub trait AsciiInput: Input {
    fn parse_char(self) -> PResult<AsciiChar, Self>;
}

pub fn char<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    I::parse_char(input)
}

impl<I> AsciiInput for I
where
    I: Input,
    I::Symbol: AsciiSymbol,
{
    fn parse_char(self) -> PResult<AsciiChar, Self> {
        <I::Symbol as AsciiSymbol>::parse_char(self)
    }
}

impl AsciiSymbol for AsciiChar {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = AsciiChar>,
    {
        use crate::basic::pop;
        pop(input)
    }
}

impl AsciiSymbol for char {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = char>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i8 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = i8>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for u8 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = u8>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i16 {
    fn parse_char<I>(mut input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = i16>,
    {
        let orig_input = input.clone();
        let Some(ch) = input.next() else {
            return Err(Failure(orig_input));
        };
        let Ok(ch) = (ch as u16).to_ascii_char() else {
            return Err(Failure(orig_input));
        };
        Ok(Success(ch, input))
    }
}

impl AsciiSymbol for u16 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = u16>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i32 {
    fn parse_char<I>(mut input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = i32>,
    {
        let orig_input = input.clone();
        let Some(ch) = input.next() else {
            return Err(Failure(orig_input));
        };
        let Ok(ch) = (ch as u32).to_ascii_char() else {
            return Err(Failure(orig_input));
        };
        Ok(Success(ch, input))
    }
}

impl AsciiSymbol for u32 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I>
    where
        I: Input<Symbol = u32>,
    {
        ascii_char(input)
    }
}

fn ascii_char<I>(mut input: I) -> PResult<AsciiChar, I>
where
    I: Input,
    I::Symbol: ToAsciiChar,
{
    let orig_input = input.clone();
    let Some(ch) = input.next() else {
        return Err(Failure(orig_input));
    };
    let Ok(ch) = ch.to_ascii_char() else {
        return Err(Failure(orig_input));
    };
    Ok(Success(ch, input))
}
