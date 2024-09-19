// TODO: Switch to `core::ascii::Char` if it ever gets stablized

use crate::{Error as PError, Failure, Input, PResult, Parse, Success};
use ascii::{AsciiChar, ToAsciiChar};
use core::marker::PhantomData;

pub mod prop;

#[derive(Debug, Clone)]
pub enum Error<I: Input> {
    NeedMoreInput,
    ExpectedEof(I),
    InvalidInput(I),
    NonAsciiChar(I),
}

pub trait AsciiSymbol {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = Self>;
}

pub trait AsciiInput: Input {
    fn parse_char(self) -> PResult<AsciiChar, Self, Error<Self>>;
}

pub fn char<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    I::parse_char(input)
}

pub struct VerbatimParser<P, I>(P, PhantomData<I>)
where
    P: AsciiInput,
    I: AsciiInput;

impl<P, I> Parse<I> for VerbatimParser<P, I>
where
    P: AsciiInput,
    I: AsciiInput,
{
    type Parsed = super::Span<I>;
    type Error = Error<I>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let mut expected = self.0.clone();
        let mut rem = input.clone();
        loop {
            match expected.parse_char() {
                Ok(Success(ex, expected_new)) => {
                    expected = expected_new;
                    let tmp = rem.clone();
                    match rem.parse_char() {
                        Ok(Success(ch, rem_new)) => {
                            if ch != ex {
                                return Err(Failure(PError::invalid_input(tmp), input));
                            }
                            rem = rem_new;
                        }
                        Err(Failure(err, _)) => {
                            return Err(Failure(err, input));
                        }
                    }
                }
                Err(Failure(_, expected)) => {
                    if expected.is_empty() {
                        break;
                    } else {
                        return Err(Failure(PError::invalid_input(rem.clone()), input));
                    }
                }
            }
        }
        Ok(Success(super::Span::new(input, rem.clone()), rem))
    }
}

pub const fn verbatim<P, I>(pattern: P) -> VerbatimParser<P, I>
where
    P: AsciiInput,
    I: AsciiInput,
{
    VerbatimParser(pattern, PhantomData)
}

pub trait Property: core::fmt::Debug + Copy {
    fn contains(self, ch: AsciiChar) -> bool;
}

#[derive(Debug, Clone)]
pub struct CharWithPropParser<P, I>(P, PhantomData<I>)
where
    P: Property,
    I: AsciiInput;

impl<P, I> Parse<I> for CharWithPropParser<P, I>
where
    P: Property,
    I: AsciiInput,
{
    type Parsed = AsciiChar;
    type Error = Error<I>;

    fn parse(&self, input: I) -> PResult<AsciiChar, I, Self::Error> {
        match input.clone().parse_char() {
            Ok(Success(ch, rem)) if self.0.contains(ch) => Ok(Success(ch, rem)),
            Ok(_) => Err(Failure(PError::invalid_input(input.clone()), input)),
            Err(Failure(err, _)) => Err(Failure(err, input)),
        }
    }
}

pub const fn char_with_prop<P, I>(property: P) -> CharWithPropParser<P, I>
where
    P: Property,
    I: AsciiInput,
{
    CharWithPropParser(property, PhantomData)
}

pub fn alphabetic<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Alphabetic) }.parse(input)
}

pub fn alphanumeric<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Alphanumeric) }.parse(input)
}

pub fn blank<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Blank) }.parse(input)
}

pub fn control<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Control) }.parse(input)
}

pub fn digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Digit) }.parse(input)
}

pub fn graphic<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Graphic) }.parse(input)
}

pub fn hex_digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::HexDigit) }.parse(input)
}

pub fn lowercase<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Lowercase) }.parse(input)
}

pub fn oct_digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::OctDigit) }.parse(input)
}

pub fn printable<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Printable) }.parse(input)
}

pub fn punctuation<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Punctuation) }.parse(input)
}

pub fn uppercase<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Uppercase) }.parse(input)
}

pub fn whitespace<I: AsciiInput>(input: I) -> PResult<AsciiChar, I, Error<I>> {
    const { &char_with_prop(prop::Whitespace) }.parse(input)
}

impl<I: Input> crate::Error<I> for Error<I> {
    fn need_more_input() -> Self {
        Self::NeedMoreInput
    }

    fn expected_eof(pos: I) -> Self {
        Self::ExpectedEof(pos)
    }

    fn invalid_input(pos: I) -> Self {
        Self::InvalidInput(pos)
    }
}

impl<I> AsciiInput for I
where
    I: Input,
    I::Symbol: AsciiSymbol,
{
    fn parse_char(self) -> PResult<AsciiChar, Self, Error<Self>> {
        <I::Symbol as AsciiSymbol>::parse_char(self)
    }
}

impl AsciiSymbol for AsciiChar {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = AsciiChar>,
    {
        use crate::basic::pop;
        pop(input)
    }
}

impl AsciiSymbol for char {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = char>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i8 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = i8>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for u8 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = u8>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i16 {
    fn parse_char<I>(mut input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = i16>,
    {
        let orig_input = input.clone();
        let Some(ch) = input.next() else {
            return Err(Failure(Error::NeedMoreInput, orig_input));
        };
        let Ok(ch) = (ch as u16).to_ascii_char() else {
            return Err(Failure(Error::NonAsciiChar(orig_input.clone()), orig_input));
        };
        Ok(Success(ch, input))
    }
}

impl AsciiSymbol for u16 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = u16>,
    {
        ascii_char(input)
    }
}

impl AsciiSymbol for i32 {
    fn parse_char<I>(mut input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = i32>,
    {
        let orig_input = input.clone();
        let Some(ch) = input.next() else {
            return Err(Failure(Error::NeedMoreInput, orig_input));
        };
        let Ok(ch) = (ch as u32).to_ascii_char() else {
            return Err(Failure(Error::NonAsciiChar(orig_input.clone()), orig_input));
        };
        Ok(Success(ch, input))
    }
}

impl AsciiSymbol for u32 {
    fn parse_char<I>(input: I) -> PResult<AsciiChar, I, Error<I>>
    where
        I: Input<Symbol = u32>,
    {
        ascii_char(input)
    }
}

fn ascii_char<I>(mut input: I) -> PResult<AsciiChar, I, Error<I>>
where
    I: Input,
    I::Symbol: ToAsciiChar,
{
    let orig_input = input.clone();
    let Some(ch) = input.next() else {
        return Err(Failure(Error::NeedMoreInput, orig_input));
    };
    let Ok(ch) = ch.to_ascii_char() else {
        return Err(Failure(Error::NonAsciiChar(orig_input.clone()), orig_input));
    };
    Ok(Success(ch, input))
}
