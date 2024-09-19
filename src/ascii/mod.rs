// TODO: Switch to `core::ascii::Char` if it ever gets stablized

use crate::{compile, Error as PError, Failure, Input, Parse, Success};
use ascii::{AsciiChar, ToAsciiChar};
use core::marker::PhantomData;

pub mod prop;

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind {
    NeedMoreInput,
    ExpectedEof,
    InvalidInput,
    NonAsciiChar,
}

#[derive(Debug, Clone)]
pub struct Error<I: Input> {
    kind: ErrorKind,
    pos: I,
}

pub type PResult<T, I> = super::PResult<T, I, Error<I>>;

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

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
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

    fn parse(&self, input: I) -> PResult<AsciiChar, I> {
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

pub fn alphabetic<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Alphabetic))(input)
}

pub fn alphanumeric<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Alphanumeric))(input)
}

pub fn blank<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Blank))(input)
}

pub fn control<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Control))(input)
}

pub fn digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Digit))(input)
}

pub fn graphic<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Graphic))(input)
}

pub fn hex_digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::HexDigit))(input)
}

pub fn lowercase<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Lowercase))(input)
}

pub fn oct_digit<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::OctDigit))(input)
}

pub fn printable<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Printable))(input)
}

pub fn punctuation<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Punctuation))(input)
}

pub fn uppercase<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Uppercase))(input)
}

pub fn whitespace<I: AsciiInput>(input: I) -> PResult<AsciiChar, I> {
    compile!(char_with_prop(prop::Whitespace))(input)
}

impl<I: Input> Error<I> {
    pub const fn new(kind: ErrorKind, pos: I) -> Self {
        Self { kind, pos }
    }

    pub const fn non_ascii_char(pos: I) -> Self {
        Self::new(ErrorKind::NonAsciiChar, pos)
    }

    pub const fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl<I: Input> crate::Error<I> for Error<I> {
    fn need_more_input(pos: I) -> Self {
        Self::new(ErrorKind::NeedMoreInput, pos)
    }

    fn expected_eof(pos: I) -> Self {
        Self::new(ErrorKind::ExpectedEof, pos)
    }

    fn invalid_input(pos: I) -> Self {
        Self::new(ErrorKind::InvalidInput, pos)
    }

    fn position(&self) -> &I {
        &self.pos
    }
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
            return Err(Failure(Error::need_more_input(input), orig_input));
        };
        let Ok(ch) = (ch as u16).to_ascii_char() else {
            return Err(Failure(
                Error::non_ascii_char(orig_input.clone()),
                orig_input,
            ));
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
            return Err(Failure(Error::need_more_input(input), orig_input));
        };
        let Ok(ch) = (ch as u32).to_ascii_char() else {
            return Err(Failure(
                Error::non_ascii_char(orig_input.clone()),
                orig_input,
            ));
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
        return Err(Failure(Error::need_more_input(input), orig_input));
    };
    let Ok(ch) = ch.to_ascii_char() else {
        return Err(Failure(
            Error::non_ascii_char(orig_input.clone()),
            orig_input,
        ));
    };
    Ok(Success(ch, input))
}
