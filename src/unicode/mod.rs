use crate::{Failure, Input, PResult, Parse, Success};
use core::marker::PhantomData;

pub mod prop;

pub trait UnicodeSymbol {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = Self>;
}

pub trait UnicodeInput: Input {
    fn parse_char(self) -> PResult<char, Self>;
}

pub trait Property: core::fmt::Debug + Copy {
    fn contains(self, ch: char) -> bool;
}

pub fn char<I: UnicodeInput>(input: I) -> PResult<char, I> {
    I::parse_char(input)
}

#[derive(Debug, Clone)]
pub struct CharWithPropParser<P, I>(P, PhantomData<I>)
where
    P: Property,
    I: UnicodeInput;

impl<P, I> Parse<I> for CharWithPropParser<P, I>
where
    P: Property,
    I: UnicodeInput,
{
    type Parsed = char;

    fn parse(&self, input: I) -> PResult<char, I> {
        match input.clone().parse_char() {
            Ok(Success(ch, rem)) if self.0.contains(ch) => Ok(Success(ch, rem)),
            _ => Err(Failure(input)),
        }
    }
}

pub const fn char_with_prop<P, I>(property: P) -> CharWithPropParser<P, I>
where
    P: Property,
    I: UnicodeInput,
{
    CharWithPropParser(property, PhantomData)
}

impl<I> UnicodeInput for I
where
    I: Input,
    I::Symbol: UnicodeSymbol,
{
    fn parse_char(self) -> PResult<char, Self> {
        <I::Symbol as UnicodeSymbol>::parse_char(self)
    }
}

impl UnicodeSymbol for char {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = char>,
    {
        use crate::basic::pop;
        pop(input)
    }
}

impl UnicodeSymbol for u32 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = u32>,
    {
        utf32_char(input)
    }
}

impl UnicodeSymbol for i32 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = i32>,
    {
        utf32_char(input)
    }
}

impl UnicodeSymbol for u16 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = u16>,
    {
        utf16_char(input)
    }
}

impl UnicodeSymbol for i16 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = i16>,
    {
        utf16_char(input)
    }
}

impl UnicodeSymbol for u8 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = u8>,
    {
        utf8_char(input)
    }
}

impl UnicodeSymbol for i8 {
    fn parse_char<I>(input: I) -> PResult<char, I>
    where
        I: Input<Symbol = i8>,
    {
        utf8_char(input)
    }
}

trait AsU8 {
    fn as_u8(self) -> u8;
}

impl AsU8 for u8 {
    fn as_u8(self) -> u8 {
        self
    }
}

impl AsU8 for i8 {
    fn as_u8(self) -> u8 {
        self as u8
    }
}

trait AsU16 {
    fn as_u16(self) -> u16;
}

impl AsU16 for u16 {
    fn as_u16(self) -> u16 {
        self
    }
}

impl AsU16 for i16 {
    fn as_u16(self) -> u16 {
        self as u16
    }
}

trait AsU32 {
    fn as_u32(self) -> u32;
}

impl AsU32 for u32 {
    fn as_u32(self) -> u32 {
        self
    }
}

impl AsU32 for i32 {
    fn as_u32(self) -> u32 {
        self as u32
    }
}

fn utf32_char<I>(mut input: I) -> PResult<char, I>
where
    I: Input,
    I::Symbol: AsU32,
{
    if let Some(ch) = input.next().map(AsU32::as_u32).and_then(char::from_u32) {
        Ok(Success(ch, input))
    } else {
        Err(Failure(input))
    }
}

fn utf16_char<I>(input: I) -> PResult<char, I>
where
    I: Input,
    I::Symbol: AsU16,
{
    let mut rem = input.clone();
    let Some(c0) = rem.next().map(AsU16::as_u16) else {
        return Err(Failure(input));
    };
    let ch = if (c0 & 0b1111_1100_0000_0000) == 0b1101_1000_0000_0000 {
        let Some(c1) = rem.next().map(AsU16::as_u16) else {
            return Err(Failure(input));
        };
        if (c1 & 0b1111_1100_0000_0000) != 0b1101_1100_0000_0000 {
            return Err(Failure(input));
        }
        let c0 = (c0 & 0b0000_0011_1111_1111) as u32;
        let c1 = (c1 & 0b0000_0011_1111_1111) as u32;
        ((c0 << 10) | c1) + 0x10000
    } else {
        c0 as u32
    };
    if let Some(ch) = char::from_u32(ch) {
        Ok(Success(ch, rem))
    } else {
        Err(Failure(input))
    }
}

fn utf8_char<I>(input: I) -> PResult<char, I>
where
    I: Input,
    I::Symbol: AsU8,
{
    let mut rem = input.clone();
    let Some(b0) = rem.next().map(|b0| b0.as_u8()) else {
        return Err(Failure(input));
    };
    if b0 < 0x80 {
        return if let Some(ch) = char::from_u32(b0 as u32) {
            Ok(Success(ch, rem))
        } else {
            Err(Failure(input))
        };
    } else if (b0 & 0b1100_0000) == 0b1000_0000 {
        return Err(Failure(input));
    }

    let Some(b1) = rem.next().map(|b1| b1.as_u8()) else {
        return Err(Failure(input));
    };
    if (b1 & 0b1100_0000) != 0b1000_0000 {
        return Err(Failure(input));
    }
    let b1 = (b1 & 0b0011_1111) as u32;
    if (b0 & 0b1110_0000) == 0b1100_0000 {
        let b0 = (b0 & 0b0001_1111) as u32;
        let ch = (b0 << 6) | b1;
        return if let Some(ch) = char::from_u32(ch) {
            Ok(Success(ch, rem))
        } else {
            Err(Failure(input))
        };
    }

    let Some(b2) = rem.next().map(|b2| b2.as_u8()) else {
        return Err(Failure(input));
    };
    if (b2 & 0b1100_0000) != 0b1000_0000 {
        return Err(Failure(input));
    }
    let b2 = (b2 & 0b0011_1111) as u32;
    if (b0 & 0b1111_0000) == 0b1110_0000 {
        let b0 = (b0 & 0b0000_1111) as u32;
        let ch = (b0 << 12) | (b1 << 6) | b2;
        return if let Some(ch) = char::from_u32(ch) {
            Ok(Success(ch, rem))
        } else {
            Err(Failure(input))
        };
    }

    let Some(b3) = rem.next().map(|b3| b3.as_u8()) else {
        return Err(Failure(input));
    };
    if (b3 & 0b1100_0000) != 0b1000_0000 {
        return Err(Failure(input));
    }
    let b3 = (b3 & 0b0011_1111) as u32;
    if (b0 & 0b1111_1000) == 0b1111_0000 {
        let b0 = (b0 & 0b0000_0111) as u32;
        let ch = (b0 << 18) | (b1 << 12) | (b2 << 6) | b3;
        return if let Some(ch) = char::from_u32(ch) {
            Ok(Success(ch, rem))
        } else {
            Err(Failure(input))
        };
    }

    Err(Failure(input))
}

pub struct VerbatimParser<P, I>(P, PhantomData<I>)
where
    P: Input<Symbol = char>,
    I: UnicodeInput;

impl<P, I> Parse<I> for VerbatimParser<P, I>
where
    P: Input<Symbol = char>,
    I: UnicodeInput,
{
    type Parsed = super::Span<I>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let mut expected = self.0.clone();
        let mut rem = input.clone();

        while let Some(ex) = expected.next() {
            if let Ok(Success(ch, new_rem)) = rem.parse_char() {
                if ch != ex {
                    return Err(Failure(input));
                }
                rem = new_rem;
            } else {
                return Err(Failure(input));
            }
        }

        Ok(Success(super::Span::new(input, rem.clone()), rem))
    }
}

pub const fn verbatim<P, I>(pattern: P) -> VerbatimParser<P, I>
where
    P: Input<Symbol = char>,
    I: UnicodeInput,
{
    VerbatimParser(pattern, PhantomData)
}
