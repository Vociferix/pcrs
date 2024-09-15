use crate::{basic::pop, Failure, Input, PResult, Parse, Success};
use core::marker::PhantomData;

pub trait ByteSymbol {
    fn parse_byte<I>(input: I) -> PResult<u8, I>
    where
        I: Input<Symbol = Self>;
}

pub trait ByteInput: Input {
    fn parse_byte(self) -> PResult<u8, Self>;
}

impl<I> ByteInput for I
where
    I: Input,
    I::Symbol: ByteSymbol,
{
    fn parse_byte(self) -> PResult<u8, Self> {
        <I::Symbol as ByteSymbol>::parse_byte(self)
    }
}

impl ByteSymbol for u8 {
    fn parse_byte<I>(input: I) -> PResult<u8, I>
    where
        I: Input<Symbol = Self>,
    {
        pop(input)
    }
}

impl ByteSymbol for i8 {
    fn parse_byte<I>(input: I) -> PResult<u8, I>
    where
        I: Input<Symbol = Self>,
    {
        pop.map(|b| b as u8).parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct VerbatimParser<P, I>(P, PhantomData<I>)
where
    P: Input<Symbol = u8>,
    I: ByteInput;

impl<P, I> Parse<I> for VerbatimParser<P, I>
where
    P: Input<Symbol = u8>,
    I: ByteInput,
{
    type Parsed = super::Span<I>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let mut expected = self.0.clone();
        let mut rem = input.clone();

        while let Some(ex) = expected.next() {
            if let Ok(Success(b, new_rem)) = rem.parse_byte() {
                if b != ex {
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
    P: Input<Symbol = u8>,
    I: ByteInput,
{
    VerbatimParser(pattern, PhantomData)
}

pub fn u8<I: ByteInput>(input: I) -> PResult<u8, I> {
    I::parse_byte(input)
}

pub fn i8<I: ByteInput>(input: I) -> PResult<i8, I> {
    const { &super::basic::map(u8, |b| b as i8) }.parse(input)
}

pub mod be {
    use super::{u8, ByteInput};
    use crate::{
        basic::{array, map},
        PResult, Parse,
    };

    pub fn u16<I: ByteInput>(input: I) -> PResult<u16, I> {
        const { &map(array(u8), u16::from_be_bytes) }.parse(input)
    }

    pub fn u24<I: ByteInput>(input: I) -> PResult<u32, I> {
        const {
            &map(array(u8), |[b0, b1, b2]| {
                u32::from_be_bytes([0, b0, b1, b2])
            })
        }
        .parse(input)
    }

    pub fn u32<I: ByteInput>(input: I) -> PResult<u32, I> {
        const { &map(array(u8), u32::from_be_bytes) }.parse(input)
    }

    pub fn u40<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4]| {
                u64::from_be_bytes([0, 0, 0, b0, b1, b2, b3, b4])
            })
        }
        .parse(input)
    }

    pub fn u48<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5]| {
                u64::from_be_bytes([0, 0, b0, b1, b2, b3, b4, b5])
            })
        }
        .parse(input)
    }

    pub fn u56<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6]| {
                u64::from_be_bytes([0, b0, b1, b2, b3, b4, b5, b6])
            })
        }
        .parse(input)
    }

    pub fn u64<I: ByteInput>(input: I) -> PResult<u64, I> {
        const { &map(array(u8), u64::from_be_bytes) }.parse(input)
    }

    pub fn u72<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8]| {
                u128::from_be_bytes([0, 0, 0, 0, 0, 0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8])
            })
        }
        .parse(input)
    }

    pub fn u80<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]| {
                u128::from_be_bytes([0, 0, 0, 0, 0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9])
            })
        }
        .parse(input)
    }

    pub fn u88<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10]| {
                u128::from_be_bytes([0, 0, 0, 0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10])
            })
        }
        .parse(input)
    }

    pub fn u96<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]| {
                    u128::from_be_bytes([
                        0, 0, 0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u104<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12]| {
                    u128::from_be_bytes([
                        0, 0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u112<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13]| {
                    u128::from_be_bytes([
                        0, 0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u120<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]| {
                    u128::from_be_bytes([
                        0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u128<I: ByteInput>(input: I) -> PResult<u128, I> {
        const { &map(array(u8), u128::from_be_bytes) }.parse(input)
    }

    pub fn i16<I: ByteInput>(input: I) -> PResult<i16, I> {
        const { &map(array(u8), i16::from_be_bytes) }.parse(input)
    }

    pub fn i24<I: ByteInput>(input: I) -> PResult<i32, I> {
        const {
            &map(array(u8), |[b0, b1, b2]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i32::from_be_bytes([s, b0, b1, b2])
            })
        }
        .parse(input)
    }

    pub fn i32<I: ByteInput>(input: I) -> PResult<i32, I> {
        const { &map(array(u8), i32::from_be_bytes) }.parse(input)
    }

    pub fn i40<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i64::from_be_bytes([s, s, s, b0, b1, b2, b3, b4])
            })
        }
        .parse(input)
    }

    pub fn i48<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i64::from_be_bytes([s, s, b0, b1, b2, b3, b4, b5])
            })
        }
        .parse(input)
    }

    pub fn i56<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i64::from_be_bytes([s, b0, b1, b2, b3, b4, b5, b6])
            })
        }
        .parse(input)
    }

    pub fn i64<I: ByteInput>(input: I) -> PResult<i64, I> {
        const { &map(array(u8), i64::from_be_bytes) }.parse(input)
    }

    pub fn i72<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i128::from_be_bytes([s, s, s, s, s, s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8])
            })
        }
        .parse(input)
    }

    pub fn i80<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i128::from_be_bytes([s, s, s, s, s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9])
            })
        }
        .parse(input)
    }

    pub fn i88<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10]| {
                let s = if b0 < 0x80 { 0 } else { 0xff };
                i128::from_be_bytes([s, s, s, s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10])
            })
        }
        .parse(input)
    }

    pub fn i96<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]| {
                    let s = if b0 < 0x80 { 0 } else { 0xff };
                    i128::from_be_bytes([
                        s, s, s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i104<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12]| {
                    let s = if b0 < 0x80 { 0 } else { 0xff };
                    i128::from_be_bytes([
                        s, s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i112<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13]| {
                    let s = if b0 < 0x80 { 0 } else { 0xff };
                    i128::from_be_bytes([
                        s, s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i120<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]| {
                    let s = if b0 < 0x80 { 0 } else { 0xff };
                    i128::from_be_bytes([
                        s, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i128<I: ByteInput>(input: I) -> PResult<i128, I> {
        const { &map(array(u8), i128::from_be_bytes) }.parse(input)
    }

    pub fn f32<I: ByteInput>(input: I) -> PResult<f32, I> {
        const { &map(array(u8), f32::from_be_bytes) }.parse(input)
    }

    pub fn f64<I: ByteInput>(input: I) -> PResult<f64, I> {
        const { &map(array(u8), f64::from_be_bytes) }.parse(input)
    }
}

pub mod le {
    use super::{u8, ByteInput};
    use crate::{
        basic::{array, map},
        PResult, Parse,
    };

    pub fn u16<I: ByteInput>(input: I) -> PResult<u16, I> {
        const { &map(array(u8), u16::from_le_bytes) }.parse(input)
    }

    pub fn u24<I: ByteInput>(input: I) -> PResult<u32, I> {
        const {
            &map(array(u8), |[b0, b1, b2]| {
                u32::from_le_bytes([b0, b1, b2, 0])
            })
        }
        .parse(input)
    }

    pub fn u32<I: ByteInput>(input: I) -> PResult<u32, I> {
        const { &map(array(u8), u32::from_le_bytes) }.parse(input)
    }

    pub fn u40<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4]| {
                u64::from_le_bytes([b0, b1, b2, b3, b4, 0, 0, 0])
            })
        }
        .parse(input)
    }

    pub fn u48<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5]| {
                u64::from_le_bytes([b0, b1, b2, b3, b4, b5, 0, 0])
            })
        }
        .parse(input)
    }

    pub fn u56<I: ByteInput>(input: I) -> PResult<u64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6]| {
                u64::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, 0])
            })
        }
        .parse(input)
    }

    pub fn u64<I: ByteInput>(input: I) -> PResult<u64, I> {
        const { &map(array(u8), u64::from_le_bytes) }.parse(input)
    }

    pub fn u72<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8]| {
                u128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, 0, 0, 0, 0, 0, 0, 0])
            })
        }
        .parse(input)
    }

    pub fn u80<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]| {
                u128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, 0, 0, 0, 0, 0, 0])
            })
        }
        .parse(input)
    }

    pub fn u88<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10]| {
                u128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, 0, 0, 0, 0, 0])
            })
        }
        .parse(input)
    }

    pub fn u96<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]| {
                    u128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, 0, 0, 0, 0,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u104<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12]| {
                    u128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, 0, 0, 0,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u112<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13]| {
                    u128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, 0, 0,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u120<I: ByteInput>(input: I) -> PResult<u128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]| {
                    u128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, 0,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn u128<I: ByteInput>(input: I) -> PResult<u128, I> {
        const { &map(array(u8), u128::from_le_bytes) }.parse(input)
    }

    pub fn i16<I: ByteInput>(input: I) -> PResult<i16, I> {
        const { &map(array(u8), i16::from_le_bytes) }.parse(input)
    }

    pub fn i24<I: ByteInput>(input: I) -> PResult<i32, I> {
        const {
            &map(array(u8), |[b0, b1, b2]| {
                let s = if b2 < 0x80 { 0 } else { 0xff };
                i32::from_le_bytes([b0, b1, b2, s])
            })
        }
        .parse(input)
    }

    pub fn i32<I: ByteInput>(input: I) -> PResult<i32, I> {
        const { &map(array(u8), i32::from_le_bytes) }.parse(input)
    }

    pub fn i40<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4]| {
                let s = if b4 < 0x80 { 0 } else { 0xff };
                i64::from_le_bytes([b0, b1, b2, b3, b4, s, s, s])
            })
        }
        .parse(input)
    }

    pub fn i48<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5]| {
                let s = if b5 < 0x80 { 0 } else { 0xff };
                i64::from_le_bytes([b0, b1, b2, b3, b4, b5, s, s])
            })
        }
        .parse(input)
    }

    pub fn i56<I: ByteInput>(input: I) -> PResult<i64, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6]| {
                let s = if b6 < 0x80 { 0 } else { 0xff };
                i64::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, s])
            })
        }
        .parse(input)
    }

    pub fn i64<I: ByteInput>(input: I) -> PResult<i64, I> {
        const { &map(array(u8), i64::from_le_bytes) }.parse(input)
    }

    pub fn i72<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8]| {
                let s = if b8 < 0x80 { 0 } else { 0xff };
                i128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, s, s, s, s, s, s, s])
            })
        }
        .parse(input)
    }

    pub fn i80<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]| {
                let s = if b9 < 0x80 { 0 } else { 0xff };
                i128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, s, s, s, s, s, s])
            })
        }
        .parse(input)
    }

    pub fn i88<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(array(u8), |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10]| {
                let s = if b10 < 0x80 { 0 } else { 0xff };
                i128::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, s, s, s, s, s])
            })
        }
        .parse(input)
    }

    pub fn i96<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]| {
                    let s = if b11 < 0x80 { 0 } else { 0xff };
                    i128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, s, s, s, s,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i104<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12]| {
                    let s = if b12 < 0x80 { 0 } else { 0xff };
                    i128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, s, s, s,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i112<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13]| {
                    let s = if b13 < 0x80 { 0 } else { 0xff };
                    i128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, s, s,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i120<I: ByteInput>(input: I) -> PResult<i128, I> {
        const {
            &map(
                array(u8),
                |[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]| {
                    let s = if b14 < 0x80 { 0 } else { 0xff };
                    i128::from_le_bytes([
                        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, s,
                    ])
                },
            )
        }
        .parse(input)
    }

    pub fn i128<I: ByteInput>(input: I) -> PResult<i128, I> {
        const { &map(array(u8), i128::from_le_bytes) }.parse(input)
    }

    pub fn f32<I: ByteInput>(input: I) -> PResult<f32, I> {
        const { &map(array(u8), f32::from_le_bytes) }.parse(input)
    }

    pub fn f64<I: ByteInput>(input: I) -> PResult<f64, I> {
        const { &map(array(u8), f64::from_le_bytes) }.parse(input)
    }
}
