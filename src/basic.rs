use crate::{Failure, Input, PResult, PResultExt, Parse, Span, Success};
use core::marker::PhantomData;

pub use pcrs_macros::{alt, permutation, seq};

pub const fn fail<T, I: Input>(input: I) -> PResult<T, I> {
    Err(Failure(input))
}

#[derive(Debug, Clone)]
pub struct ConstantParser<T, I>(T, PhantomData<I>)
where
    T: Clone,
    I: Input;

impl<T: Clone, I: Input> Parse<I> for ConstantParser<T, I> {
    type Parsed = T;

    fn parse(&self, input: I) -> PResult<T, I> {
        Ok(Success(self.0.clone(), input))
    }
}

pub const fn constant<I, T>(value: T) -> ConstantParser<T, I>
where
    T: Clone,
    I: Input,
{
    ConstantParser(value, PhantomData)
}

#[derive(Debug, Clone)]
pub struct MapParser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> R;

impl<P, F, R, I> Parse<I> for MapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> R,
{
    type Parsed = R;

    fn parse(&self, input: I) -> PResult<R, I> {
        self.0.parse(input).map_parsed(&self.1)
    }
}

pub const fn map<P, F, R, I>(parser: P, map_fn: F) -> MapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> R,
{
    MapParser(parser, map_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryMapParser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> Option<R>;

impl<P, F, R, I> Parse<I> for TryMapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> Option<R>,
{
    type Parsed = R;

    fn parse(&self, input: I) -> PResult<R, I> {
        if let Ok(Success(Some(val), rem)) = self.0.parse(input.clone()).map_parsed(&self.1) {
            Ok(Success(val, rem))
        } else {
            Err(Failure(input))
        }
    }
}

pub const fn try_map<P, F, R, I>(parser: P, try_map_fn: F) -> TryMapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> Option<R>,
{
    TryMapParser(parser, try_map_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TakeParser<I>(usize, PhantomData<I>)
where
    I: Input;

impl<I: Input> Parse<I> for TakeParser<I> {
    type Parsed = Span<I>;

    fn parse(&self, mut input: I) -> PResult<Span<I>, I> {
        let begin = input.clone();
        if input.advance_by(self.0) != self.0 {
            Err(Failure(begin))
        } else {
            Ok(Success(Span::new(begin, input.clone()), input))
        }
    }
}

pub const fn take<I>(count: usize) -> TakeParser<I>
where
    I: Input,
{
    TakeParser(count, PhantomData)
}

#[derive(Debug, Clone)]
pub struct ValueParser<P, T, I>(P, T, PhantomData<I>)
where
    P: Parse<I>,
    I: Input,
    T: Clone;

impl<P, T, I> Parse<I> for ValueParser<P, T, I>
where
    P: Parse<I>,
    T: Clone,
    I: Input,
{
    type Parsed = T;

    fn parse(&self, input: I) -> PResult<T, I> {
        self.0.parse(input).map_parsed(|_| self.1.clone())
    }
}

pub const fn value<P, T, I>(parser: P, value: T) -> ValueParser<P, T, I>
where
    P: Parse<I>,
    T: Clone,
    I: Input,
{
    ValueParser(parser, value, PhantomData)
}

#[derive(Debug, Clone)]
pub struct FlatMapParser<P, C, R, I>(P, C, PhantomData<(R, I)>);

impl<P, C, R, I> Parse<I> for FlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> R,
    R: Parse<I>,
    I: Input,
{
    type Parsed = R::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(arg, rem)) => match ((self.1)(arg)).parse(rem) {
                Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
                Err(Failure(_)) => Err(Failure(input)),
            },
            Err(Failure(_)) => Err(Failure(input)),
        }
    }
}

pub const fn flat_map<P, C, R, I>(parser: P, combinator: C) -> FlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> R,
    R: Parse<I>,
    I: Input,
{
    FlatMapParser(parser, combinator, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryFlatMapParser<P, C, R, I>(P, C, PhantomData<(R, I)>);

impl<P, C, R, I> Parse<I> for TryFlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> Option<R>,
    R: Parse<I>,
    I: Input,
{
    type Parsed = R::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(arg, rem)) => {
                if let Some(p) = (self.1)(arg) {
                    match p.parse(rem) {
                        Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
                        Err(Failure(_)) => Err(Failure(input)),
                    }
                } else {
                    Err(Failure(input))
                }
            }
            Err(Failure(_)) => Err(Failure(input)),
        }
    }
}

pub const fn try_flat_map<P, C, R, I>(parser: P, combinator: C) -> TryFlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> Option<R>,
    R: Parse<I>,
    I: Input,
{
    TryFlatMapParser(parser, combinator, PhantomData)
}

#[derive(Debug, Clone)]
pub struct VerbatimParser<P, I>(P, PhantomData<I>)
where
    P: Input,
    I: Input;

impl<P, I> Parse<I> for VerbatimParser<P, I>
where
    P: Input,
    I: Input,
    I::Symbol: PartialEq<P::Symbol>,
{
    type Parsed = Span<I>;

    fn parse(&self, mut input: I) -> PResult<Span<I>, I> {
        let mut pattern = self.0.clone();
        let orig_input = input.clone();
        while let Some(expected) = pattern.next() {
            let Some(symb) = input.next() else {
                return Err(Failure(orig_input));
            };
            if !PartialEq::eq(&symb, &expected) {
                return Err(Failure(orig_input));
            }
        }
        Ok(Success(Span::new(orig_input, input.clone()), input))
    }
}

pub const fn verbatim<P, I>(pattern: P) -> VerbatimParser<P, I>
where
    P: Input,
    I: Input,
    I::Symbol: PartialEq<P::Symbol>,
{
    VerbatimParser(pattern, PhantomData)
}

pub fn pop<I: Input>(mut input: I) -> PResult<I::Symbol, I> {
    if let Some(symb) = input.next() {
        Ok(Success(symb, input))
    } else {
        Err(Failure(input))
    }
}

#[derive(Debug, Clone)]
pub struct AndThenParser<P, F, R, I>(P, F, PhantomData<(R, I)>);

impl<P, F, R, I> Parse<I> for AndThenParser<P, F, R, I>
where
    P: Parse<I>,
    F: Fn(P::Parsed) -> Option<R>,
    I: Input,
{
    type Parsed = R;

    fn parse(&self, input: I) -> PResult<R, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(arg, rem)) => {
                if let Some(ret) = (self.1)(arg) {
                    Ok(Success(ret, rem))
                } else {
                    Err(Failure(input))
                }
            }
            Err(Failure(_)) => Err(Failure(input)),
        }
    }
}

pub const fn and_then<P, F, R, I>(parser: P, f: F) -> AndThenParser<P, F, R, I>
where
    P: Parse<I>,
    F: Fn(P::Parsed) -> Option<R>,
    I: Input,
{
    AndThenParser(parser, f, PhantomData)
}

#[derive(Debug, Clone)]
pub struct OrValueParser<P, I>(P, P::Parsed, PhantomData<I>)
where
    P: Parse<I>,
    I: Input,
    P::Parsed: Clone;

impl<P, I> Parse<I> for OrValueParser<P, I>
where
    P: Parse<I>,
    I: Input,
    P::Parsed: Clone,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(rem)) => Ok(Success(self.1.clone(), rem)),
        }
    }
}

pub const fn or_value<P, I>(parser: P, value: P::Parsed) -> OrValueParser<P, I>
where
    P: Parse<I>,
    I: Input,
    P::Parsed: Clone,
{
    OrValueParser(parser, value, PhantomData)
}

#[derive(Debug, Clone)]
pub struct OrDefaultParser<P, I>(P, PhantomData<I>);

impl<P, I> Parse<I> for OrDefaultParser<P, I>
where
    P: Parse<I>,
    P::Parsed: Default,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(rem)) => Ok(Success(Default::default(), rem)),
        }
    }
}

pub const fn or_default<P, I>(parser: P) -> OrDefaultParser<P, I>
where
    P: Parse<I>,
    P::Parsed: Default,
    I: Input,
{
    OrDefaultParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct OrElseParser<P, F, I>(P, F, PhantomData<I>);

impl<P, F, I> Parse<I> for OrElseParser<P, F, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn() -> P::Parsed,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(rem)) => Ok(Success((self.1)(), rem)),
        }
    }
}

pub const fn or_else<P, F, I>(parser: P, or_else_fn: F) -> OrElseParser<P, F, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn() -> P::Parsed,
{
    OrElseParser(parser, or_else_fn, PhantomData)
}

pub struct Many0Iter<'a, P, I>(&'a P, &'a mut I);

impl<'a, P, I> Iterator for Many0Iter<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.0.parse(self.1.clone());
        match res {
            Ok(Success(ret, rem)) => {
                *self.1 = rem;
                Some(ret)
            }
            Err(Failure(_)) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Many0Parser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> R;

impl<P, F, R, I> Parse<I> for Many0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> R,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let ret = (self.1)(Many0Iter(&self.0, &mut input));
        let _ = Many0Iter(&self.0, &mut input).count();
        Ok(Success(ret, input))
    }
}

pub const fn many0<P, F, R, I>(parser: P, collect_fn: F) -> Many0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> R,
{
    Many0Parser(parser, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryMany0Parser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Option<R>;

impl<P, F, R, I> Parse<I> for TryMany0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Option<R>,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let orig_input = input.clone();
        let Some(ret) = (self.1)(Many0Iter(&self.0, &mut input)) else {
            return Err(Failure(orig_input));
        };
        let _ = Many0Iter(&self.0, &mut input).count();
        Ok(Success(ret, input))
    }
}

pub const fn try_many0<P, F, R, I>(parser: P, collect_fn: F) -> TryMany0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Option<R>,
{
    TryMany0Parser(parser, collect_fn, PhantomData)
}

pub struct Many1Iter<'a, P, I>(&'a P, &'a mut I, &'a mut bool);

impl<'a, P, I> Iterator for Many1Iter<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.0.parse(self.1.clone());
        match res {
            Ok(Success(ret, rem)) => {
                *self.2 = true;
                *self.1 = rem;
                Some(ret)
            }
            Err(Failure(_)) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Many1Parser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> R;

impl<P, F, R, I> Parse<I> for Many1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> R,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let mut parsed = false;
        let orig_input = input.clone();
        let ret = (self.1)(Many1Iter(&self.0, &mut input, &mut parsed));
        let _ = Many1Iter(&self.0, &mut input, &mut parsed).count();
        if !parsed {
            return Err(Failure(orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn many1<P, F, R, I>(parser: P, collect_fn: F) -> Many1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> R,
{
    Many1Parser(parser, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryMany1Parser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Option<R>;

impl<P, F, R, I> Parse<I> for TryMany1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Option<R>,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let mut parsed = false;
        let orig_input = input.clone();
        let Some(ret) = (self.1)(Many1Iter(&self.0, &mut input, &mut parsed)) else {
            return Err(Failure(orig_input));
        };
        let _ = Many1Iter(&self.0, &mut input, &mut parsed).count();
        if !parsed {
            return Err(Failure(orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn try_many1<P, F, R, I>(parser: P, collect_fn: F) -> TryMany1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Option<R>,
{
    TryMany1Parser(parser, collect_fn, PhantomData)
}

pub struct RepeatedIter<'a, P, I>(&'a P, &'a mut I, &'a mut usize);

impl<'a, P, I> Iterator for RepeatedIter<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        if *self.2 == 0 {
            return None;
        }

        let res = self.0.parse(self.1.clone());
        match res {
            Ok(Success(ret, rem)) => {
                *self.2 -= 1;
                *self.1 = rem;
                Some(ret)
            }
            Err(Failure(_)) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RepeatedParser<P, F, R, I>(P, usize, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> R;

impl<P, F, R, I> Parse<I> for RepeatedParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> R,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let mut remaining = self.1;
        let orig_input = input.clone();
        let ret = (self.2)(RepeatedIter(&self.0, &mut input, &mut remaining));
        let _ = RepeatedIter(&self.0, &mut input, &mut remaining).count();
        if remaining != 0 {
            return Err(Failure(orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn repeated<P, F, R, I>(
    parser: P,
    count: usize,
    collect_fn: F,
) -> RepeatedParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> R,
{
    RepeatedParser(parser, count, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryRepeatedParser<P, F, R, I>(P, usize, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Option<R>;

impl<P, F, R, I> Parse<I> for TryRepeatedParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Option<R>,
{
    type Parsed = R;

    fn parse(&self, mut input: I) -> PResult<R, I> {
        let mut remaining = self.1;
        let orig_input = input.clone();
        let Some(ret) = (self.2)(RepeatedIter(&self.0, &mut input, &mut remaining)) else {
            return Err(Failure(orig_input));
        };
        let _ = RepeatedIter(&self.0, &mut input, &mut remaining).count();
        if remaining != 0 {
            return Err(Failure(orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn try_repeated<P, F, R, I>(
    parser: P,
    count: usize,
    collect_fn: F,
) -> TryRepeatedParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Option<R>,
{
    TryRepeatedParser(parser, count, collect_fn, PhantomData)
}

pub struct ManyUntilIter<'a, P, Q, I>(&'a P, &'a Q, &'a mut I, &'a mut Option<Q::Parsed>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<'a, P, Q, I> Iterator for ManyUntilIter<'a, P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        if self.3.is_some() {
            return None;
        }

        match self.1.parse(self.2.clone()) {
            Ok(Success(sent, rem)) => {
                *self.3 = Some(sent);
                *self.2 = rem;
                None
            }
            Err(Failure(rem)) => match self.0.parse(rem) {
                Ok(Success(ret, rem)) => {
                    *self.2 = rem;
                    Some(ret)
                }
                Err(Failure(_)) => None,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ManyUntilParser<P, Q, F, R, I>(P, Q, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input;

impl<P, Q, F, R, I> Parse<I> for ManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input,
{
    type Parsed = (R, Q::Parsed);

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I> {
        let mut sentinel = None;
        let orig_input = input.clone();
        let ret = (self.2)(ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel));
        let _ = ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel).count();
        if let Some(sentinel) = sentinel {
            Ok(Success((ret, sentinel), input))
        } else {
            Err(Failure(orig_input))
        }
    }
}

pub const fn many_until<P, Q, F, R, I>(
    parser: P,
    sentinel_parser: Q,
    collect_fn: F,
) -> ManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input,
{
    ManyUntilParser(parser, sentinel_parser, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryManyUntilParser<P, Q, F, R, I>(P, Q, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Option<R>,
    I: Input;

impl<P, Q, F, R, I> Parse<I> for TryManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Option<R>,
    I: Input,
{
    type Parsed = (R, Q::Parsed);

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I> {
        let mut sentinel = None;
        let orig_input = input.clone();
        let Some(ret) = (self.2)(ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel)) else {
            return Err(Failure(orig_input));
        };
        let _ = ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel).count();
        if let Some(sentinel) = sentinel {
            Ok(Success((ret, sentinel), input))
        } else {
            Err(Failure(orig_input))
        }
    }
}

pub const fn try_many_until<P, Q, F, R, I>(
    parser: P,
    sentinel_parser: Q,
    collect_fn: F,
) -> TryManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Option<R>,
    I: Input,
{
    TryManyUntilParser(parser, sentinel_parser, collect_fn, PhantomData)
}

pub struct ManyRangeIter<'a, P, I>(&'a P, Option<usize>, &'a mut I, &'a mut usize);

impl<'a, P, I> Iterator for ManyRangeIter<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(max_count) = self.1 {
            if *self.3 >= max_count {
                return None;
            }
        }

        match self.0.parse(self.2.clone()) {
            Ok(Success(ret, rem)) => {
                *self.3 += 1;
                *self.2 = rem;
                Some(ret)
            }
            Err(Failure(_)) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ManyRangeParser<P, R, F, O, I>(P, R, F, PhantomData<(O, I)>)
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> O,
    I: Input;

impl<P, R, F, O, I> Parse<I> for ManyRangeParser<P, R, F, O, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> O,
    I: Input,
{
    type Parsed = O;

    fn parse(&self, mut input: I) -> PResult<O, I> {
        let max_count = match self.1.end_bound() {
            core::ops::Bound::Included(max_count) => Some(*max_count),
            core::ops::Bound::Excluded(&0) => Some(0),
            core::ops::Bound::Excluded(max_count) => Some(*max_count - 1),
            _ => None,
        };

        let mut count = 0usize;
        let orig_input = input.clone();
        let ret = (self.2)(ManyRangeIter(&self.0, max_count, &mut input, &mut count));
        let _ = ManyRangeIter(&self.0, max_count, &mut input, &mut count).count();

        match self.1.start_bound() {
            core::ops::Bound::Included(min_count) if count < *min_count => Err(Failure(orig_input)),
            core::ops::Bound::Excluded(min_count) if count <= *min_count => {
                Err(Failure(orig_input))
            }
            _ => Ok(Success(ret, input)),
        }
    }
}

pub const fn many_range<P, R, F, O, I>(
    parser: P,
    range: R,
    collect_fn: F,
) -> ManyRangeParser<P, R, F, O, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> O,
    I: Input,
{
    ManyRangeParser(parser, range, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryManyRangeParser<P, R, F, O, I>(P, R, F, PhantomData<(O, I)>)
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Option<O>,
    I: Input;

impl<P, R, F, O, I> Parse<I> for TryManyRangeParser<P, R, F, O, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Option<O>,
    I: Input,
{
    type Parsed = O;

    fn parse(&self, mut input: I) -> PResult<O, I> {
        let max_count = match self.1.end_bound() {
            core::ops::Bound::Included(max_count) => Some(*max_count),
            core::ops::Bound::Excluded(&0) => Some(0),
            core::ops::Bound::Excluded(max_count) => Some(*max_count - 1),
            _ => None,
        };

        let mut count = 0usize;
        let orig_input = input.clone();
        let Some(ret) = (self.2)(ManyRangeIter(&self.0, max_count, &mut input, &mut count)) else {
            return Err(Failure(orig_input));
        };
        let _ = ManyRangeIter(&self.0, max_count, &mut input, &mut count).count();

        match self.1.start_bound() {
            core::ops::Bound::Included(min_count) if count < *min_count => Err(Failure(orig_input)),
            core::ops::Bound::Excluded(min_count) if count <= *min_count => {
                Err(Failure(orig_input))
            }
            _ => Ok(Success(ret, input)),
        }
    }
}

pub const fn try_many_range<P, R, F, O, I>(
    parser: P,
    range: R,
    collect_fn: F,
) -> TryManyRangeParser<P, R, F, O, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Option<O>,
    I: Input,
{
    TryManyRangeParser(parser, range, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct ArrayParser<P, I, const LEN: usize>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I, const LEN: usize> Parse<I> for ArrayParser<P, I, LEN>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = [P::Parsed; LEN];

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let mut rem = input.clone();
        let mut arr: [core::mem::MaybeUninit<P::Parsed>; LEN] =
            [const { core::mem::MaybeUninit::uninit() }; LEN];
        for idx in 0..LEN {
            match self.0.parse(rem) {
                Ok(Success(val, new_rem)) => {
                    arr[idx].write(val);
                    rem = new_rem;
                }
                _ => {
                    unsafe {
                        for i in 0..idx {
                            arr[i].assume_init_drop();
                        }
                    }
                    return Err(Failure(input));
                }
            }
        }
        Ok(Success(
            unsafe {
                core::ptr::read(core::mem::transmute(
                    &arr as *const [core::mem::MaybeUninit<P::Parsed>; LEN],
                ))
            },
            rem,
        ))
    }
}

pub const fn array<P, I, const LEN: usize>(parser: P) -> ArrayParser<P, I, LEN>
where
    P: Parse<I>,
    I: Input,
{
    ArrayParser(parser, PhantomData)
}

pub fn eof<I: Input>(input: I) -> PResult<(), I> {
    if let Some(_) = input.clone().next() {
        Err(Failure(input))
    } else {
        Ok(Success((), input))
    }
}

#[derive(Debug, Clone)]
pub struct AllConsumingParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for AllConsumingParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let Success(val, mut rem) = self.0.parse(input.clone())?;
        if let Some(_) = rem.next() {
            Err(Failure(input))
        } else {
            Ok(Success(val, rem))
        }
    }
}

pub const fn all_consuming<P, I>(parser: P) -> AllConsumingParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    AllConsumingParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct SpannedParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for SpannedParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = (P::Parsed, Span<I>);

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let orig_input = input.clone();
        let Success(ret, rem) = self.0.parse(input)?;
        Ok(Success((ret, Span::new(orig_input, rem.clone())), rem))
    }
}

pub const fn spanned<P, I>(parser: P) -> SpannedParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    SpannedParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct RecognizeParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for RecognizeParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = Span<I>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        let orig_input = input.clone();
        let Success(_, rem) = self.0.parse(input)?;
        Ok(Success(Span::new(orig_input, rem.clone()), rem))
    }
}

pub const fn recognize<P, I>(parser: P) -> RecognizeParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    RecognizeParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct CondParser<P, I>(P, bool, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for CondParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = Option<P::Parsed>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        if self.1 {
            Ok(Success(None, input))
        } else {
            self.0.parse(input).map_parsed(Some)
        }
    }
}

pub const fn cond<P, I>(parser: P, condition: bool) -> CondParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    CondParser(parser, condition, PhantomData)
}

#[derive(Debug, Clone)]
pub struct VerifyParser<P, F, I>(P, F, PhantomData<I>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(&P::Parsed) -> bool;

impl<P, F, I> Parse<I> for VerifyParser<P, F, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(&P::Parsed) -> bool,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, rem)) if (self.1)(&val) => Ok(Success(val, rem)),
            _ => Err(Failure(input)),
        }
    }
}

pub const fn verify<P, F, I>(parser: P, verify_fn: F) -> VerifyParser<P, F, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(&P::Parsed) -> bool,
{
    VerifyParser(parser, verify_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct NotParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for NotParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = ();

    fn parse(&self, input: I) -> PResult<(), I> {
        match self.0.parse(input.clone()) {
            Ok(_) => Err(Failure(input)),
            Err(_) => Ok(Success((), input)),
        }
    }
}

pub const fn not<P, I>(parser: P) -> NotParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    NotParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct OptParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for OptParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = Option<P::Parsed>;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input) {
            Ok(Success(val, rem)) => Ok(Success(Some(val), rem)),
            Err(Failure(rem)) => Ok(Success(None, rem)),
        }
    }
}

pub const fn opt<P, I>(parser: P) -> OptParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    OptParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct PeekParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for PeekParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, _)) => Ok(Success(val, input)),
            _ => Err(Failure(input)),
        }
    }
}

pub const fn peek<P, I>(parser: P) -> PeekParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    PeekParser(parser, PhantomData)
}

pub fn remaining<I: Input>(mut input: I) -> PResult<I, I> {
    let ret = input.clone();
    input.advance_by(usize::MAX);
    Ok(Success(ret, input))
}

pub fn remaining_len<I: Input>(input: I) -> PResult<usize, I> {
    let len = input.clone().advance_by(usize::MAX);
    Ok(Success(len, input))
}

#[derive(Debug, Clone)]
pub struct FromRefParser<'a, P, I>(&'a P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<'a, P, I> Parse<I> for FromRefParser<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        self.0.parse(input)
    }
}

pub const fn from_ref<'a, P, I>(parser: &'a P) -> FromRefParser<'a, P, I>
where
    P: Parse<I>,
    I: Input,
{
    FromRefParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct PrefixParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<P, Q, I> Parse<I> for PrefixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    type Parsed = Q::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(_, rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => Ok(Success(val, rem)),
                _ => Err(Failure(input)),
            },
            _ => Err(Failure(input)),
        }
    }
}

pub const fn prefix<P, Q, I>(prefix: P, parser: Q) -> PrefixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    PrefixParser(prefix, parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct SuffixParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<P, Q, I> Parse<I> for SuffixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, rem)) => match self.1.parse(rem) {
                Ok(Success(_, rem)) => Ok(Success(val, rem)),
                _ => Err(Failure(input)),
            },
            _ => Err(Failure(input)),
        }
    }
}

pub const fn suffix<P, Q, I>(parser: P, suffix: Q) -> SuffixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    SuffixParser(parser, suffix, PhantomData)
}

#[derive(Debug, Clone)]
pub struct DelimitedParser<P, Q, S, I>(P, Q, S, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I>,
    S: Parse<I>,
    I: Input;

impl<P, Q, S, I> Parse<I> for DelimitedParser<P, Q, S, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    S: Parse<I>,
    I: Input,
{
    type Parsed = Q::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(_, rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => match self.2.parse(rem) {
                    Ok(Success(_, rem)) => Ok(Success(val, rem)),
                    _ => Err(Failure(input)),
                },
                _ => Err(Failure(input)),
            },
            _ => Err(Failure(input)),
        }
    }
}

pub const fn delimited<P, Q, S, I>(prefix: P, parser: Q, suffix: S) -> DelimitedParser<P, Q, S, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    S: Parse<I>,
    I: Input,
{
    DelimitedParser(prefix, parser, suffix, PhantomData)
}

#[derive(Debug, Clone)]
pub struct SeparatedParser<P, S, Q, I>(P, S, Q, PhantomData<I>)
where
    P: Parse<I>,
    S: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<P, S, Q, I> Parse<I> for SeparatedParser<P, S, Q, I>
where
    P: Parse<I>,
    S: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    type Parsed = (P::Parsed, Q::Parsed);

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(first, rem)) => match self.1.parse(rem) {
                Ok(Success(_, rem)) => match self.2.parse(rem) {
                    Ok(Success(second, rem)) => Ok(Success((first, second), rem)),
                    _ => Err(Failure(input)),
                },
                _ => Err(Failure(input)),
            },
            _ => Err(Failure(input)),
        }
    }
}

pub const fn separated<P, S, Q, I>(first: P, separator: S, second: Q) -> SeparatedParser<P, S, Q, I>
where
    P: Parse<I>,
    S: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    SeparatedParser(first, separator, second, PhantomData)
}

pub struct PairParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<P, Q, I> Parse<I> for PairParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    type Parsed = (P::Parsed, Q::Parsed);

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input.clone()) {
            Ok(Success(first, rem)) => match self.1.parse(rem) {
                Ok(Success(second, rem)) => Ok(Success((first, second), rem)),
                _ => Err(Failure(input)),
            },
            _ => Err(Failure(input)),
        }
    }
}

pub const fn pair<P, Q, I>(first: P, second: Q) -> PairParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    PairParser(first, second, PhantomData)
}

pub struct EitherParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input;

impl<P, Q, I> Parse<I> for EitherParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Parsed = P::Parsed>,
    I: Input,
{
    type Parsed = P::Parsed;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        match self.0.parse(input) {
            Ok(Success(val, rem)) => Ok(Success(val, rem)),
            Err(Failure(rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => Ok(Success(val, rem)),
                Err(Failure(rem)) => Err(Failure(rem)),
            },
        }
    }
}

pub const fn either<P, Q, I>(first: P, second: Q) -> EitherParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
{
    EitherParser(first, second, PhantomData)
}
