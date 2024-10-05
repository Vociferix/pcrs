use crate::{Error, Failure, Input, PResult, PResultExt, Parse, Span, Success};
use core::iter::FromIterator;
use core::marker::PhantomData;

pub use pcrs_macros::{alt, permutation, seq};

pub struct ErrorParser<T, I, E>(E, PhantomData<(T, I)>)
where
    E: Error<I> + Clone,
    I: Input;

impl<T, I, E> Parse<I> for ErrorParser<T, I, E>
where
    E: Error<I> + Clone,
    I: Input,
{
    type Parsed = T;
    type Error = E;

    fn parse(&self, input: I) -> PResult<T, I, E> {
        Err(Failure(self.0.clone(), input))
    }
}

pub const fn error<T, I, E>(error: E) -> ErrorParser<T, I, E>
where
    E: Error<I> + Clone,
    I: Input,
{
    ErrorParser(error, PhantomData)
}

#[derive(Debug, Clone)]
pub struct ConstantParser<T, I, E>(T, PhantomData<(I, E)>)
where
    T: Clone,
    I: Input,
    E: Error<I>;

impl<T: Clone, I: Input, E: Error<I>> Parse<I> for ConstantParser<T, I, E> {
    type Parsed = T;
    type Error = E;

    fn parse(&self, input: I) -> PResult<T, I, E> {
        Ok(Success(self.0.clone(), input))
    }
}

pub const fn constant<I, E, T>(value: T) -> ConstantParser<T, I, E>
where
    T: Clone,
    I: Input,
    E: Error<I>,
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<R, I, Self::Error> {
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
    F: Fn(P::Parsed) -> Result<R, P::Error>;

impl<P, F, R, I> Parse<I> for TryMapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> Result<R, P::Error>,
{
    type Parsed = R;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<R, I, Self::Error> {
        match self.0.parse(input.clone()).map_parsed(&self.1) {
            Ok(Success(Ok(val), rem)) => Ok(Success(val, rem)),
            Ok(Success(Err(err), _)) => Err(Failure(err, input)),
            Err(Failure(err, _)) => Err(Failure(err, input)),
        }
    }
}

pub const fn try_map<P, F, R, I>(parser: P, try_map_fn: F) -> TryMapParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Parsed) -> Result<R, P::Error>,
{
    TryMapParser(parser, try_map_fn, PhantomData)
}

pub struct MapErrParser<P, F, R, I>(P, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Error) -> R,
    R: Error<I>;

impl<P, F, R, I> Parse<I> for MapErrParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Error) -> R,
    R: Error<I>,
{
    type Parsed = P::Parsed;
    type Error = R;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(succ) => Ok(succ),
            Err(Failure(err, rem)) => Err(Failure((self.1)(err), rem)),
        }
    }
}

pub const fn map_err<P, F, R, I>(parser: P, map_err_fn: F) -> MapErrParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(P::Error) -> R,
    R: Error<I>,
{
    MapErrParser(parser, map_err_fn, PhantomData)
}

pub struct MapResParser<P, F, R, E, I>(P, F, PhantomData<(R, E, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(Result<P::Parsed, P::Error>) -> Result<R, E>,
    E: Error<I>;

impl<P, F, R, E, I> Parse<I> for MapResParser<P, F, R, E, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(Result<P::Parsed, P::Error>) -> Result<R, E>,
    E: Error<I>,
{
    type Parsed = R;
    type Error = E;

    fn parse(&self, input: I) -> PResult<R, I, E> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, rem)) => match (self.1)(Ok(val)) {
                Ok(val) => Ok(Success(val, rem)),
                Err(err) => Err(Failure(err, input)),
            },
            Err(Failure(err, rem)) => match (self.1)(Err(err)) {
                Ok(val) => Ok(Success(val, rem)),
                Err(err) => Err(Failure(err, input)),
            },
        }
    }
}

pub const fn map_res<P, F, R, E, I>(parser: P, map_res_fn: F) -> MapResParser<P, F, R, E, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(Result<P::Parsed, P::Error>) -> Result<R, E>,
    E: Error<I>,
{
    MapResParser(parser, map_res_fn, PhantomData)
}

pub struct MapPResParser<P, F, R, E, I>(P, F, PhantomData<(R, E, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn(PResult<P::Parsed, I, P::Error>) -> PResult<R, I, E>,
    E: Error<I>;

impl<P, F, R, E, I> Parse<I> for MapPResParser<P, F, R, E, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(PResult<P::Parsed, I, P::Error>) -> PResult<R, I, E>,
    E: Error<I>,
{
    type Parsed = R;
    type Error = E;

    fn parse(&self, input: I) -> PResult<R, I, E> {
        (self.1)(self.0.parse(input))
    }
}

pub const fn map_pres<P, F, R, E, I>(parser: P, map_pres_fn: F) -> MapPResParser<P, F, R, E, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn(PResult<P::Parsed, I, P::Error>) -> PResult<R, I, E>,
    E: Error<I>,
{
    MapPResParser(parser, map_pres_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TakeParser<I, E>(usize, PhantomData<(I, E)>)
where
    I: Input,
    E: Error<I>;

impl<I: Input, E: Error<I>> Parse<I> for TakeParser<I, E> {
    type Parsed = Span<I>;
    type Error = E;

    fn parse(&self, mut input: I) -> PResult<Span<I>, I, E> {
        let begin = input.clone();
        if input.advance_by(self.0) != self.0 {
            Err(Failure(E::need_more_input(input), begin))
        } else {
            Ok(Success(Span::new(begin, input.clone()), input))
        }
    }
}

pub const fn take<I, E>(count: usize) -> TakeParser<I, E>
where
    I: Input,
    E: Error<I>,
{
    TakeParser(count, PhantomData)
}

pub struct WithParser<P, F, T, I>(P, F, PhantomData<(T, I)>)
where
    P: Parse<I>,
    I: Input,
    F: Fn() -> T;

impl<P, F, T, I> Parse<I> for WithParser<P, F, T, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn() -> T,
{
    type Parsed = T;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(_, rem)) => Ok(Success((self.1)(), rem)),
            Err(fail) => Err(fail),
        }
    }
}

pub const fn with<P, F, T, I>(parser: P, with_fn: F) -> WithParser<P, F, T, I>
where
    P: Parse<I>,
    I: Input,
    F: Fn() -> T,
{
    WithParser(parser, with_fn, PhantomData)
}

pub struct WithDefaultParser<P, T, I>(P, PhantomData<(T, I)>)
where
    P: Parse<I>,
    I: Input,
    T: Default;

impl<P, T, I> Parse<I> for WithDefaultParser<P, T, I>
where
    P: Parse<I>,
    I: Input,
    T: Default,
{
    type Parsed = T;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<T, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(_, rem)) => Ok(Success(T::default(), rem)),
            Err(fail) => Err(fail),
        }
    }
}

pub const fn with_default<P, T, I>(parser: P) -> WithDefaultParser<P, T, I>
where
    P: Parse<I>,
    I: Input,
    T: Default,
{
    WithDefaultParser(parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct WithValueParser<P, T, I>(P, T, PhantomData<I>)
where
    P: Parse<I>,
    I: Input,
    T: Clone;

impl<P, T, I> Parse<I> for WithValueParser<P, T, I>
where
    P: Parse<I>,
    T: Clone,
    I: Input,
{
    type Parsed = T;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<T, I, Self::Error> {
        self.0.parse(input).map_parsed(|_| self.1.clone())
    }
}

pub const fn with_value<P, T, I>(parser: P, value: T) -> WithValueParser<P, T, I>
where
    P: Parse<I>,
    T: Clone,
    I: Input,
{
    WithValueParser(parser, value, PhantomData)
}

#[derive(Debug, Clone)]
pub struct FlatMapParser<P, C, R, I>(P, C, PhantomData<(R, I)>)
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> R,
    R: Parse<I, Error = P::Error>,
    I: Input;

impl<P, C, R, I> Parse<I> for FlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> R,
    R: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = R::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(arg, rem)) => match ((self.1)(arg)).parse(rem) {
                Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
                Err(Failure(err, _)) => Err(Failure(err, input)),
            },
            Err(Failure(err, _)) => Err(Failure(err, input)),
        }
    }
}

pub const fn flat_map<P, C, R, I>(parser: P, combinator: C) -> FlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> R,
    R: Parse<I, Error = P::Error>,
    I: Input,
{
    FlatMapParser(parser, combinator, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryFlatMapParser<P, C, R, I>(P, C, PhantomData<(R, I)>)
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> Result<R, P::Error>,
    R: Parse<I, Error = P::Error>,
    I: Input;

impl<P, C, R, I> Parse<I> for TryFlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> Result<R, P::Error>,
    R: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = R::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(arg, rem)) => match (self.1)(arg) {
                Ok(p) => match p.parse(rem) {
                    Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
                    Err(Failure(err, _)) => Err(Failure(err, input)),
                },
                Err(e) => Err(Failure(e, input)),
            },
            Err(Failure(err, _)) => Err(Failure(err, input)),
        }
    }
}

pub const fn try_flat_map<P, C, R, I>(parser: P, combinator: C) -> TryFlatMapParser<P, C, R, I>
where
    P: Parse<I>,
    C: Fn(P::Parsed) -> Result<R, P::Error>,
    R: Parse<I, Error = P::Error>,
    I: Input,
{
    TryFlatMapParser(parser, combinator, PhantomData)
}

#[derive(Debug, Clone)]
pub struct VerbatimParser<P, I, E>(P, PhantomData<(I, E)>)
where
    P: Input,
    I: Input,
    I::Symbol: PartialEq<P::Symbol>,
    E: Error<I>;

impl<P, I, E> Parse<I> for VerbatimParser<P, I, E>
where
    P: Input,
    I: Input,
    I::Symbol: PartialEq<P::Symbol>,
    E: Error<I>,
{
    type Parsed = Span<I>;
    type Error = E;

    fn parse(&self, mut input: I) -> PResult<Span<I>, I, E> {
        let mut pattern = self.0.clone();
        let orig_input = input.clone();
        while let Some(expected) = pattern.next() {
            let tmp = input.clone();
            let Some(symb) = input.next() else {
                return Err(Failure(E::need_more_input(input), orig_input));
            };
            if !PartialEq::eq(&symb, &expected) {
                return Err(Failure(E::invalid_input(tmp), orig_input));
            }
        }
        Ok(Success(Span::new(orig_input, input.clone()), input))
    }
}

pub const fn verbatim<P, I, E>(pattern: P) -> VerbatimParser<P, I, E>
where
    P: Input,
    I: Input,
    I::Symbol: PartialEq<P::Symbol>,
    E: Error<I>,
{
    VerbatimParser(pattern, PhantomData)
}

pub fn pop<I: Input, E: Error<I>>(mut input: I) -> PResult<I::Symbol, I, E> {
    if let Some(symb) = input.next() {
        Ok(Success(symb, input))
    } else {
        Err(Failure(E::need_more_input(input.clone()), input))
    }
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(_, rem)) => Ok(Success(self.1.clone(), rem)),
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(_, rem)) => Ok(Success(Default::default(), rem)),
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(ret, rem)) => Ok(Success(ret, rem)),
            Err(Failure(_, rem)) => Ok(Success((self.1)(), rem)),
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

pub struct Many0Iter<'a, P, I>(&'a P, &'a mut I)
where
    P: Parse<I>,
    I: Input;

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
            Err(_) => None,
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
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
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
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Result<R, P::Error>;

impl<P, F, R, I> Parse<I> for TryMany0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Result<R, P::Error>,
{
    type Parsed = R;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
        let orig_input = input.clone();
        match (self.1)(Many0Iter(&self.0, &mut input)) {
            Ok(ret) => {
                let _ = Many0Iter(&self.0, &mut input).count();
                Ok(Success(ret, input))
            }
            Err(err) => Err(Failure(err, orig_input)),
        }
    }
}

pub const fn try_many0<P, F, R, I>(parser: P, collect_fn: F) -> TryMany0Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many0Iter<'a, P, I>) -> Result<R, P::Error>,
{
    TryMany0Parser(parser, collect_fn, PhantomData)
}

pub struct Many0CollectParser<P, C, I>(P, PhantomData<(C, I)>)
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>;

impl<P, C, I> Parse<I> for Many0CollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    type Parsed = C;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<C, I, Self::Error> {
        let ret = C::from_iter(Many0Iter(&self.0, &mut input));
        let _ = Many0Iter(&self.0, &mut input).count();
        Ok(Success(ret, input))
    }
}

pub const fn many0_collect<P, C, I>(parser: P) -> Many0CollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    Many0CollectParser(parser, PhantomData)
}

pub struct Many1Iter<'a, P, I>(&'a P, &'a mut I, &'a mut bool, &'a mut Option<P::Error>)
where
    P: Parse<I>,
    I: Input;

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
            Err(Failure(e, _)) if !*self.2 => {
                *self.3 = Some(e);
                None
            }
            _ => None,
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
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
        let mut parsed = false;
        let mut err = None;
        let orig_input = input.clone();
        let ret = (self.1)(Many1Iter(&self.0, &mut input, &mut parsed, &mut err));
        let _ = Many1Iter(&self.0, &mut input, &mut parsed, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
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
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Result<R, P::Error>;

impl<P, F, R, I> Parse<I> for TryMany1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Result<R, P::Error>,
{
    type Parsed = R;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
        let mut parsed = false;
        let mut err = None;
        let orig_input = input.clone();
        let ret = match (self.1)(Many1Iter(&self.0, &mut input, &mut parsed, &mut err)) {
            Ok(ret) => ret,
            Err(e) => {
                return Err(Failure(e, orig_input));
            }
        };
        let _ = Many1Iter(&self.0, &mut input, &mut parsed, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn try_many1<P, F, R, I>(parser: P, collect_fn: F) -> TryMany1Parser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(Many1Iter<'a, P, I>) -> Result<R, P::Error>,
{
    TryMany1Parser(parser, collect_fn, PhantomData)
}

pub struct Many1CollectParser<P, C, I>(P, PhantomData<(C, I)>)
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>;

impl<P, C, I> Parse<I> for Many1CollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    type Parsed = C;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<C, I, Self::Error> {
        let mut parsed = false;
        let mut err = None;
        let orig_input = input.clone();
        let ret = C::from_iter(Many1Iter(&self.0, &mut input, &mut parsed, &mut err));
        let _ = Many1Iter(&self.0, &mut input, &mut parsed, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn many1_collect<P, C, I>(parser: P) -> Many1CollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    Many1CollectParser(parser, PhantomData)
}

pub struct RepeatedIter<'a, P, I>(&'a P, &'a mut I, &'a mut usize, &'a mut Option<P::Error>)
where
    P: Parse<I>,
    I: Input;

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
            Err(Failure(e, _)) => {
                *self.3 = Some(e);
                None
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(*self.2))
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
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
        let mut remaining = self.1;
        let mut err = None;
        let orig_input = input.clone();
        let ret = (self.2)(RepeatedIter(&self.0, &mut input, &mut remaining, &mut err));
        let _ = RepeatedIter(&self.0, &mut input, &mut remaining, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
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
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Result<R, P::Error>;

impl<P, F, R, I> Parse<I> for TryRepeatedParser<P, F, R, I>
where
    P: Parse<I>,
    I: Input,
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Result<R, P::Error>,
{
    type Parsed = R;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<R, I, Self::Error> {
        let mut remaining = self.1;
        let mut err = None;
        let orig_input = input.clone();
        let ret = match (self.2)(RepeatedIter(&self.0, &mut input, &mut remaining, &mut err)) {
            Ok(ret) => ret,
            Err(e) => {
                return Err(Failure(e, orig_input));
            }
        };
        let _ = RepeatedIter(&self.0, &mut input, &mut remaining, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
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
    F: for<'a> Fn(RepeatedIter<'a, P, I>) -> Result<R, P::Error>,
{
    TryRepeatedParser(parser, count, collect_fn, PhantomData)
}

pub struct RepeatedCollectParser<P, C, I>(P, usize, PhantomData<(C, I)>)
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>;

impl<P, C, I> Parse<I> for RepeatedCollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    type Parsed = C;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<C, I, Self::Error> {
        let mut remaining = self.1;
        let mut err = None;
        let orig_input = input.clone();
        let ret = C::from_iter(RepeatedIter(&self.0, &mut input, &mut remaining, &mut err));
        let _ = RepeatedIter(&self.0, &mut input, &mut remaining, &mut err).count();
        if let Some(err) = err {
            return Err(Failure(err, orig_input));
        }
        Ok(Success(ret, input))
    }
}

pub const fn repeated_collect<P, C, I>(parser: P, count: usize) -> RepeatedCollectParser<P, C, I>
where
    P: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    RepeatedCollectParser(parser, count, PhantomData)
}

pub struct ManyUntilIter<'a, P, Q, I>(
    &'a P,
    &'a Q,
    &'a mut I,
    &'a mut Option<Result<Q::Parsed, P::Error>>,
)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<'a, P, Q, I> Iterator for ManyUntilIter<'a, P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    type Item = P::Parsed;

    fn next(&mut self) -> Option<Self::Item> {
        if self.3.is_some() {
            return None;
        }

        match self.1.parse(self.2.clone()) {
            Ok(Success(sent, rem)) => {
                *self.3 = Some(Ok(sent));
                *self.2 = rem;
                None
            }
            Err(Failure(err, rem)) => match self.0.parse(rem) {
                Ok(Success(ret, rem)) => {
                    *self.2 = rem;
                    Some(ret)
                }
                Err(_) => {
                    *self.3 = Some(Err(err));
                    None
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ManyUntilParser<P, Q, F, R, I>(P, Q, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input;

impl<P, Q, F, R, I> Parse<I> for ManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input,
{
    type Parsed = (R, Q::Parsed);
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let mut sentinel = None;
        let orig_input = input.clone();
        let ret = (self.2)(ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel));
        let _ = ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel).count();
        match sentinel {
            Some(Ok(sentinel)) => Ok(Success((ret, sentinel), input)),
            Some(Err(err)) => Err(Failure(err, orig_input)),
            None => unreachable!(),
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
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> R,
    I: Input,
{
    ManyUntilParser(parser, sentinel_parser, collect_fn, PhantomData)
}

#[derive(Debug, Clone)]
pub struct TryManyUntilParser<P, Q, F, R, I>(P, Q, F, PhantomData<(R, I)>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Result<R, P::Error>,
    I: Input;

impl<P, Q, F, R, I> Parse<I> for TryManyUntilParser<P, Q, F, R, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Result<R, P::Error>,
    I: Input,
{
    type Parsed = (R, Q::Parsed);
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let mut sentinel = None;
        let orig_input = input.clone();
        let ret = match (self.2)(ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel)) {
            Ok(ret) => ret,
            Err(e) => {
                return Err(Failure(e, orig_input));
            }
        };
        let _ = ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel).count();
        match sentinel {
            Some(Ok(sentinel)) => Ok(Success((ret, sentinel), input)),
            Some(Err(err)) => Err(Failure(err, orig_input)),
            None => unreachable!(),
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
    Q: Parse<I, Error = P::Error>,
    F: for<'a> Fn(ManyUntilIter<'a, P, Q, I>) -> Result<R, P::Error>,
    I: Input,
{
    TryManyUntilParser(parser, sentinel_parser, collect_fn, PhantomData)
}

pub struct ManyUntilCollectParser<P, Q, C, I>(P, Q, PhantomData<(C, I)>)
where
    P: Parse<I>,
    Q: Parse<I>,
    I: Input,
    C: FromIterator<P::Parsed>;

impl<P, Q, C, I> Parse<I> for ManyUntilCollectParser<P, Q, C, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    type Parsed = (C, Q::Parsed);
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let mut sentinel = None;
        let orig_input = input.clone();
        let ret = C::from_iter(ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel));
        let _ = ManyUntilIter(&self.0, &self.1, &mut input, &mut sentinel).count();
        match sentinel {
            Some(Ok(sentinel)) => Ok(Success((ret, sentinel), input)),
            Some(Err(err)) => Err(Failure(err, orig_input)),
            None => unreachable!(),
        }
    }
}

pub const fn many_until_collect<P, Q, C, I>(
    parser: P,
    sentinel: Q,
) -> ManyUntilCollectParser<P, Q, C, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    ManyUntilCollectParser(parser, sentinel, PhantomData)
}

pub struct ManyRangeIter<'a, P, I>(
    &'a P,
    Option<usize>,
    &'a mut I,
    &'a mut usize,
    &'a mut Option<P::Error>,
)
where
    P: Parse<I>,
    I: Input;

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
            Err(Failure(e, _)) => {
                *self.4 = Some(e);
                None
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            0,
            if let Some(max_count) = self.1 {
                Some(max_count - *self.3)
            } else {
                None
            },
        )
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
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<O, I, Self::Error> {
        let max_count = match self.1.end_bound() {
            core::ops::Bound::Included(max_count) => Some(*max_count),
            core::ops::Bound::Excluded(&0) => Some(0),
            core::ops::Bound::Excluded(max_count) => Some(*max_count - 1),
            _ => None,
        };

        let mut count = 0usize;
        let mut err = None;
        let orig_input = input.clone();
        let ret = (self.2)(ManyRangeIter(
            &self.0, max_count, &mut input, &mut count, &mut err,
        ));
        let _ = ManyRangeIter(&self.0, max_count, &mut input, &mut count, &mut err).count();

        match self.1.start_bound() {
            core::ops::Bound::Included(min_count) if count < *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
            }
            core::ops::Bound::Excluded(min_count) if count <= *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
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
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Result<O, P::Error>,
    I: Input;

impl<P, R, F, O, I> Parse<I> for TryManyRangeParser<P, R, F, O, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Result<O, P::Error>,
    I: Input,
{
    type Parsed = O;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<O, I, Self::Error> {
        let max_count = match self.1.end_bound() {
            core::ops::Bound::Included(max_count) => Some(*max_count),
            core::ops::Bound::Excluded(&0) => Some(0),
            core::ops::Bound::Excluded(max_count) => Some(*max_count - 1),
            _ => None,
        };

        let mut count = 0usize;
        let mut err = None;
        let orig_input = input.clone();
        let ret = match (self.2)(ManyRangeIter(
            &self.0, max_count, &mut input, &mut count, &mut err,
        )) {
            Ok(ret) => ret,
            Err(e) => {
                return Err(Failure(e, orig_input));
            }
        };
        let _ = ManyRangeIter(&self.0, max_count, &mut input, &mut count, &mut err).count();

        match self.1.start_bound() {
            core::ops::Bound::Included(min_count) if count < *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
            }
            core::ops::Bound::Excluded(min_count) if count <= *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
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
    F: for<'a> Fn(ManyRangeIter<'a, P, I>) -> Result<O, P::Error>,
    I: Input,
{
    TryManyRangeParser(parser, range, collect_fn, PhantomData)
}

pub struct ManyRangeCollectParser<P, R, C, I>(P, R, PhantomData<(C, I)>)
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    I: Input,
    C: FromIterator<P::Parsed>;

impl<P, R, C, I> Parse<I> for ManyRangeCollectParser<P, R, C, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    type Parsed = C;
    type Error = P::Error;

    fn parse(&self, mut input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let max_count = match self.1.end_bound() {
            core::ops::Bound::Included(max_count) => Some(*max_count),
            core::ops::Bound::Excluded(&0) => Some(0),
            core::ops::Bound::Excluded(max_count) => Some(*max_count - 1),
            _ => None,
        };

        let mut count = 0usize;
        let mut err = None;
        let orig_input = input.clone();
        let ret = C::from_iter(ManyRangeIter(
            &self.0, max_count, &mut input, &mut count, &mut err,
        ));
        let _ = ManyRangeIter(&self.0, max_count, &mut input, &mut count, &mut err).count();

        match self.1.start_bound() {
            core::ops::Bound::Included(min_count) if count < *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
            }
            core::ops::Bound::Excluded(min_count) if count <= *min_count => {
                if let Some(err) = err {
                    Err(Failure(err, orig_input))
                } else {
                    unreachable!()
                }
            }
            _ => Ok(Success(ret, input)),
        }
    }
}

pub const fn many_range_collect<P, R, C, I>(
    parser: P,
    range: R,
) -> ManyRangeCollectParser<P, R, C, I>
where
    P: Parse<I>,
    R: core::ops::RangeBounds<usize>,
    I: Input,
    C: FromIterator<P::Parsed>,
{
    ManyRangeCollectParser(parser, range, PhantomData)
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let mut rem = input.clone();
        let mut arr: [core::mem::MaybeUninit<P::Parsed>; LEN] =
            [const { core::mem::MaybeUninit::uninit() }; LEN];
        for idx in 0..LEN {
            match self.0.parse(rem) {
                Ok(Success(val, new_rem)) => {
                    arr[idx].write(val);
                    rem = new_rem;
                }
                Err(Failure(e, _)) => {
                    unsafe {
                        for i in 0..idx {
                            arr[i].assume_init_drop();
                        }
                    }
                    return Err(Failure(e, input));
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

pub fn eof<I: Input, E: Error<I>>(input: I) -> PResult<(), I, E> {
    if let Some(_) = input.clone().next() {
        Err(Failure(E::expected_eof(input.clone()), input))
    } else {
        Ok(Success((), input))
    }
}

#[derive(Debug, Clone)]
pub struct CompleteParser<P, I>(P, PhantomData<I>)
where
    P: Parse<I>,
    I: Input;

impl<P, I> Parse<I> for CompleteParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    type Parsed = P::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        let Success(val, rem) = self.0.parse(input.clone())?;
        if let Some(_) = rem.clone().next() {
            Err(Failure(Error::expected_eof(rem), input))
        } else {
            Ok(Success(val, rem))
        }
    }
}

pub const fn complete<P, I>(parser: P) -> CompleteParser<P, I>
where
    P: Parse<I>,
    I: Input,
{
    CompleteParser(parser, PhantomData)
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, rem)) if (self.1)(&val) => Ok(Success(val, rem)),
            Ok(_) => Err(Failure(Error::invalid_input(input.clone()), input)),
            Err(Failure(e, _)) => Err(Failure(e, input)),
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<(), I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(_) => Err(Failure(Error::invalid_input(input.clone()), input)),
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(val, rem)) => Ok(Success(Some(val), rem)),
            Err(Failure(_, rem)) => Ok(Success(None, rem)),
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, _)) => Ok(Success(val, input)),
            Err(Failure(e, _)) => Err(Failure(e, input)),
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

pub fn remaining<I: Input, E: Error<I>>(mut input: I) -> PResult<I, I, E> {
    let ret = input.clone();
    input.advance_by(usize::MAX);
    Ok(Success(ret, input))
}

pub fn remaining_len<I: Input, E: Error<I>>(input: I) -> PResult<usize, I, E> {
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
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
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
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<P, Q, I> Parse<I> for PrefixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = Q::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(_, rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => Ok(Success(val, rem)),
                Err(Failure(e, _)) => Err(Failure(e, input)),
            },
            Err(Failure(e, _)) => Err(Failure(e, input)),
        }
    }
}

pub const fn prefix<P, Q, I>(prefix: P, parser: Q) -> PrefixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    PrefixParser(prefix, parser, PhantomData)
}

#[derive(Debug, Clone)]
pub struct SuffixParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<P, Q, I> Parse<I> for SuffixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = P::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(val, rem)) => match self.1.parse(rem) {
                Ok(Success(_, rem)) => Ok(Success(val, rem)),
                Err(Failure(e, _)) => Err(Failure(e, input)),
            },
            Err(Failure(e, _)) => Err(Failure(e, input)),
        }
    }
}

pub const fn suffix<P, Q, I>(parser: P, suffix: Q) -> SuffixParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    SuffixParser(parser, suffix, PhantomData)
}

#[derive(Debug, Clone)]
pub struct DelimitedParser<P, Q, S, I>(P, Q, S, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    S: Parse<I, Error = P::Error>,
    I: Input;

impl<P, Q, S, I> Parse<I> for DelimitedParser<P, Q, S, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    S: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = Q::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(_, rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => match self.2.parse(rem) {
                    Ok(Success(_, rem)) => Ok(Success(val, rem)),
                    Err(Failure(e, _)) => Err(Failure(e, input)),
                },
                Err(Failure(e, _)) => Err(Failure(e, input)),
            },
            Err(Failure(e, _)) => Err(Failure(e, input)),
        }
    }
}

pub const fn delimited<P, Q, S, I>(prefix: P, parser: Q, suffix: S) -> DelimitedParser<P, Q, S, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    S: Parse<I, Error = P::Error>,
    I: Input,
{
    DelimitedParser(prefix, parser, suffix, PhantomData)
}

#[derive(Debug, Clone)]
pub struct SeparatedParser<P, S, Q, I>(P, S, Q, PhantomData<I>)
where
    P: Parse<I>,
    S: Parse<I, Error = P::Error>,
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<P, S, Q, I> Parse<I> for SeparatedParser<P, S, Q, I>
where
    P: Parse<I>,
    S: Parse<I, Error = P::Error>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = (P::Parsed, Q::Parsed);
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(first, rem)) => match self.1.parse(rem) {
                Ok(Success(_, rem)) => match self.2.parse(rem) {
                    Ok(Success(second, rem)) => Ok(Success((first, second), rem)),
                    Err(Failure(e, _)) => Err(Failure(e, input)),
                },
                Err(Failure(e, _)) => Err(Failure(e, input)),
            },
            Err(Failure(e, _)) => Err(Failure(e, input)),
        }
    }
}

pub const fn separated<P, S, Q, I>(first: P, separator: S, second: Q) -> SeparatedParser<P, S, Q, I>
where
    P: Parse<I>,
    S: Parse<I, Error = P::Error>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    SeparatedParser(first, separator, second, PhantomData)
}

pub struct PairParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<P, Q, I> Parse<I> for PairParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    type Parsed = (P::Parsed, Q::Parsed);
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input.clone()) {
            Ok(Success(first, rem)) => match self.1.parse(rem) {
                Ok(Success(second, rem)) => Ok(Success((first, second), rem)),
                Err(Failure(e, _)) => Err(Failure(e, input)),
            },
            Err(Failure(e, _)) => Err(Failure(e, input)),
        }
    }
}

pub const fn pair<P, Q, I>(first: P, second: Q) -> PairParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    PairParser(first, second, PhantomData)
}

pub struct EitherParser<P, Q, I>(P, Q, PhantomData<I>)
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input;

impl<P, Q, I> Parse<I> for EitherParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Parsed = P::Parsed, Error = P::Error>,
    I: Input,
{
    type Parsed = P::Parsed;
    type Error = P::Error;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error> {
        match self.0.parse(input) {
            Ok(Success(val, rem)) => Ok(Success(val, rem)),
            Err(Failure(_, rem)) => match self.1.parse(rem) {
                Ok(Success(val, rem)) => Ok(Success(val, rem)),
                Err(Failure(e, rem)) => Err(Failure(e, rem)),
            },
        }
    }
}

pub const fn either<P, Q, I>(first: P, second: Q) -> EitherParser<P, Q, I>
where
    P: Parse<I>,
    Q: Parse<I, Error = P::Error>,
    I: Input,
{
    EitherParser(first, second, PhantomData)
}
