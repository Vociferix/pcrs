#![no_std]

mod input;
mod span;

pub mod ascii;
pub mod basic;
pub mod bytes;
pub mod unicode;

pub use input::*;
pub use span::*;

#[derive(Debug, Clone)]
pub struct Success<T, I: Input>(pub T, pub I);

#[derive(Debug, Clone)]
pub struct Failure<I: Input>(pub I);

pub type PResult<T, I> = Result<Success<T, I>, Failure<I>>;

pub trait Parse<I: Input>: Sized {
    type Parsed: Sized;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I>;

    fn as_ref<'a>(&'a self) -> basic::FromRefParser<'a, Self, I> {
        basic::from_ref(self)
    }

    fn into_fn(self) -> impl Fn(I) -> PResult<Self::Parsed, I> {
        move |input| self.parse(input)
    }

    fn into_input(self, input: I) -> ParsedInput<Self, I>
    where
        Self: Clone,
    {
        ParsedInput::new(self, input)
    }

    fn map<F, R>(self, map_fn: F) -> basic::MapParser<Self, F, R, I>
    where
        F: Fn(Self::Parsed) -> R,
    {
        basic::map(self, map_fn)
    }

    fn try_map<F, R>(self, try_map_fn: F) -> basic::TryMapParser<Self, F, R, I>
    where
        F: Fn(Self::Parsed) -> Option<R>,
    {
        basic::try_map(self, try_map_fn)
    }

    fn flat_map<F, R>(self, combinator: F) -> basic::FlatMapParser<Self, F, R, I>
    where
        F: Fn(Self::Parsed) -> R,
        R: Parse<I>,
    {
        basic::flat_map(self, combinator)
    }

    fn try_flat_map<F, R>(self, combinator: F) -> basic::TryFlatMapParser<Self, F, R, I>
    where
        F: Fn(Self::Parsed) -> Option<R>,
        R: Parse<I>,
    {
        basic::try_flat_map(self, combinator)
    }

    fn and_then<F, R>(self, and_then_fn: F) -> basic::AndThenParser<Self, F, R, I>
    where
        F: Fn(Self::Parsed) -> Option<R>,
    {
        basic::and_then(self, and_then_fn)
    }

    fn or_value(self, value: Self::Parsed) -> basic::OrValueParser<Self, I>
    where
        Self::Parsed: Clone,
    {
        basic::or_value(self, value)
    }

    fn or_default(self) -> basic::OrDefaultParser<Self, I>
    where
        Self::Parsed: Default,
    {
        basic::or_default(self)
    }

    fn or_else<F>(self, or_else_fn: F) -> basic::OrElseParser<Self, F, I>
    where
        F: Fn() -> Self::Parsed,
    {
        basic::or_else(self, or_else_fn)
    }

    fn value<T>(self, value: T) -> basic::ValueParser<Self, T, I>
    where
        T: Clone,
    {
        basic::value(self, value)
    }

    fn many0<F, R>(self, collect_fn: F) -> basic::Many0Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many0Iter<'a, Self, I>) -> R,
    {
        basic::many0(self, collect_fn)
    }

    fn try_many0<F, R>(self, collect_fn: F) -> basic::TryMany0Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many0Iter<'a, Self, I>) -> Option<R>,
    {
        basic::try_many0(self, collect_fn)
    }

    fn many1<F, R>(self, collect_fn: F) -> basic::Many1Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many1Iter<'a, Self, I>) -> R,
    {
        basic::many1(self, collect_fn)
    }

    fn try_many1<F, R>(self, collect_fn: F) -> basic::TryMany1Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many1Iter<'a, Self, I>) -> Option<R>,
    {
        basic::try_many1(self, collect_fn)
    }

    fn many_until<P, F, R>(
        self,
        sentinel_parser: P,
        collect_fn: F,
    ) -> basic::ManyUntilParser<Self, P, F, R, I>
    where
        P: Parse<I>,
        F: for<'a> Fn(basic::ManyUntilIter<'a, Self, P, I>) -> R,
    {
        basic::many_until(self, sentinel_parser, collect_fn)
    }

    fn try_many_until<P, F, R>(
        self,
        sentinel_parser: P,
        collect_fn: F,
    ) -> basic::TryManyUntilParser<Self, P, F, R, I>
    where
        P: Parse<I>,
        F: for<'a> Fn(basic::ManyUntilIter<'a, Self, P, I>) -> Option<R>,
    {
        basic::try_many_until(self, sentinel_parser, collect_fn)
    }

    fn many_range<R, F, O>(
        self,
        range: R,
        collect_fn: F,
    ) -> basic::ManyRangeParser<Self, R, F, O, I>
    where
        R: core::ops::RangeBounds<usize>,
        F: for<'a> Fn(basic::ManyRangeIter<'a, Self, I>) -> O,
    {
        basic::many_range(self, range, collect_fn)
    }

    fn try_many_range<R, F, O>(
        self,
        range: R,
        collect_fn: F,
    ) -> basic::TryManyRangeParser<Self, R, F, O, I>
    where
        R: core::ops::RangeBounds<usize>,
        F: for<'a> Fn(basic::ManyRangeIter<'a, Self, I>) -> Option<O>,
    {
        basic::try_many_range(self, range, collect_fn)
    }

    fn repeated<F, R>(self, count: usize, collect_fn: F) -> basic::RepeatedParser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::RepeatedIter<'a, Self, I>) -> R,
    {
        basic::repeated(self, count, collect_fn)
    }

    fn try_repeated<F, R>(
        self,
        count: usize,
        collect_fn: F,
    ) -> basic::TryRepeatedParser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::RepeatedIter<'a, Self, I>) -> Option<R>,
    {
        basic::try_repeated(self, count, collect_fn)
    }

    fn array<const LEN: usize>(self) -> basic::ArrayParser<Self, I, LEN> {
        basic::array(self)
    }

    fn prefix_with<P>(self, prefix: P) -> basic::PrefixParser<P, Self, I>
    where
        P: Parse<I>,
    {
        basic::prefix(prefix, self)
    }

    fn prefixes<P>(self, parser: P) -> basic::PrefixParser<Self, P, I>
    where
        P: Parse<I>,
    {
        basic::prefix(self, parser)
    }

    fn suffix_with<P>(self, suffix: P) -> basic::SuffixParser<Self, P, I>
    where
        P: Parse<I>,
    {
        basic::suffix(self, suffix)
    }

    fn suffixes<P>(self, parser: P) -> basic::SuffixParser<P, Self, I>
    where
        P: Parse<I>,
    {
        basic::suffix(parser, self)
    }

    fn delimited_with<P, Q>(self, prefix: P, suffix: Q) -> basic::DelimitedParser<P, Self, Q, I>
    where
        P: Parse<I>,
        Q: Parse<I>,
    {
        basic::delimited(prefix, self, suffix)
    }

    fn separated_with<P, Q>(self, separator: P, next: Q) -> basic::SeparatedParser<Self, P, Q, I>
    where
        P: Parse<I>,
        Q: Parse<I>,
    {
        basic::separated(self, separator, next)
    }

    fn separates<P, Q>(self, first: P, second: Q) -> basic::SeparatedParser<P, Self, Q, I>
    where
        P: Parse<I>,
        Q: Parse<I>,
    {
        basic::separated(first, self, second)
    }

    fn spanned(self) -> basic::SpannedParser<Self, I> {
        basic::spanned(self)
    }

    fn all_consuming(self) -> basic::AllConsumingParser<Self, I> {
        basic::all_consuming(self)
    }

    fn recognize(self) -> basic::RecognizeParser<Self, I> {
        basic::recognize(self)
    }

    fn cond(self, condition: bool) -> basic::CondParser<Self, I> {
        basic::cond(self, condition)
    }

    fn verify<F>(self, verify_fn: F) -> basic::VerifyParser<Self, F, I>
    where
        F: Fn(&Self::Parsed) -> bool,
    {
        basic::verify(self, verify_fn)
    }

    fn not(self) -> basic::NotParser<Self, I> {
        basic::not(self)
    }

    fn opt(self) -> basic::OptParser<Self, I> {
        basic::opt(self)
    }

    fn peek(self) -> basic::PeekParser<Self, I> {
        basic::peek(self)
    }
}

mod sealed {
    use super::{Input, PResult};

    pub trait Sealed {}

    impl<T, I: Input> Sealed for PResult<T, I> {}
}

pub trait PResultExt: sealed::Sealed {
    type Parsed;
    type Input: Input;

    fn success(parsed: Self::Parsed, remaining: Self::Input) -> Self;

    fn failure(remaining: Self::Input) -> Self;

    fn remaining(&self) -> &Self::Input;

    fn remaining_mut(&mut self) -> &mut Self::Input;

    fn parsed(&self) -> Option<&Self::Parsed>;

    fn parsed_mut(&mut self) -> Option<&mut Self::Parsed>;

    fn extract(self) -> (Option<Self::Parsed>, Self::Input);

    fn map_parsed<F, R>(self, map_fn: F) -> PResult<R, Self::Input>
    where
        F: FnOnce(Self::Parsed) -> R;
}

impl<T, I: Input> PResultExt for PResult<T, I> {
    type Parsed = T;
    type Input = I;

    fn success(parsed: T, remaining: I) -> Self {
        Ok(Success(parsed, remaining))
    }

    fn failure(remaining: I) -> Self {
        Err(Failure(remaining))
    }

    fn remaining(&self) -> &I {
        match self {
            Ok(Success(_, rem)) => rem,
            Err(Failure(rem)) => rem,
        }
    }

    fn remaining_mut(&mut self) -> &mut I {
        match self {
            Ok(succ) => &mut succ.1,
            Err(Failure(rem)) => rem,
        }
    }

    fn parsed(&self) -> Option<&Self::Parsed> {
        if let Ok(Success(val, _)) = self {
            Some(val)
        } else {
            None
        }
    }

    fn parsed_mut(&mut self) -> Option<&mut Self::Parsed> {
        if let Ok(succ) = self {
            Some(&mut succ.0)
        } else {
            None
        }
    }

    fn extract(self) -> (Option<T>, I) {
        match self {
            Ok(Success(val, rem)) => (Some(val), rem),
            Err(Failure(rem)) => (None, rem),
        }
    }

    fn map_parsed<F, R>(self, map_fn: F) -> PResult<R, I>
    where
        F: FnOnce(Self::Parsed) -> R,
    {
        self.map(move |succ| succ.map(map_fn))
    }
}

impl<F, I, R> Parse<I> for F
where
    F: Fn(I) -> PResult<R, I>,
    I: Input,
{
    type Parsed = R;

    fn parse(&self, input: I) -> PResult<Self::Parsed, I> {
        (*self)(input)
    }
}

impl<T, I: Input> Success<T, I> {
    pub fn map<F, R>(self, map_fn: F) -> Success<R, I>
    where
        F: FnOnce(T) -> R,
    {
        let Success(val, rem) = self;
        Success(map_fn(val), rem)
    }
}

impl<T, U: From<T>, I: Input> From<Success<T, I>> for PResult<U, I> {
    fn from(Success(val, rem): Success<T, I>) -> Self {
        Ok(Success(U::from(val), rem))
    }
}

impl<T, I: Input> From<Failure<I>> for PResult<T, I> {
    fn from(f: Failure<I>) -> Self {
        Err(f)
    }
}
