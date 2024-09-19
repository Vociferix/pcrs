#![no_std]

mod input;
mod span;

pub mod ascii;
pub mod basic;
pub mod bytes;
pub mod unicode;

pub use input::*;
pub use span::*;

/// A parsing error.
pub trait Error<I: Input>: Sized {
    /// Creates an error representing the need for more input.
    ///
    /// Generic parsers will call this method to generate an error when parsing failed
    /// due to reaching the end of input.
    fn need_more_input(pos: I) -> Self;

    /// Creates an error representing that the end of input was expected but not reached.
    ///
    /// Generic parsers will call this method to generate an error when parsing failed
    /// because it expected to reach the end of the input, but there was more input
    /// available after parsing was finished.
    fn expected_eof(pos: I) -> Self;

    /// Creates an error representing that the input is unparsable.
    ///
    /// Generic parsers will call this method to generate an error when parsing failed
    /// due to a failed verification of a symbol or parsed value.
    fn invalid_input(pos: I) -> Self;

    /// Gets the input position where the error occured.
    fn position(&self) -> &I;
}

/// Type returned by a parser when parsing succeeds.
///
/// [`Success`] is returned in the [`Ok`] variant of a [`PResult`] to signal that
/// parsing succeeded. [`Success`] is a named tuple containing the parsed value
/// (see [`Parse::Parsed`]) and the remaining unparsed input.
#[derive(Debug, Clone)]
pub struct Success<T, I: Input>(pub T, pub I);

/// Type returned by a parser when parsing fails.
///
/// [`Failure`] is returned in the [`Err`] variant of a [`PResult`] to signal that
/// parsing failed. [`Failure`] is a named tuple containing the parsing error
/// (see [`Error`] and [`Parse::Error`]) and the input that failed to parse. The
/// input member should always have the same position as the input that was
/// originally provided to the parser - failure to do so is a bug, but not UB.
#[derive(Debug, Clone)]
pub struct Failure<E: Error<I>, I: Input>(pub E, pub I);

/// The [`Result`] type returned by a parser.
///
/// All parsers return either a [`Success`] when parsing succeeds, or a
/// [`Failure`] when parsing fails. The input type that is returned is the same
/// in either case, and is the same type as was provided to the parser to parse.
///
/// Unlike a typical [`Result`] alias in rust, [`PResult`] has three generic
/// parameters: The parsed value type, the input type, and the error type. Since
/// this differs from the standard convention, the alias has a slightly different
/// name to clearly indicate the break from convention.
pub type PResult<T, I, E> = Result<Success<T, I>, Failure<E, I>>;

/// Trait implemented by all parsers.
///
/// The [`Parse`] trait is where the logic of parsing is implemented. However,
/// most `pcrs` users will not implement [`Parse`] directly. See the crate level
/// documentation for examples of user defined parsers.
///
/// In addition to implementing parsing logic in [`parse`](Parse::parse),
/// [`Parse`] provides many combinator monads. These are all also available as
/// freestanding `const` functions in the [`basic`] module.
pub trait Parse<I: Input>: Sized {
    /// The value type that is produced by the parser on success.
    type Parsed: Sized;

    /// The error type that is produced by the parser on failure.
    type Error: Error<I>;

    /// Parses the provided input.
    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error>;

    fn into_fn(self) -> impl Fn(I) -> PResult<Self::Parsed, I, Self::Error> {
        move |input| self.parse(input)
    }

    fn as_ref<'a>(&'a self) -> basic::FromRefParser<'a, Self, I> {
        basic::from_ref(self)
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
        F: Fn(Self::Parsed) -> Result<R, Self::Error>,
    {
        basic::try_map(self, try_map_fn)
    }

    fn map_err<F, R>(self, map_err_fn: F) -> basic::MapErrParser<Self, F, R, I>
    where
        F: Fn(Self::Error) -> R,
        R: Error<I>,
    {
        basic::map_err(self, map_err_fn)
    }

    fn map_res<F, R, E>(self, map_res_fn: F) -> basic::MapResParser<Self, F, R, E, I>
    where
        F: Fn(Result<Self::Parsed, Self::Error>) -> Result<R, E>,
        E: Error<I>,
    {
        basic::map_res(self, map_res_fn)
    }

    fn map_pres<F, R, E>(self, map_pres_fn: F) -> basic::MapPResParser<Self, F, R, E, I>
    where
        F: Fn(PResult<Self::Parsed, I, Self::Error>) -> PResult<R, I, E>,
        E: Error<I>,
    {
        basic::map_pres(self, map_pres_fn)
    }

    fn with<F, T>(self, with_fn: F) -> basic::WithParser<Self, F, T, I>
    where
        F: Fn() -> T,
    {
        basic::with(self, with_fn)
    }

    fn with_default<T>(self) -> basic::WithDefaultParser<Self, T, I>
    where
        T: Default,
    {
        basic::with_default(self)
    }

    fn with_value<T>(self, value: T) -> basic::WithValueParser<Self, T, I>
    where
        T: Clone,
    {
        basic::with_value(self, value)
    }

    fn flat_map<C, R>(self, combinator: C) -> basic::FlatMapParser<Self, C, R, I>
    where
        C: Fn(Self::Parsed) -> R,
        R: Parse<I, Error = Self::Error>,
    {
        basic::flat_map(self, combinator)
    }

    fn try_flat_map<C, R>(self, combinator: C) -> basic::TryFlatMapParser<Self, C, R, I>
    where
        C: Fn(Self::Parsed) -> Result<R, Self::Error>,
        R: Parse<I, Error = Self::Error>,
    {
        basic::try_flat_map(self, combinator)
    }

    fn or_else<F>(self, or_else_fn: F) -> basic::OrElseParser<Self, F, I>
    where
        F: Fn() -> Self::Parsed,
    {
        basic::or_else(self, or_else_fn)
    }

    fn or_default(self) -> basic::OrDefaultParser<Self, I>
    where
        Self::Parsed: Default,
    {
        basic::or_default(self)
    }

    fn or_value(self, value: Self::Parsed) -> basic::OrValueParser<Self, I>
    where
        Self::Parsed: Clone,
    {
        basic::or_value(self, value)
    }

    fn many0<F, R>(self, collect_fn: F) -> basic::Many0Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many0Iter<'a, Self, I>) -> R,
    {
        basic::many0(self, collect_fn)
    }

    fn try_many0<F, R>(self, collect_fn: F) -> basic::TryMany0Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many0Iter<'a, Self, I>) -> Result<R, Self::Error>,
    {
        basic::try_many0(self, collect_fn)
    }

    fn many0_collect<C>(self) -> basic::Many0CollectParser<Self, C, I>
    where
        C: core::iter::FromIterator<Self::Parsed>,
    {
        basic::many0_collect(self)
    }

    fn many1<F, R>(self, collect_fn: F) -> basic::Many1Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many1Iter<'a, Self, I>) -> R,
    {
        basic::many1(self, collect_fn)
    }

    fn try_many1<F, R>(self, collect_fn: F) -> basic::TryMany1Parser<Self, F, R, I>
    where
        F: for<'a> Fn(basic::Many1Iter<'a, Self, I>) -> Result<R, Self::Error>,
    {
        basic::try_many1(self, collect_fn)
    }

    fn many1_collect<C>(self) -> basic::Many1CollectParser<Self, C, I>
    where
        C: core::iter::FromIterator<Self::Parsed>,
    {
        basic::many1_collect(self)
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
        F: for<'a> Fn(basic::RepeatedIter<'a, Self, I>) -> Result<R, Self::Error>,
    {
        basic::try_repeated(self, count, collect_fn)
    }

    fn repeated_collect<C>(self, count: usize) -> basic::RepeatedCollectParser<Self, C, I>
    where
        C: core::iter::FromIterator<Self::Parsed>,
    {
        basic::repeated_collect(self, count)
    }

    fn many_until<P, F, R>(
        self,
        sentinel: P,
        collect_fn: F,
    ) -> basic::ManyUntilParser<Self, P, F, R, I>
    where
        P: Parse<I, Error = Self::Error>,
        F: for<'a> Fn(basic::ManyUntilIter<'a, Self, P, I>) -> R,
    {
        basic::many_until(self, sentinel, collect_fn)
    }

    fn try_many_until<P, F, R>(
        self,
        sentinel: P,
        collect_fn: F,
    ) -> basic::TryManyUntilParser<Self, P, F, R, I>
    where
        P: Parse<I, Error = Self::Error>,
        F: for<'a> Fn(basic::ManyUntilIter<'a, Self, P, I>) -> Result<R, Self::Error>,
    {
        basic::try_many_until(self, sentinel, collect_fn)
    }

    fn many_until_collect<P, C>(self, sentinel: P) -> basic::ManyUntilCollectParser<Self, P, C, I>
    where
        P: Parse<I, Error = Self::Error>,
        C: core::iter::FromIterator<Self::Parsed>,
    {
        basic::many_until_collect(self, sentinel)
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
        F: for<'a> Fn(basic::ManyRangeIter<'a, Self, I>) -> Result<O, Self::Error>,
    {
        basic::try_many_range(self, range, collect_fn)
    }

    fn many_range_collect<R, C>(self, range: R) -> basic::ManyRangeCollectParser<Self, R, C, I>
    where
        R: core::ops::RangeBounds<usize>,
        C: core::iter::FromIterator<Self::Parsed>,
    {
        basic::many_range_collect(self, range)
    }

    fn array<const LEN: usize>(self) -> basic::ArrayParser<Self, I, LEN> {
        basic::array(self)
    }

    fn all_consuming(self) -> basic::AllConsumingParser<Self, I> {
        basic::all_consuming(self)
    }

    fn spanned(self) -> basic::SpannedParser<Self, I> {
        basic::spanned(self)
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

    fn or<P>(self, parser: P) -> basic::EitherParser<Self, P, I>
    where
        P: Parse<I, Parsed = Self::Parsed, Error = Self::Error>,
    {
        basic::either(self, parser)
    }

    fn then<P>(self, parser: P) -> basic::PairParser<Self, P, I>
    where
        P: Parse<I, Error = Self::Error>,
    {
        basic::pair(self, parser)
    }

    fn prefixed_with<P>(self, prefix: P) -> basic::PrefixParser<P, Self, I>
    where
        P: Parse<I, Error = Self::Error>,
    {
        basic::prefix(prefix, self)
    }

    fn suffixed_with<P>(self, suffix: P) -> basic::SuffixParser<Self, P, I>
    where
        P: Parse<I, Error = Self::Error>,
    {
        basic::suffix(self, suffix)
    }

    fn delimited_with<P, S>(self, prefix: P, suffix: S) -> basic::DelimitedParser<P, Self, S, I>
    where
        P: Parse<I, Error = Self::Error>,
        S: Parse<I, Error = Self::Error>,
    {
        basic::delimited(prefix, self, suffix)
    }

    fn separates<P, Q>(self, first: P, second: Q) -> basic::SeparatedParser<P, Self, Q, I>
    where
        P: Parse<I, Error = Self::Error>,
        Q: Parse<I, Error = Self::Error>,
    {
        basic::separated(first, self, second)
    }

    fn separated_with<P, Q>(self, separator: P, other: Q) -> basic::SeparatedParser<Self, P, Q, I>
    where
        P: Parse<I, Error = Self::Error>,
        Q: Parse<I, Error = Self::Error>,
    {
        basic::separated(self, separator, other)
    }
}

mod sealed {
    use super::{Error, Input, PResult};

    pub trait Sealed {}

    impl<T, I: Input, E: Error<I>> Sealed for PResult<T, I, E> {}
}

/// Additional convenience methods for [`PResult`].
pub trait PResultExt: sealed::Sealed {
    type Parsed;
    type Error: Error<Self::Input>;
    type Input: Input;

    fn success(parsed: Self::Parsed, remaining: Self::Input) -> Self;

    fn failure(error: Self::Error, remaining: Self::Input) -> Self;

    fn remaining(&self) -> &Self::Input;

    fn remaining_mut(&mut self) -> &mut Self::Input;

    fn parsed(&self) -> Option<&Self::Parsed>;

    fn parsed_mut(&mut self) -> Option<&mut Self::Parsed>;

    fn extract(self) -> (Result<Self::Parsed, Self::Error>, Self::Input);

    fn map_parsed<F, R>(self, map_fn: F) -> PResult<R, Self::Input, Self::Error>
    where
        F: FnOnce(Self::Parsed) -> R;
}

impl<T, I: Input, E: Error<I>> PResultExt for PResult<T, I, E> {
    type Parsed = T;
    type Error = E;
    type Input = I;

    fn success(parsed: T, remaining: I) -> Self {
        Ok(Success(parsed, remaining))
    }

    fn failure(error: E, remaining: I) -> Self {
        Err(Failure(error, remaining))
    }

    fn remaining(&self) -> &I {
        match self {
            Ok(Success(_, rem)) => rem,
            Err(Failure(_, rem)) => rem,
        }
    }

    fn remaining_mut(&mut self) -> &mut I {
        match self {
            Ok(succ) => &mut succ.1,
            Err(fail) => &mut fail.1,
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

    fn extract(self) -> (Result<T, E>, I) {
        match self {
            Ok(Success(val, rem)) => (Ok(val), rem),
            Err(Failure(err, rem)) => (Err(err), rem),
        }
    }

    fn map_parsed<F, R>(self, map_fn: F) -> PResult<R, I, E>
    where
        F: FnOnce(Self::Parsed) -> R,
    {
        self.map(move |succ| succ.map(map_fn))
    }
}

impl<F, I, R, E> Parse<I> for F
where
    F: Fn(I) -> PResult<R, I, E>,
    I: Input,
    E: Error<I>,
{
    type Parsed = R;
    type Error = E;

    fn parse(&self, input: I) -> PResult<R, I, E> {
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

impl<T, U: From<T>, I: Input, E: Error<I>> From<Success<T, I>> for PResult<U, I, E> {
    fn from(Success(val, rem): Success<T, I>) -> Self {
        Ok(Success(U::from(val), rem))
    }
}

impl<T, I: Input, E: Error<I>, U: Error<I> + From<E>> From<Failure<E, I>> for PResult<T, I, U> {
    fn from(Failure(err, rem): Failure<E, I>) -> Self {
        Err(Failure(U::from(err), rem))
    }
}

/// Compiles a parser-combinator expression into a `const` parser.
///
/// This macro ensures that the provided `pcrs` parser-combinator expression
/// is evaluated a compile time into a statically defined object implementing
/// [`Parse`]. Even mildly complex parser-combinator expressions can incur
/// some small, but measurable, runtime overhead associated with building the
/// parser object on demand. This macro generally eliminates that overhead.
///
/// The returned parser is also wrapped in a closure for convenience, so that
/// the parser can optionally be called like a function instead of using
/// `.parse(...)`, which requires having [`Parse`] in scope.
///
/// In order for the expression to be compilable, `const fn` combinators must
/// be used. All free-function versions of combinators from `pcrs` must be
/// used instead of the provided methods of [`Parse`]. All free-function
/// combinators provided by `pcrs` are `const fn`. For example,
/// `pcrs::basic::map(my_parser, |val| val * 2)` must be used instead of
/// `my_parser.map(|val| val * 2)`
///
/// # Example
/// ```
/// use pcrs::{
///     basic::many0,
///     unicode::{PResult, char}
///     compile,
/// };
///
/// fn my_parser(input: &str) -> PResult<usize, &str> {
///     compile!(
///         many0(char, |iter| iter.count())
///     )(input)
/// }
/// ```
#[macro_export]
macro_rules! compile {
    ($parser:expr) => {{
        const { |input| $crate::Parse::parse(const { &($parser) }, input) }
    }};
}
