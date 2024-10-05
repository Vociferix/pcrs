#![no_std]

//! Parser-combinator library for Rust.
//!
//! `pcrs` is heavily inspired by another parser-combinator crate, `nom`. Users of the `nom` crate
//! will find `pcrs` very familiar.

mod input;
mod span;

pub mod ascii;
pub mod basic;
pub mod bytes;
pub mod unicode;

pub use input::*;
pub use span::*;

#[doc(inline)]
pub use basic::{
    alt, array, complete, cond, constant, delimited, either, eof, error, flat_map, from_ref, many0,
    many0_collect, many1, many1_collect, many_range, many_range_collect, many_until,
    many_until_collect, map, map_err, map_pres, map_res, not, opt, or_default, or_else, or_value,
    pair, peek, permutation, pop, prefix, recognize, repeated, repeated_collect, separated, seq,
    spanned, suffix, take, try_flat_map, try_many0, try_many1, try_many_range, try_many_until,
    try_map, try_repeated, verbatim, verify, with, with_default, with_value,
};

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
pub trait Parse<I: Input>: Sized {
    /// The value type that is produced by the parser on success.
    type Parsed: Sized;

    /// The error type that is produced by the parser on failure.
    type Error: Error<I>;

    /// Parses the provided input.
    fn parse(&self, input: I) -> PResult<Self::Parsed, I, Self::Error>;
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
