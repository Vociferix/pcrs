use crate::{Parse, Success};

/// A parsable symbol stream.
///
/// [`Input`] is very similar to [`Iterator`]. The key difference is that a type
/// implementing [`Input`] must support multiple and partial passes in any
/// combination. `pcrs` implements recursive descent parsing, which means that
/// there is no limit to the number of symbols the parser may consume before
/// determining that it needs to backtrack (although individual parsers may have
/// a finite bound).
///
/// As such, [`Input`] is essentially an [`Iterator`] that can be cloned and can
/// be compared with clones for positional equality.
pub trait Input: Clone {
    /// The symbol type this input provides a stream of.
    type Symbol;

    /// Gets the next symbol in the stream.
    ///
    /// [`None`] indicates that the end of the input has been reached.
    fn next(&mut self) -> Option<Self::Symbol>;

    /// Compares the position of two inputs for positional equality.
    ///
    /// If both inputs are at the same position of the stream, `true` is
    /// returned, `false` otherwise.
    fn pos_eq(&self, other: &Self) -> bool;

    /// Advances the stream by a given number of symbols.
    ///
    /// The number of skipped symbols is returned, which should always be equal
    /// to `count`, unless the remaining input was less than `count` symbols.
    /// If the returned value is not equal to `count`, the input must now be
    /// at the end of input.
    ///
    /// Implementors may wish to customize this method to provide a more
    /// performant implementation than calling [`next`](Input::next) `count`
    /// times.
    fn advance_by(&mut self, count: usize) -> usize {
        for idx in 0..count {
            let Some(_) = self.next() else {
                return idx;
            };
        }
        count
    }

    /// Returns the remaining number of symbols, if known.
    ///
    /// Not all input streams can easily know how many symbols they contain,
    /// but when it is known, some parsers can use that information for
    /// optimization.
    ///
    /// It is recommended that implementors customize this method if the
    /// remaining size of the stream can be computed in constant time.
    fn size_hint(&self) -> Option<usize> {
        None
    }

    /// Returns `true` if the stream is at the end of input.
    ///
    /// Implementors may wish to customize this method to provide a more
    /// performant implementation. The default implementation clones the
    /// input and checks if calling [`next`](Input::next) yeilds a value.
    fn is_empty(&self) -> bool {
        self.clone().next().is_none()
    }

    /// Creates a new input stream with mapped values.
    ///
    /// The provided function or closure will be used by the returned
    /// stream to transform symbols of the original stream.
    fn map<F, O>(self, map_fn: F) -> impl Input<Symbol = O>
    where
        F: Clone + Fn(Self::Symbol) -> O,
    {
        struct Map<I, F, O>(I, F, core::marker::PhantomData<O>);

        impl<I: Clone, F: Clone, O> Clone for Map<I, F, O> {
            fn clone(&self) -> Self {
                Self(self.0.clone(), self.1.clone(), core::marker::PhantomData)
            }
        }

        impl<I, F, O> Input for Map<I, F, O>
        where
            I: Input,
            F: Clone + Fn(I::Symbol) -> O,
        {
            type Symbol = O;

            fn next(&mut self) -> Option<Self::Symbol> {
                self.0.next().map(&self.1)
            }

            fn pos_eq(&self, other: &Self) -> bool {
                self.0.pos_eq(&other.0)
            }

            fn advance_by(&mut self, count: usize) -> usize {
                self.0.advance_by(count)
            }

            fn size_hint(&self) -> Option<usize> {
                self.0.size_hint()
            }
        }

        Map(self, map_fn, core::marker::PhantomData)
    }
}

/// Input derived from other input by repeatedly applying a parser.
///
/// [`ParsedInput`] consists of an underlying input and a parser.
/// Each symbol produced by a [`ParsedInput`] is produced by applying
/// the parser to the inner input. After parsing a symbol, the inner
/// input becomes the remaining unparsed inner input. The [`ParsedInput`]
/// ends when parsing a symbol fails.
#[derive(Debug, Clone)]
pub struct ParsedInput<P, I>
where
    P: Parse<I> + Clone,
    I: Input,
{
    parser: P,
    input: Option<I>,
}

impl<T: Copy> Input for &[T] {
    type Symbol = T;

    fn next(&mut self) -> Option<Self::Symbol> {
        if let Some((head, tail)) = self.split_at_checked(1) {
            *self = tail;
            head.first().copied()
        } else {
            None
        }
    }

    fn pos_eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }

    fn advance_by(&mut self, count: usize) -> usize {
        let count = core::cmp::min(self.len(), count);
        *self = &self[count..];
        count
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len())
    }

    fn is_empty(&self) -> bool {
        <[T]>::is_empty(*self)
    }
}

impl Input for &str {
    type Symbol = char;

    fn next(&mut self) -> Option<Self::Symbol> {
        let mut chars = self.chars();
        let ret = chars.next();
        *self = chars.as_str();
        ret
    }

    fn pos_eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }

    fn is_empty(&self) -> bool {
        str::is_empty(*self)
    }
}

impl<P, I> ParsedInput<P, I>
where
    P: Parse<I> + Clone,
    I: Input,
{
    /// Creates a new [`ParsedInput`].
    ///
    /// The returned [`ParsedInput`] will produce symbols by repeated
    /// applying the provided parser over the provided input.
    pub const fn new(parser: P, input: I) -> Self {
        Self {
            parser,
            input: Some(input),
        }
    }
}

impl<P, I> Input for ParsedInput<P, I>
where
    P: Parse<I> + Clone,
    I: Input,
{
    type Symbol = P::Parsed;

    fn next(&mut self) -> Option<Self::Symbol> {
        let Some(input) = self.input.take() else {
            return None;
        };
        match self.parser.parse(input) {
            Ok(Success(symb, rem)) => {
                self.input = Some(rem);
                Some(symb)
            }
            _ => None,
        }
    }

    fn pos_eq(&self, other: &Self) -> bool {
        if let Some(input) = self.input.as_ref() {
            if let Some(other) = other.input.as_ref() {
                input.pos_eq(other)
            } else {
                false
            }
        } else {
            other.input.is_none()
        }
    }

    fn is_empty(&self) -> bool {
        self.input.as_ref().map(I::is_empty).unwrap_or(true)
    }
}
