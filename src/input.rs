use crate::{Parse, Success};

pub trait Input: Clone {
    type Symbol;

    fn next(&mut self) -> Option<Self::Symbol>;

    fn pos_eq(&self, other: &Self) -> bool;

    fn advance_by(&mut self, count: usize) -> usize {
        for idx in 0..count {
            let Some(_) = self.next() else {
                return idx;
            };
        }
        count
    }

    fn size_hint(&self) -> Option<usize> {
        None
    }

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
}

impl<P, I> ParsedInput<P, I>
where
    P: Parse<I> + Clone,
    I: Input,
{
    pub fn new(parser: P, input: I) -> Self {
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
}
