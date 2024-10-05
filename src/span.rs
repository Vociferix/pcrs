use crate::Input;

/// A slice of parser input.
///
/// A [`Span`] represents a subset of input. Internally, a [`Span`]
/// consists of two [`Input`] objects: one representing the start
/// of the [`Span`] and the other other representing the end.
#[derive(Debug, Clone)]
pub struct Span<I: Input> {
    begin: I,
    end: I,
}

impl<I: Input + Copy> Copy for Span<I> {}

impl<I: Input> Span<I> {
    /// Creates a new [`Span`] over the input between `begin` and `end`.
    pub fn new(begin: I, end: I) -> Self {
        Self { begin, end }
    }

    /// Gets the start position of the [`Span`].
    pub fn start(&self) -> &I {
        &self.begin
    }

    /// Gets the end position of the [`Span`].
    pub fn end(&self) -> &I {
        &self.end
    }
}

impl<I: Input> Input for Span<I> {
    type Symbol = I::Symbol;

    fn next(&mut self) -> Option<Self::Symbol> {
        if self.begin.pos_eq(&self.end) {
            None
        } else {
            self.begin.next()
        }
    }

    fn pos_eq(&self, other: &Self) -> bool {
        self.begin.pos_eq(&other.begin)
    }
}

impl<I: Input> Iterator for Span<I> {
    type Item = I::Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        Input::next(self)
    }
}
