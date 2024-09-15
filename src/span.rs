use crate::Input;

#[derive(Debug, Clone)]
pub struct Span<I: Input> {
    begin: I,
    end: I,
}

impl<I: Input + Copy> Copy for Span<I> {}

impl<I: Input> Span<I> {
    pub fn new(begin: I, end: I) -> Self {
        Self { begin, end }
    }

    pub fn iter(&self) -> Span<I> {
        self.clone()
    }

    pub fn start(&self) -> &I {
        &self.begin
    }

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
