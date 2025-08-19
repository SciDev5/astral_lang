use std::{
    fmt::Debug,
    ops::{Index, Range},
};

use nom::{Compare, FindSubstring, Input};

use super::loc::FileLoc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocatedSpan<T, S> {
    loc: FileLoc,
    src: T,
    state: S,
}
impl<'a, T: ?Sized, S> LocatedSpan<&'a T, S> {
    pub const fn with_state<Z>(&self, state: Z) -> LocatedSpan<&'a T, Z> {
        LocatedSpan {
            loc: self.loc,
            src: self.src,
            state,
        }
    }
    pub fn map_state<Z>(self, map: impl Fn(S) -> Z) -> LocatedSpan<&'a T, Z> {
        LocatedSpan {
            loc: self.loc,
            src: self.src,
            state: map(self.state),
        }
    }
    pub const fn as_span(&self) -> LocatedSpan<&'a T, ()> {
        LocatedSpan {
            loc: self.loc,
            src: self.src,
            state: (),
        }
    }
    pub const fn inner_state(&self) -> &S {
        &self.state
    }
    pub const fn loc(&self) -> FileLoc {
        self.loc
    }
    pub const fn as_start(&self) -> LocatedSpan<&'a T, ()> {
        LocatedSpan {
            loc: self.loc.with_len(0),
            src: self.src,
            state: (),
        }
    }
}
impl<'a, S> LocatedSpan<&'a str, S> {
    pub const fn from_inner(src: &'a str, state: S) -> Self {
        Self {
            loc: FileLoc::at(0).with_len(src.len()),
            src,
            state,
        }
    }
}

impl<'a, S> LocatedSpan<&'a [u8], S> {
    pub const fn from_inner(src: &'a [u8], state: S) -> Self {
        Self {
            loc: FileLoc::at(0).with_len(src.len()),
            src,
            state,
        }
    }
}
impl<T: Eq + Debug, S> LocatedSpan<T, S> {
    pub fn merge(self, other: Self) -> LocatedSpan<T, ()> {
        debug_assert_eq!(&self.src, &other.src);
        LocatedSpan {
            loc: self.loc.merge(other.loc),
            src: self.src,
            state: (),
        }
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, S> LocatedSpan<&'a T, S>
where
    &'a T: Input,
{
    pub fn as_inner(self) -> &'a T {
        &self.src[self.loc.start_byte..self.loc.end_byte]
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, R, S: Copy> Compare<R>
    for LocatedSpan<&'a T, S>
where
    &'a T: Input + Compare<R>,
{
    fn compare(&self, t: R) -> nom::CompareResult {
        self.as_inner().compare(t)
    }

    fn compare_no_case(&self, t: R) -> nom::CompareResult {
        self.as_inner().compare_no_case(t)
    }
}
impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, R, S: Copy> FindSubstring<R>
    for LocatedSpan<&'a T, S>
where
    &'a T: Input + FindSubstring<R>,
{
    fn find_substring(&self, substr: R) -> Option<usize> {
        self.as_inner().find_substring(substr)
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, S: Copy> Input for LocatedSpan<&'a T, S>
where
    &'a T: Input,
{
    type Item = <&'a T as Input>::Item;
    type Iter = <&'a T as Input>::Iter;
    type IterIndices = <&'a T as Input>::IterIndices;

    fn input_len(&self) -> usize {
        self.loc.len()
    }

    fn take(&self, index: usize) -> Self {
        Self {
            loc: self.loc.with_len(index),
            src: self.src,
            state: self.state,
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            loc: self.loc.with_shift_start(index),
            src: self.src,
            state: self.state,
        }
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        (self.take(index), self.take_from(index))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_inner().position(predicate)
    }

    fn iter_elements(&self) -> Self::Iter {
        self.as_inner().iter_elements()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.as_inner().iter_indices()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_inner().slice_index(count)
    }
}
