use std::{
    fmt::Debug,
    ops::{Index, Range},
};

use nom::{Compare, FindSubstring, Input};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FileLoc {
    start_byte: usize,
    end_byte: usize,
}
impl FileLoc {
    const fn len(self) -> usize {
        self.end_byte - self.start_byte
    }
    const fn with_shift_start(self, off: usize) -> Self {
        let start = self.start_byte + off;
        Self {
            start_byte: if start < self.end_byte {
                start
            } else {
                self.end_byte
            },
            end_byte: self.end_byte,
        }
    }
    const fn with_len(self, len: usize) -> Self {
        Self {
            start_byte: self.start_byte,
            end_byte: self.start_byte + len,
        }
    }
    const fn at(index: usize) -> Self {
        Self {
            start_byte: index,
            end_byte: index,
        }
    }
    const fn merge(self, other: Self) -> Self {
        Self {
            start_byte: if self.start_byte < other.start_byte {
                self.start_byte
            } else {
                other.start_byte
            },
            end_byte: if self.start_byte > other.start_byte {
                self.start_byte
            } else {
                other.start_byte
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocatedSpan<T> {
    loc: FileLoc,
    src: T,
}
impl<'a> LocatedSpan<&'a str> {
    pub const fn from_inner(src: &'a str) -> Self {
        Self {
            loc: FileLoc::at(0).with_len(src.len()),
            src,
        }
    }
}
impl<'a> LocatedSpan<&'a [u8]> {
    pub const fn from_inner(src: &'a [u8]) -> Self {
        Self {
            loc: FileLoc::at(0).with_len(src.len()),
            src,
        }
    }
}
impl<T: Eq + Debug> LocatedSpan<T> {
    pub fn merge(self, other: Self) -> Self {
        debug_assert_eq!(&self.src, &other.src);
        Self {
            loc: self.loc.merge(other.loc),
            src: self.src,
        }
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized> LocatedSpan<&'a T>
where
    &'a T: Input,
{
    pub fn as_inner(self) -> &'a T {
        &self.src[self.loc.start_byte..self.loc.end_byte]
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, R> Compare<R> for LocatedSpan<&'a T>
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
impl<'a, T: Index<Range<usize>, Output = T> + ?Sized, R> FindSubstring<R> for LocatedSpan<&'a T>
where
    &'a T: Input + FindSubstring<R>,
{
    fn find_substring(&self, substr: R) -> Option<usize> {
        self.as_inner().find_substring(substr)
    }
}

impl<'a, T: Index<Range<usize>, Output = T> + ?Sized> Input for LocatedSpan<&'a T>
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
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            loc: self.loc.with_shift_start(index),
            src: self.src,
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
