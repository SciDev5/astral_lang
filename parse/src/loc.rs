#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileLoc {
    pub start_byte: usize,
    pub end_byte: usize,
}
impl FileLoc {
    pub const fn len(self) -> usize {
        self.end_byte - self.start_byte
    }
    pub const fn with_shift_start(self, off: usize) -> Self {
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
    pub const fn with_len(self, len: usize) -> Self {
        Self {
            start_byte: self.start_byte,
            end_byte: self.start_byte + len,
        }
    }
    pub const fn at(index: usize) -> Self {
        Self {
            start_byte: index,
            end_byte: index,
        }
    }
    pub const fn merge(self, other: Self) -> Self {
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
