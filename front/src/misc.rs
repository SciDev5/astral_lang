#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArbitraryInt {
    negate: bool,
    chunks: Vec<u64>,
}
