//! Receives ASTBlocks for all files and a hierachical structure for indicating how they can be accessed from one another.
//!
//! Outputs a collection of datas, metatypes, and functions and a list of global symbols with partial types.

pub mod extract;
pub mod resolve;
