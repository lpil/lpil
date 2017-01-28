// TODO
#![allow(dead_code)]

mod tests;

mod write;
mod reduce;
mod new;

pub use self::new::*;

use std::fmt::Write;
use super::env::Env;
use self::reduce::Reduced;


/// A node in the AST
///
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    String { value: String },
    Symbol { value: String },
    Number { value: f64 },
    Call { name: String, args: Vec<Node> },
}


impl Node {
    /// Attempt to reduce the Node by one step.
    /// Nodes may reduce, not reduce, or error depending their type and state.
    ///
    pub fn reduce(&self, env: &mut Env) -> Reduced {
        match self {
            &Node::String { .. } => Reduced::None,
            &Node::Number { .. } => Reduced::None,
            &Node::Symbol { ref value } => reduce::symbol(value, env),
            &Node::Call { .. } => Reduced::None, // TODO
        }
    }

    /// Convert Node in the Xcss source syntax and write into the given buffer.
    ///
    pub fn write_source(&self, b: &mut String) {
        match self {
            &Node::String { ref value, .. } => write!(b, "{:?}", value).unwrap(),
            &Node::Number { ref value, .. } => write!(b, "{}", value).unwrap(),
            &Node::Symbol { ref value, .. } => write!(b, "{}", value).unwrap(),
            &Node::Call { ref name, ref args, .. } => write::call(b, name, args),
        }
    }
}
