// TODO
#![allow(dead_code)]

use std::fmt::Write;
use super::env::Env;

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
}


/// The result of attempting to reduce an AST Node
///
#[derive(Debug, PartialEq)]
pub enum Reduced {
    Some(Node),
    Err(Error),
    None,
}


/// An AST Node
///
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    String { value: String },
    Symbol { value: String },
    Number { value: f64 },
}

impl Node {
    /// Attempt to reduce the Node by one step.
    /// Nodes may reduce, not reduce, or error depending their type and state.
    ///
    pub fn reduce(&self, env: &mut Env) -> Reduced {
        match self {
            &Node::String { .. } => Reduced::None,
            &Node::Number { .. } => Reduced::None,
            &Node::Symbol { value: ref v } => {
                match env.get_var(v) {
                    Some(value) => Reduced::Some(value.clone()),
                    None => Reduced::Err(Error::UndefinedVariable(v.to_string())),
                }
            }
        }
    }

    /// Convert Node in the Xcss source syntax and write into the given buffer.
    ///
    pub fn write_source(&self, b: &mut String) {
        match self {
            &Node::String { value: ref v, .. } => write!(b, "{:?}", v).unwrap(),
            &Node::Number { value: ref v, .. } => write!(b, "{}", v).unwrap(),
            &Node::Symbol { value: ref v, .. } => write!(b, "{}", v).unwrap(),
        }
    }
}


/// Construct a Number Node
///
pub fn number(v: f64) -> Node {
    Node::Number { value: v }
}

/// Construct a String Node
///
pub fn string(v: String) -> Node {
    Node::String { value: v }
}

/// Construct a Symbol Node
///
pub fn symbol(v: String) -> Node {
    Node::Symbol { value: v }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::super::env::Env;

    //
    // Numbers
    //

    #[test]
    fn new_number_test() {
        let number = number(100.0);
        assert_eq!(number, Node::Number { value: 100.0 });
    }

    #[test]
    fn numbers_are_not_reducible() {
        let number = number(100.0);
        let mut env = Env::new();
        assert_eq!(number.reduce(&mut env), Reduced::None);
    }

    #[test]
    fn number_write_source() {
        let number = number(100.0);
        let mut buffer = String::new();
        number.write_source(&mut buffer);
        assert_eq!(buffer, "100".to_string());
    }


    //
    // Symbols
    //

    #[test]
    fn new_symbol_test() {
        let symbol = symbol("main".to_string());
        assert_eq!(symbol, Node::Symbol { value: "main".to_string() });
    }

    #[test]
    fn symbols_reduce_to_variable_from_env() {
        let symbol = symbol("main".to_string());
        let mut env = Env::new();
        env.set_var("main".to_string(), number(1.0));
        assert_eq!(symbol.reduce(&mut env), Reduced::Some(number(1.0)));
    }

    #[test]
    fn symbols_reduce_to_error_with_unset_var() {
        let symbol = symbol("main".to_string());
        let mut env = Env::new();
        let err = Error::UndefinedVariable("main".to_string());
        assert_eq!(symbol.reduce(&mut env), Reduced::Err(err));
    }

    #[test]
    fn symbol_write_source() {
        let symbol = symbol("main".to_string());
        let mut buffer = String::new();
        symbol.write_source(&mut buffer);
        assert_eq!(buffer, "main".to_string());
    }


    //
    // Strings
    //

    #[test]
    fn new_string_test() {
        let string = string("Hello sailor.".to_string());
        assert_eq!(string, Node::String { value: "Hello sailor.".to_string() });
    }

    #[test]
    fn strings_are_not_reducible() {
        let string = string("Hello sailor.".to_string());
        let mut env = Env::new();
        assert_eq!(string.reduce(&mut env), Reduced::None);
    }

    #[test]
    fn string_write_source() {
        let string = string("Hello sailor.".to_string());
        let mut buffer = String::new();
        string.write_source(&mut buffer);
        assert_eq!(buffer, "\"Hello sailor.\"".to_string());
    }
}
