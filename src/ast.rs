// TODO
#![allow(dead_code)]

use std::fmt::Write;


#[derive(Debug, PartialEq)]
pub enum Reduced {
    Some(Node),
    None,
}


#[derive(Debug, PartialEq)]
pub enum Node {
    String { value: String },
    Symbol { value: String },
    Number { value: f64 },
}

pub fn number(v: f64) -> Node {
    Node::Number { value: v }
}

pub fn string(v: String) -> Node {
    Node::String { value: v }
}

pub fn symbol(v: String) -> Node {
    Node::Symbol { value: v }
}

impl Node {
    /// Attempt to reduce the Node by one step.
    /// Nodes may reduce, not reduce, or error depending their type and state.
    ///
    pub fn reduce(&self) -> Reduced {
        match self {
            &Node::String { value: _ } => Reduced::None,
            &Node::Symbol { value: _ } => Reduced::None,
            &Node::Number { value: _ } => Reduced::None,
        }
    }

    /// Convert Node in the Xcss source syntax and write into the given buffer.
    ///
    pub fn write_source(&self, b: &mut String) {
        match self {
            &Node::String { value: ref v } => write!(b, "{:?}", v).unwrap(),
            &Node::Symbol { value: ref v } => write!(b, "{}", v).unwrap(),
            &Node::Number { value: ref v } => write!(b, "{}", v).unwrap(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(number.reduce(), Reduced::None);
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
    fn symbols_are_not_reducible() {
        let symbol = symbol("main".to_string());
        assert_eq!(symbol.reduce(), Reduced::None);
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
        assert_eq!(string.reduce(), Reduced::None);
    }

    #[test]
    fn string_write_source() {
        let string = string("Hello sailor.".to_string());
        let mut buffer = String::new();
        string.write_source(&mut buffer);
        assert_eq!(buffer, "\"Hello sailor.\"".to_string());
    }
}
