// TODO
#![allow(dead_code)]


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
    pub fn reduce(&self) -> Reduced {
        match self {
            &Node::String { value: _ } => Reduced::None,
            &Node::Symbol { value: _ } => Reduced::None,
            &Node::Number { value: _ } => Reduced::None,
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
}
