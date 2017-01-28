use super::Node;

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

/// Construct a Symbol Node
///
pub fn call(name: String, args: Vec<Node>) -> Node {
    Node::Call {
        name: name,
        args: args,
    }
}
