// TODO
#![allow(dead_code)]

use super::ast;

/// A function defined in Xcss
///
#[derive(Debug)]
pub struct Function {
    name: String,
    arguments: Vec<String>,
    body: ast::Node,
}

impl Function {
    pub fn new(name: String, arguments: Vec<String>, body: ast::Node) -> Function {
        Function {
            name: name,
            arguments: arguments,
            body: body,
        }
    }
}
