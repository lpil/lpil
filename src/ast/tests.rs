#![cfg(test)]

use super::*;
use super::super::error::Error;
use super::super::env::Env;
use super::reduce::Reduced;

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
    let node = symbol("main".to_string());
    let mut env = Env::new();
    env.set_var("main".to_string(), number(1.0));
    assert_eq!(node.reduce(&mut env), Reduced::Some(number(1.0)));
}

#[test]
fn symbols_reduce_to_error_with_unset_var() {
    let node = symbol("main".to_string());
    let mut env = Env::new();
    let err = Error::UndefinedVariable("main".to_string());
    assert_eq!(node.reduce(&mut env), Reduced::Err(err));
}

#[test]
fn symbol_write_source() {
    let node = symbol("main".to_string());
    let mut buffer = String::new();
    node.write_source(&mut buffer);
    assert_eq!(buffer, "main".to_string());
}


//
// Strings
//

#[test]
fn new_string_test() {
    let node = string("Hello sailor.".to_string());
    assert_eq!(node, Node::String { value: "Hello sailor.".to_string() });
}

#[test]
fn strings_are_not_reducible() {
    let node = string("Hello sailor.".to_string());
    let mut env = Env::new();
    assert_eq!(node.reduce(&mut env), Reduced::None);
}

#[test]
fn string_write_source() {
    let node = string("Hello sailor.".to_string());
    let mut buffer = String::new();
    node.write_source(&mut buffer);
    assert_eq!(buffer, "\"Hello sailor.\"".to_string());
}

//
// Function calls
//

// #[test]
// fn new_string_test() {
//     let string = string("Hello sailor.".to_string());
//     assert_eq!(string, Node::String { value: "Hello sailor.".to_string() });
// }

// #[test]
// fn strings_are_not_reducible() {
//     let string = string("Hello sailor.".to_string());
//     let mut env = Env::new();
//     assert_eq!(string.reduce(&mut env), Reduced::None);
// }

#[test]
fn call_write_source_0_args() {
    let node = call("add".to_string(), vec![]);
    let mut buffer = String::new();
    node.write_source(&mut buffer);
    assert_eq!(buffer, "add()".to_string());
}

#[test]
fn call_write_source_1_args() {
    let node = call("add".to_string(), vec![number(1.0)]);
    let mut buffer = String::new();
    node.write_source(&mut buffer);
    assert_eq!(buffer, "add(1)".to_string());
}

#[test]
fn call_write_source_2_args() {
    let node = call("add".to_string(), vec![number(1.0), number(2.0)]);
    let mut buffer = String::new();
    node.write_source(&mut buffer);
    assert_eq!(buffer, "add(1, 2)".to_string());
}
