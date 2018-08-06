extern crate lib;

use lib::*;

#[test]
fn display_sexpr_node_value() {
    let sexpr = Sexpr::Value(123.0);
    assert_eq!(123.0, sexpr.value());
}

#[test]
fn display_sexpr_empty_plus_list_value() {
    let sexpr = Sexpr::List(Op::Plus, vec![]);
    assert_eq!(0.0, sexpr.value());
}

#[test]
fn display_sexpr_plus_list_value() {
    let sexpr = Sexpr::List(Op::Plus,
                            vec![Sexpr::Value(1.1), Sexpr::Value(5.5), Sexpr::Value(2.2)]);
    assert_eq!(8.8, sexpr.value());
}

#[test]
fn display_sexpr_empty_minus_list_value() {
    let sexpr = Sexpr::List(Op::Minus, vec![]);
    assert_eq!(0.0, sexpr.value());
}

#[test]
fn display_sexpr_minus_1_elem_list_value() {
    let sexpr = Sexpr::List(Op::Minus, vec![Sexpr::Value(1.0)]);
    assert_eq!(-1.0, sexpr.value());
}

#[test]
fn display_sexpr_minus_list_value() {
    let sexpr = Sexpr::List(Op::Minus, vec![Sexpr::Value(1.1), Sexpr::Value(5.5)]);
    assert_eq!(-4.4, sexpr.value());
}

#[test]
fn display_sexpr_empty_mult_list_value() {
    let sexpr = Sexpr::List(Op::Mult, vec![]);
    assert_eq!(1.0, sexpr.value());
}

#[test]
fn display_sexpr_mult_1_elem_list_value() {
    let sexpr = Sexpr::List(Op::Mult, vec![Sexpr::Value(3.0)]);
    assert_eq!(3.0, sexpr.value());
}

#[test]
fn display_sexpr_mult_list_value() {
    let sexpr = Sexpr::List(Op::Mult, vec![Sexpr::Value(2.0), Sexpr::Value(5.5)]);
    assert_eq!(11.0, sexpr.value());
}

#[test]
fn display_sexpr_empty_div_list_value() {
    let sexpr = Sexpr::List(Op::Div, vec![]);
    assert_eq!(1.0, sexpr.value());
}

#[test]
fn display_sexpr_div_1_elem_list_value() {
    let sexpr = Sexpr::List(Op::Div, vec![Sexpr::Value(2.0)]);
    assert_eq!(0.5, sexpr.value());
}

#[test]
fn display_sexpr_div_list_value() {
    let sexpr = Sexpr::List(Op::Div, vec![Sexpr::Value(10.0), Sexpr::Value(2.0)]);
    assert_eq!(5.0, sexpr.value());
}
