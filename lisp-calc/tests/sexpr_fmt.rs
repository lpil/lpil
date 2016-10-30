extern crate lib;

use lib::*;

#[test]
fn display_sexpr_node() {
    let sexpr = Sexpr::Value(123.0);
    assert_eq!("123", format!("{}", sexpr));
}

#[test]
fn display_sexpr_nil() {
    let sexpr = Sexpr::Nil;
    assert_eq!("()", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_plus() {
    let sexpr = Sexpr::List(Op::Plus, vec![Sexpr::Value(20.02)]);
    assert_eq!("(+ 20.02)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_minus() {
    let sexpr = Sexpr::List(Op::Minus, vec![Sexpr::Value(34.4)]);
    assert_eq!("(- 34.4)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_div() {
    let sexpr = Sexpr::List(Op::Div, vec![Sexpr::Value(45.0)]);
    assert_eq!("(/ 45)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_mult() {
    let sexpr = Sexpr::List(Op::Mult, vec![Sexpr::Value(4.0)]);
    assert_eq!("(* 4)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_no_tail() {
    let sexpr = Sexpr::List(Op::Mult, vec![]);
    assert_eq!("(*)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_long_tail() {
    let sexpr = Sexpr::List(Op::Mult,
                            vec![Sexpr::Value(1.0),
                                 Sexpr::Value(2.0),
                                 Sexpr::Value(3.0),
                                 Sexpr::Value(4.0),
                                 Sexpr::Value(5.0)]);
    assert_eq!("(* 1 2 3 4 5)", format!("{}", sexpr));
}

#[test]
fn display_sexpr_branch_nested() {
    let grandchild = Sexpr::List(Op::Minus, vec![Sexpr::Value(10.0)]);
    let child1 = Sexpr::List(Op::Plus,
                             vec![Sexpr::Value(1.0), Sexpr::Value(5.0), grandchild]);
    let child2 = Sexpr::List(Op::Minus, vec![Sexpr::Value(100.0), Sexpr::Value(0.0)]);
    let sexpr = Sexpr::List(Op::Mult,
                            vec![Sexpr::Value(1.0),
                                 child1,
                                 Sexpr::Value(3.0),
                                 Sexpr::Value(4.0),
                                 Sexpr::Value(5.0),
                                 child2]);
    assert_eq!("(* 1 (+ 1 5 (- 10)) 3 4 5 (- 100 0))", format!("{}", sexpr));
}
