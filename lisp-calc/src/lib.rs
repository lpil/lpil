mod parser;

use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Div,
    Mult,
}

#[derive(Debug, PartialEq)]
pub enum Sexpr {
    List(Op, Vec<Sexpr>),
    Value(f64),
    Nil,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Op::Plus => write!(f, "+"),
            &Op::Minus => write!(f, "-"),
            &Op::Div => write!(f, "/"),
            &Op::Mult => write!(f, "*"),
        }
    }
}

impl fmt::Display for Sexpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Sexpr::List(ref op, ref elems) => {
                let contents = elems.iter()
                    .map(|e| format!(" {}", *e))
                    .collect::<Vec<_>>()
                    .join("");
                write!(f, "({}{})", op, contents)
            }
            &Sexpr::Value(n) => write!(f, "{}", n),
            &Sexpr::Nil => write!(f, "()"),

        }
    }
}
