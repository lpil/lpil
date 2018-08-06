pub mod parser;

use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Div,
    Mult,
}

impl Op {
    pub fn apply(&self, nums: &Vec<Sexpr>) -> f64 {
        match self {
            &Op::Plus => nums.iter().fold(0.0, |a, e| a + e.value()),
            &Op::Mult => nums.iter().fold(1.0, |a, e| a * e.value()),
            &Op::Div => {
                match nums.len() {
                    0 => 1.0,
                    1 => 1.0 / nums[0].value(),
                    _ => {
                        let (hd, tl) = nums.split_at(1);
                        tl.iter().fold(hd[0].value(), |a, e| a / e.value())
                    }
                }
            }
            &Op::Minus => {
                match nums.len() {
                    0 => 0.0,
                    1 => 0.0 - nums[0].value(),
                    _ => {
                        let (hd, tl) = nums.split_at(1);
                        tl.iter().fold(hd[0].value(), |a, e| a - e.value())
                    }
                }
            }
        }
    }
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


#[derive(Debug, PartialEq)]
pub enum Sexpr {
    List(Op, Vec<Sexpr>),
    Value(f64),
}

impl Sexpr {
    pub fn value(&self) -> f64 {
        match self {
            &Sexpr::Value(n) => n,
            &Sexpr::List(ref op, ref nums) => op.apply(nums),
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
        }
    }
}
