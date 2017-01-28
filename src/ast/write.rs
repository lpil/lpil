use std::fmt::Write;
use super::Node;

pub fn call(b: &mut String, name: &String, args: &Vec<Node>) {
    let len = args.len();
    match len {
        0 => write!(b, "{}()", name).unwrap(),
        _ => {
            write!(b, "{}(", name).unwrap();
            for i in 0..(args.len() - 1) {
                args[i].write_source(b);
                write!(b, ", ").unwrap();
            }
            args.last().unwrap().write_source(b);
            write!(b, ")").unwrap();
        }
    }
}
