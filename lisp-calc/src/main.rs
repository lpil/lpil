use std::io;
use std::io::prelude::*;

extern crate lib;
use lib::parser;

static HELP_TEXT: &'static str = "Valid operators are + - / *
q to quit. h for help.";


fn main() {
    println!("Hello! Enter a lisp expression");
    print_help();
    repl();
    println!("\nBye!");
}

fn print_help() {
    println!("{}\n", HELP_TEXT);
}

fn repl() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = line.unwrap();
        match input.as_ref() {
            "q" => break,
            "?" => print_help(),
            "h" => print_help(),
            "" => println!("eh?\n"),
            _ => println!("{}\n", eval(input)),
        }
    }
}

fn eval(input: String) -> String {
    match parser::parse(&input) {
        Ok(sexpr) => format!("{}", sexpr.value()),
        Err(error) => format!("Error: {}", error),
    }
}
