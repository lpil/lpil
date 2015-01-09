#![feature(globs)]

extern crate getopts;

#[cfg(not(test))]
use std::os;

#[cfg(not(test))]
use games::*;

mod games;
mod game_mechanics;

#[cfg(not(test))]
fn main() {
    let flag = os::args().pop().expect("");

    match flag.as_slice() {
        "launchpad" => launchpad_game(),
        _ => terminal_game()
    }
}
