#![feature(globs)]

extern crate getopts;

mod games;
mod game_mechanics;

#[cfg(not(test))]
fn main() {
    let flag = std::os::args().pop().expect("");

    match flag.as_slice() {
        "launchpad" => games::launchpad_game(),
        _ => games::terminal_game()
    }
}
