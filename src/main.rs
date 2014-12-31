#![feature(globs)]
use ttt_game::*;

mod ttt_game;

#[cfg(not(test))]
fn main() {
    let game = Game {
        board: [
            None,None,None,
            None,None,None,
            None,None,None
        ]
    };

    println!("{}", game.board);

    match game.won() {
        Some(x) => println!("{} wins", x),
        _ => println!("No one won")
    }
}

