#![feature(globs)]
use ttt_game::*;

mod ttt_game;

#[cfg(not(test))]
fn main() {
    let game = new_game();
    println!("{}\n", game);

    println!("Valid moves: {}\n", game.valid_moves());

    let new_game = game.make_move(0, 'X');
    println!("{}\n", new_game);

    println!("Valid moves: {}\n", new_game.valid_moves());

    match new_game.won() {
        Some(x) => println!("{} wins", x),
        _ => println!("No one won")
    }
}
