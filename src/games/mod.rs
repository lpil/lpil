#[cfg(not(test))]
use std::old_io;
#[cfg(not(test))]
use game_mechanics::*;

#[cfg(not(test))]
pub fn terminal_game() {
    let mut game = new_game();
    let mut player: Player = 'x';
    println!("Tic Tac Toe!");
    println!("{}\n", game);

    loop {
        print!("Make a move player {}!\n", player);
        print_vec_usize(game.valid_moves());
        let input: usize;
        match
            old_io::stdin().read_line()
            .ok() // Convert to Option type from ioResult type
            .expect("Failed to read line") // Handle None
            .trim() // Remove trailing newline
            .parse() {
                Some(x) if x < 9 => {
                    input = x;
                    game = game.make_move(input, player);
                    println!("{} picked {}\n{}\n", player, input, game);
                }
                _ => {
                    println!("That's not a valid guess");
                    continue;
                }
            }


        match game.won() {
            Some(x) => {
                println!("\n{} wins!\n{}\n", x, game);
                return;
            },
            _ => {
                player = if player == 'x' { 'o' } else { 'x' };
                println!("Player {}'s turn!", player);
            }
        }
    }
}

#[cfg(not(test))]
pub fn launchpad_game() {
    println!("launchpad_game");
}

/// I can't print vectors any more, so I'm using this for now
fn print_vec_usize(a: Vec<usize>) {
    for elem in a.iter() {
        print!("{} ", elem);
    }
}
