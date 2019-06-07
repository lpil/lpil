mod rules;
use std::io;
use std::thread;

extern crate rand;

pub fn main() {
    println!("Hello, tic tac toe!");

    let mut game = rules::Game::new();
    let mut players = [rules::Player::A, rules::Player::B];

    loop {
        let valid_moves = game.valid_moves();
        if valid_moves.len() == 0 {
            println!("It's a draw!");
            break;
        }

        thread::sleep_ms(400);
        println!("\nIt's player {}'s turn!", players[0]);
        println!("Pick a square\n{}\n", game);

        println!("Valid moves are: {:?}", valid_moves);

        let mut square = String::new();
        io::stdin().read_line(&mut square)
            .ok()
            .expect("Failed to read line");

        let square: usize = match square.trim().parse() {
            Ok(num) => num,
            Err(_)  => {
                println!("That's not a number");
                continue;
            },
        };

        if !valid_moves.contains(&square) {
            println!("That's not a valid move! Try again");
            continue;
        }
        game = game.make_move(square, players[0]);
        println!("{}", game);

        match &game.winner() {
            &None         => players = [players[1], players[0]],
            &Some(player) => {
                println!("\nPLAYER {} WINS!", player);
                break;
            }
        }
    }
}
