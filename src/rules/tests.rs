extern crate rand;

use rand::Rng;
use super::*;

#[test]
fn can_print_players() {
    assert_eq!("A", format!("{}", Player::A));
    assert_eq!("B", format!("{}", Player::B));
}

#[test]
fn can_print_cells() {
    let untaken_cell = Square::Untaken;
    assert_eq!("[ ]", format!("{}", untaken_cell));
    let taken_cell = Square::Taken(Player::A);
    assert_eq!("[A]", format!("{}", taken_cell));
}

#[test]
fn can_print_games() {
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Taken(Player::A),
        Square::Untaken, Square::Untaken, Square::Taken(Player::B),
        Square::Untaken, Square::Untaken, Square::Taken(Player::A),
    ]};
    let expected = "[ ] [ ] [A]\n[ ] [ ] [B]\n[ ] [ ] [A]";
    assert_eq!(expected, format!("{}", game));
}

#[test]
fn game_new_creates_blank_game() {
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Untaken,
    ]};
    assert_eq!(game, Game::new());
}

#[test]
fn game_winner_with_winning_boards() {
    let game = Game { board: [
        Square::Taken(Player::A), Square::Taken(Player::A), Square::Taken(Player::A),
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Taken(Player::B), Square::Taken(Player::B), Square::Taken(Player::B),
        Square::Untaken, Square::Untaken, Square::Untaken,
    ]};
    assert_eq!(Some(Player::B), game.winner());
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Taken(Player::A), Square::Taken(Player::A), Square::Taken(Player::A),
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Square::Taken(Player::A), Square::Untaken, Square::Untaken,
        Square::Taken(Player::A), Square::Untaken, Square::Untaken,
        Square::Taken(Player::A), Square::Untaken, Square::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Square::Untaken, Square::Taken(Player::A), Square::Untaken,
        Square::Untaken, Square::Taken(Player::A), Square::Untaken,
        Square::Untaken, Square::Taken(Player::A), Square::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Taken(Player::B),
        Square::Untaken, Square::Untaken, Square::Taken(Player::B),
        Square::Untaken, Square::Untaken, Square::Taken(Player::B),
    ]};
    assert_eq!(Some(Player::B), game.winner());
    let game = Game { board: [
        Square::Untaken, Square::Untaken, Square::Taken(Player::A),
        Square::Untaken, Square::Taken(Player::A), Square::Untaken,
        Square::Taken(Player::A), Square::Untaken, Square::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Square::Taken(Player::B), Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Taken(Player::B), Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Taken(Player::B),
    ]};
    assert_eq!(Some(Player::B), game.winner());
}

#[test]
fn game_winner_with_new_board() {
    let game = Game::new();
    assert_eq!(None, game.winner());
}

#[test]
fn valid_moves_of_empty_board() {
    let game = Game::new();
    assert_eq!(game.valid_moves(), [0,1,2,3,4,5,6,7,8]);
}

#[test]
fn games_dont_have_move_listed_as_valid_after_taken() {
    let game = Game::new();
    for i in 0..9 {
        let new_game = game.make_move(i, Player::A);
        let moves = new_game.valid_moves();
        assert!(!moves.contains(&i));
        assert_eq!(8, moves.len());
    }
}

#[test]
fn game_make_move_associativity() {
    let mut gen = rand::thread_rng();
    let a = gen.gen_range(1, 9);
    let b = gen.gen_range(1, 9);
    let p = Player::A;
    assert_eq!(
        Game::new().make_move(a, p).make_move(b, p),
        Game::new().make_move(b, p).make_move(a, p)
        );
}

#[test]
fn game_make_move() {
    let game1 = Game { board: [
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Untaken,
        Square::Untaken, Square::Untaken, Square::Taken(Player::A),
    ]};
    let game2 = Game::new().make_move(8, Player::A);
    assert_eq!(game1, game2);
}
