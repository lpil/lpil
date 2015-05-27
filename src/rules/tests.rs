use super::*;

#[test]
fn can_print_players() {
    assert_eq!("A", format!("{}", Player::A));
    assert_eq!("B", format!("{}", Player::B));
}

#[test]
fn can_print_cells() {
    let untaken_cell = Cell::Untaken;
    assert_eq!("[ ]", format!("{}", untaken_cell));
    let taken_cell = Cell::Taken(Player::A);
    assert_eq!("[A]", format!("{}", taken_cell));
}

#[test]
fn can_print_games() {
    let expected = "[ ] [ ] [A]\n[ ] [ ] [B]\n[ ] [ ] [A]";
    assert_eq!(expected, format!("{}", Game::new()));
}

#[test]
fn game_new_creates_blank_game() {
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
    ]};
    assert_eq!(game, Game::new());
}

#[test]
fn game_winner_with_winning_boards() {
    let game = Game { board: [
        Cell::Taken(Player::A), Cell::Taken(Player::A), Cell::Taken(Player::A),
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Taken(Player::B), Cell::Taken(Player::B), Cell::Taken(Player::B),
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
    ]};
    assert_eq!(Some(Player::B), game.winner());
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Taken(Player::A), Cell::Taken(Player::A), Cell::Taken(Player::A),
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Cell::Taken(Player::A), Cell::Untaken, Cell::Untaken,
        Cell::Taken(Player::A), Cell::Untaken, Cell::Untaken,
        Cell::Taken(Player::A), Cell::Untaken, Cell::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Cell::Untaken, Cell::Taken(Player::A), Cell::Untaken,
        Cell::Untaken, Cell::Taken(Player::A), Cell::Untaken,
        Cell::Untaken, Cell::Taken(Player::A), Cell::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
    ]};
    assert_eq!(Some(Player::B), game.winner());
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
        Cell::Untaken, Cell::Taken(Player::A), Cell::Untaken,
        Cell::Taken(Player::A), Cell::Untaken, Cell::Untaken,
    ]};
    assert_eq!(Some(Player::A), game.winner());
    let game = Game { board: [
        Cell::Taken(Player::B), Cell::Untaken, Cell::Untaken,
        Cell::Untaken, Cell::Taken(Player::B), Cell::Untaken,
        Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
    ]};
    assert_eq!(Some(Player::B), game.winner());
}

#[test]
fn game_winner_with_new_board() {
    let game = Game { board: [
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
        Cell::Untaken, Cell::Untaken, Cell::Untaken,
    ]};
    assert_eq!(None, game.winner());
}
