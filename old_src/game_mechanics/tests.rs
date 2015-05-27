#[cfg(test)]

use super::*;

#[test]
fn new_games_board_is_empty() {
    assert_eq!(new_game().board,
                [ None; 9 ] );
}

#[test]
fn new_game_has_all_valid_moves() {
    assert_eq!(new_game().valid_moves(),
                [0,1,2,3,4,5,6,7,8]);
}

#[test]
fn games_dont_have_move_listed_as_valid_after_taken() {
    let game = new_game();
    for i in 0us..9 {
        assert!(!game.make_move(i, 'x').valid_moves().contains(&i));
    }
}

#[test]
fn game_valid_moves_vec_length_decreases_as_moves_taken() {
    let mut game = new_game();
    for i in 0us..9 {
        game = game.make_move(i, 'x');
        assert_eq!(game.valid_moves().len(),
                    8 - i);
    }
}

#[test]
fn game_with_all_cells_taken_has_no_valid_moves() {
    assert_eq!(Game { board: [ Some('x'); 9] }.valid_moves(),
                []);
}

#[test]
#[allow(unused_mut)]
fn making_a_move_doesnt_mutate_board() {
    let mut game = new_game();
    game.make_move(0,'x');
    assert_eq!(new_game().board, game.board);
}

#[test]
fn new_game_isnt_won() {
    assert_eq!(None, new_game().won());
}

#[test]
fn test_some_won_games_are_won() {
    for moves in [
        vec![0,1,2], vec![3,4,5], vec![6,7,8], vec![0,3,6],
        vec![1,4,7], vec![2,5,8], vec![0,4,8], vec![6,4,2],
        vec![0,1,2,6,4],   vec![3,4,5,3,8,2],
        vec![6,7,8,4,6,1], vec![0,3,6,1,2,5,7]
    ].iter() {
        let mut game = new_game();
        for i in moves.iter() {
            game = game.make_move(i.clone(), 'x');
        }
        assert!(game.won() == Some('x'));
    }
}
