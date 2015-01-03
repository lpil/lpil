use std::fmt;

pub type Player = char;
pub type Cell = Option<Player>;

pub struct Game {
    pub board: [Cell; 9]
}

/// Returns a new with no moves taken.
///
pub fn new_game() -> Game {
    Game { board: [ None,..9 ] }
}

impl Game {
    /// Checks to see if the game has been one.
    ///
    /// * If won it returns Some(player) where player is the winning player.
    /// * If not won it returns None
    ///
    pub fn won(&self) -> Option<Player> {
        let board = self.board;
        let winning_patterns: [[uint; 3]; 8] = [
            // horizontal
            [0,1,2],
            [3,4,5],
            [6,7,8],
            // Vertical
            [0,3,6],
            [1,4,7],
            [2,5,8],
            // Diagonal
            [0,4,8],
            [6,4,2]
        ];

        for pattern in winning_patterns.iter() {
            let x = pattern[0];
            let y = pattern[1];
            let z = pattern[2];

            if board[x] == board[y] && board[y] == board[z] {
                return board[x]
            }
        }
        None
    }

    /// Returns a new instance of the game after the player makes a move at
    /// the index given.
    ///
    /// This method **does not** check if the move is valid. Invalid moves
    /// will return an identical game instance.
    ///
    pub fn make_move(&self, index: uint, player: Player) -> Game {
        let mut board = self.board;
        board[index] = Some(player);

        Game { board: board }
    }

    /// Returns a vector of all valid moves
    ///
    pub fn valid_moves(&self) -> Vec<uint> {
        let mut moves: Vec<uint> = vec![];

        for (i, cell) in self.board.iter().enumerate() {
            match cell {
                &Some(_) => continue,
                &None    => moves.push(i)
            }
        }
        moves
    }
}

impl fmt::Show for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut board_string = "".to_string();

        for (i, cell) in self.board.iter().enumerate() {
            if i == 3 || i == 6 {
                board_string.push('\n');
            }

            board_string.push(match cell {
                &Some(x) => x,
                &None    => '_'
            });
        }
        write!(f, "{}", board_string)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_games_board_is_empty() {
        assert_eq!(new_game().board,
                   [ None,..9 ] );
    }

    #[test]
    fn new_game_has_all_valid_moves() {
        assert_eq!(new_game().valid_moves(),
                   [0,1,2,3,4,5,6,7,8]);
    }

    #[test]
    fn games_dont_have_move_listed_as_valid_after_taken() {
        let game = new_game();
        for i in range(0u, 9) {
            assert!(!game.make_move(i, 'x').valid_moves().contains(&i));
        }
    }

    #[test]
    fn game_valid_moves_vec_length_decreases_as_moves_taken() {
        let mut game = new_game();
        for i in range(0u, 9) {
            game = game.make_move(i, 'x');
            assert_eq!(game.valid_moves().len(),
                       8 - i);
        }
    }

    #[test]
    fn game_with_all_cells_taken_has_no_valid_moves() {
        assert_eq!(Game { board: [ Some('x'),..9] }.valid_moves(),
                   []);
    }

    #[test]
    #[allow(unused_mut)]
    fn making_a_move_doesnt_mutate_board() {
        let mut game = new_game();
        game.make_move(0,'x');
        assert_eq!(new_game().board, game.board);
    }
}
