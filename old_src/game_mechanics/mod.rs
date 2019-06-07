use std::fmt;

mod tests;

pub type Player = char;
pub type Cell = Option<Player>;

pub struct Game {
    pub board: [Cell; 9]
}

/// Returns a new with no moves taken.
///
pub fn new_game() -> Game {
    Game { board: [ None; 9 ] }
}

impl Game {
    /// Checks to see if the game has been one.
    ///
    /// * If won it returns Some(player) where player is the winning player.
    /// * If not won it returns None
    ///
    pub fn won(&self) -> Option<Player> {
        let board = self.board;
        let winning_patterns: [[usize; 3]; 8] = [
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

        for &[x,y,z] in winning_patterns.iter() {
            match (board[x], board[y], board[z]) {
                (Some(xx), Some(yy), Some(zz)) =>
                    if xx == yy && yy == zz { return Some(xx) },
                _ => continue
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
    pub fn make_move(&self, index: usize, player: Player) -> Game {
        let mut board = self.board;
        board[index] = Some(player);

        Game { board: board }
    }

    /// Returns a vector of all valid moves
    ///
    pub fn valid_moves(&self) -> Vec<usize> {
        let mut moves: Vec<usize> = vec![];

        for (i, cell) in self.board.iter().enumerate() {
            match cell {
                &Some(_) => continue,
                &None    => moves.push(i)
            }
        }
        moves
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut board_string = "".to_string();

        for (i, cell) in self.board.iter().enumerate() {
            if i == 3 || i == 6 {
                board_string.push('\n');
            }

            board_string.push(match *cell {
                Some(x) => x,
                None    => '_'
            });
        }
        write!(f, "{}", board_string)
    }
}
