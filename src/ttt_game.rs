use std::fmt;

pub type Player = char;
pub type Cell = Option<Player>;

pub struct Game {
    pub board: [Cell; 9]
}

impl Game {
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

    pub fn make_move(&self, index: uint, player: Player) -> Game {
        let mut board = self.board;
        board[index] = Some(player);

        Game { board: board }
    }
}

impl fmt::Show for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.board)
    }
}
