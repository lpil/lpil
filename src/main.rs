use Cell::{Empty,X,O};

type Board = [Cell; 9];

enum Cell {
    Empty, X, O
}

impl PartialEq for Cell {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&X, &X) => true,
            (&O, &O) => true,
            _        => false
        }
    }
}

fn main() {
    let board: Board = [
        Empty,Empty,Empty,
        Empty,Empty,Empty,
        Empty,Empty,Empty
    ];
}

fn won(board: Board) -> Option<Cell> {
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
            return Some(board[x])
        }
    }
    None
}
