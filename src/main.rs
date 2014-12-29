pub type Board = [Cell; 9];
pub type Player = char;
pub type Cell = Option<Player>;

fn main() {
    let board: Board = [
        None,None,None,
        None,None,None,
        None,None,None
    ];

    println!("{}", board);

    match won(board) {
        Some(x) => println!("{} wins", x),
        _ => println!("No one won")
    }
}

pub fn won(board: Board) -> Option<Player> {
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
