type Board = [char; 9];

fn main() {
    let board: Board = [
        '1','2','3',
        '4','5','6',
        '7','8','9'
    ];

    println!("{}", is_winning_board(board));
}

fn is_winning_board(board: Board) -> bool {
    fn same_player([x, y, z]: [char; 3]) -> bool {
        x == y && y == z
    }

    // http://projects.haskell.org/operational/examples/TicTacToe.hs.html
    same_player(['1','2','3'])
}
