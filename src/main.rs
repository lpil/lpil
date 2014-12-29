type Board = [int; 9];

fn main() {
    let board: Board = [
        0,0,0,
        0,0,0,
        0,0,0
    ];

    println!("{}", is_winning_board(board));
}

fn is_winning_board(board: Board) -> bool {
    fn same_player([x, y, z]: [int; 3]) -> bool {
        x == y && y == z
    }

    same_player([1i,2i,3i])
}
