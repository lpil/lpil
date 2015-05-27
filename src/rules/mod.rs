use std::fmt;

mod tests;

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Player {
    A, B,
}
impl fmt::Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Player::A => write!(f, "A"),
            &Player::B => write!(f, "B"),
        }
    }
}


#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Square {
    Untaken,
    Taken(Player),
}
impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Square::Untaken           => write!(f, "[ ]"),
            &Square::Taken(ref player) => write!(f, "[{}]", player),
        }
    }
}


#[derive(PartialEq, Debug)]
pub struct Game {
    pub board: [Square; 9]
}
impl Game {
    pub fn new() -> Game {
        Game { board: [
            Square::Untaken, Square::Untaken, Square::Untaken,
            Square::Untaken, Square::Untaken, Square::Untaken,
            Square::Untaken, Square::Untaken, Square::Untaken,
        ]}
    }
    pub fn winner(self) -> Option<Player> {
        let board = self.board;
        let winning_patterns: [(usize, usize, usize); 8] = [
            // horizontal
            (0,1,2), (3,4,5), (6,7,8),
            // Vertical
            (0,3,6), (1,4,7), (2,5,8),
            // Diagonal
            (0,4,8), (6,4,2)
        ];
        for &(x,y,z) in winning_patterns.iter() {
            match (&board[x], &board[y], &board[z]) {
                (&Square::Taken(ref xx), &Square::Taken(ref yy), &Square::Taken(ref zz)) =>
                    if xx == yy && yy == zz { return Some(*xx) },
                _ =>
                    continue
            }
        }
        None
    }
    pub fn valid_moves(&self) -> Vec<usize> {
        println!("{:?}", self.board);
        let mut moves: Vec<usize> = vec![];
        for (i, cell) in self.board.iter().enumerate() {
            match cell {
                &Square::Taken(_) => continue,
                &Square::Untaken  => moves.push(i)
            }
        }
        println!("{:?}", moves);
        moves
    }
    pub fn make_move(&self, index: usize, player: Player) -> Game {
        let mut board = self.board.clone();
        board[index] = Square::Taken(player);
        Game { board: board }
    }

}
impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ref b = self.board;
        write!(f, "{} {} {}\n{} {} {}\n{} {} {}",
               b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8])
    }
}
