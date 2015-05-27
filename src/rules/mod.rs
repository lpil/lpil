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

#[derive(PartialEq, Debug)]
pub enum Cell {
    Untaken,
    Taken(Player),
}
impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Cell::Untaken           => write!(f, "[ ]"),
            &Cell::Taken(ref player) => write!(f, "[{}]", player),
        }
    }
}


#[derive(PartialEq, Debug)]
pub struct Game {
    pub board: [Cell; 9]
}
impl Game {
    pub fn new() -> Game {
        Game { board: [
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
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
                (&Cell::Taken(ref xx), &Cell::Taken(ref yy), &Cell::Taken(ref zz)) =>
                    if xx == yy && yy == zz { return Some(*xx) },
                _ =>
                    continue
            }
        }
        None
    }

}
impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ref b = self.board;
        write!(f, "{} {} {}\n{} {} {}\n{} {} {}",
               b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8])
    }
}
