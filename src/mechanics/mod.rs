use std::fmt;

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


pub struct Game {
    pub board: [Cell; 9]
}
impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ref b = self.board;
        write!(f, "{} {} {}\n{} {} {}\n{} {} {}",
               b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8])
    }
}


mod tests {
    use super::*;

    #[test]
    fn can_print_players() {
        assert_eq!("A", format!("{}", Player::A));
        assert_eq!("B", format!("{}", Player::B));
    }

    #[test]
    fn can_print_cells() {
        let untaken_cell = Cell::Untaken;
        assert_eq!("[ ]", format!("{}", untaken_cell));
        let taken_cell = Cell::Taken(Player::A);
        assert_eq!("[A]", format!("{}", taken_cell));
    }

    #[test]
    fn can_print_games() {
        let game = Game { board: [
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::B),
            Cell::Untaken, Cell::Untaken, Cell::Taken(Player::A),
        ]};
        let expected = "[ ] [ ] [A]\n[ ] [ ] [B]\n[ ] [ ] [A]";
        assert_eq!(expected, format!("{}", game));
    }
}

