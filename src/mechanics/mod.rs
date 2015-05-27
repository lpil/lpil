use std::fmt;

pub enum Player { A, B }

impl fmt::Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Player::A => write!(f, "A"),
            &Player::B => write!(f, "B"),
        }
    }
}

pub type Cell = Option<Player>;

pub struct Game {
    pub board: [Cell; 9]
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}\n{} {} {}\n{} {} {}", 1,2,3,4,5,6,7,8,9)
    }
}


mod tests {
    #[test]
    fn can_print_players() {
        assert_eq!("A", format!("{}", super::Player::A));
        assert_eq!("B", format!("{}", super::Player::B));
    }
}

