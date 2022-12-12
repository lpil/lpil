pub struct ChessPosition {
    x: u8,
    y: u8,
}

impl ChessPosition {
    pub fn new(x: isize, y: isize) -> Result<ChessPosition, String> {
        if x >= 0 && x < 8 && y >= 0 && y < 8 {
            Ok(ChessPosition {
                x: x as u8,
                y: y as u8,
            })
        } else {
            Err(String::from("Invalid Position"))
        }
    }

    pub fn same_row(&self, other: &ChessPosition) -> bool {
        self.x == other.x
    }

    pub fn same_column(&self, other: &ChessPosition) -> bool {
        self.y == other.y
    }

    fn same_diagonal(&self, other: &ChessPosition) -> bool {
        self.sum() == other.sum() || self.difference() == other.difference()
    }

    fn sum(&self) -> u8 {
        self.y + self.x
    }

    fn difference(&self) -> u8 {
        self.y - self.x
    }
}

pub struct Queen {
    position: ChessPosition,
}

impl Queen {
    pub fn new(pos: ChessPosition) -> Queen {
        Queen { position: pos }
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        self.position.same_row(&other.position) || self.position.same_column(&other.position) ||
        self.position.same_diagonal(&other.position)
    }
}
