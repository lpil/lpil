// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

pub struct Robot {
    direction: Direction,
    x: isize,
    y: isize,
}

impl Robot {
    pub fn new(x: isize, y: isize, d: Direction) -> Self {
        Robot {
            direction: d,
            x: x,
            y: y,
        }
    }

    pub fn turn_right(self) -> Self {
        let d = match self.direction {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        };
        Robot::new(self.x, self.y, d)
    }

    pub fn turn_left(self) -> Self {
        let d = match self.direction {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        };
        Robot::new(self.x, self.y, d)
    }

    pub fn advance(self) -> Self {
        let (x, y) = match self.direction {
            Direction::North => (self.x, self.y + 1),
            Direction::East => (self.x + 1, self.y),
            Direction::South => (self.x, self.y - 1),
            Direction::West => (self.x - 1, self.y),
        };
        Robot::new(x, y, self.direction)
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |robot, c| {
            match c {
                'A' => robot.advance(),
                'R' => robot.turn_right(),
                'L' => robot.turn_left(),
                _ => robot,
            }
        })
    }

    pub fn position(&self) -> (isize, isize) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }
}
