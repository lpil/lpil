use super::GameResult;

#[derive(Debug)]
pub struct Team {
    pub name: String,
    wins: usize,
    losses: usize,
    draws: usize,
}

impl Team {
    pub fn new(name: String) -> Team {
        Team {
            name: name,
            wins: 0,
            draws: 0,
            losses: 0,
        }
    }

    pub fn register_result(&mut self, result: GameResult) {
        match result {
            GameResult::Win => self.wins += 1,
            GameResult::Loss => self.losses += 1,
            GameResult::Draw => self.draws += 1,
        }
    }

    pub fn to_tally(&self) -> String {
        format!("{:30} | {:2} | {:2} | {:2} | {:2} | {:2}",
                self.name,
                self.matches_played(),
                self.wins,
                self.draws,
                self.losses,
                self.points())
    }

    pub fn matches_played(&self) -> usize {
        self.wins + self.losses + self.draws
    }

    pub fn points(&self) -> usize {
        (self.wins * 3) + self.draws
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::GameResult;

    #[test]
    fn new() {
        let name = "Team Balloonicorn".to_string();
        let team = Team::new(name.clone());
        assert_eq!(team.name, name);
        assert_eq!(team.wins, 0);
        assert_eq!(team.losses, 0);
        assert_eq!(team.draws, 0);
    }

    #[test]
    fn register_result() {
        let mut team = Team::new("The Incredibles".to_string());
        team.register_result(GameResult::Win);
        assert_eq!(team.wins, 1);
        assert_eq!(team.losses, 0);
        assert_eq!(team.draws, 0);
        team.register_result(GameResult::Draw);
        assert_eq!(team.wins, 1);
        assert_eq!(team.losses, 0);
        assert_eq!(team.draws, 1);
        team.register_result(GameResult::Loss);
        team.register_result(GameResult::Loss);
        assert_eq!(team.wins, 1);
        assert_eq!(team.losses, 2);
        assert_eq!(team.draws, 1);
    }

    #[test]
    fn to_tally() {
        let mut team = Team::new("Tim".to_string());
        team.register_result(GameResult::Win);
        team.register_result(GameResult::Win);
        team.register_result(GameResult::Loss);
        team.register_result(GameResult::Draw);
        assert_eq!(team.to_tally(),
                   "Tim                            |  4 |  2 |  1 |  1 |  7");
    }

    #[test]
    fn matches_played() {
        let mut team = Team::new("X Men".to_string());
        assert_eq!(team.matches_played(), 0);
        team.register_result(GameResult::Win);
        assert_eq!(team.matches_played(), 1);
        team.register_result(GameResult::Loss);
        team.register_result(GameResult::Draw);
        team.register_result(GameResult::Draw);
        assert_eq!(team.matches_played(), 4);
    }

    #[test]
    fn points() {
        let mut team = Team::new("X Men".to_string());
        assert_eq!(team.points(), 0);
        team.register_result(GameResult::Win);
        assert_eq!(team.points(), 3);
        team.register_result(GameResult::Loss);
        assert_eq!(team.points(), 3);
        team.register_result(GameResult::Draw);
        assert_eq!(team.points(), 4);
        team.register_result(GameResult::Draw);
        assert_eq!(team.points(), 5);
    }
}
