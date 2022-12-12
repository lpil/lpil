mod team;

use std::collections::HashMap;
use std::cmp::Ordering;
use team::Team;

pub type CSV = String;
pub type Tally = String;

type Teams = HashMap<String, Team>;

#[derive(Debug)]
#[derive(Clone)]
pub enum GameResult {
    Win,
    Loss,
    Draw,
}

pub fn tally(csv: &CSV) -> Tally {
    let teams = csv.lines()
        .flat_map(row_to_info)
        .fold(HashMap::new(), |acc, (name_a, name_b, result)| {
            let acc = register_result(acc, name_a, result.clone());
            register_result(acc, name_b, flip_result(result))
        });
    let teams: Vec<_> = teams.values().collect();
    let teams = sort_teams(teams);
    let body: String = teams.iter()
        .map(|e| e.to_tally())
        .collect::<Vec<_>>()
        .join("\n");
    format!("{:30} | MP |  W |  D |  L |  P\n{}", "Team", body)
}

type ResultInfo = Option<(String, String, GameResult)>;

fn row_to_info(row: &str) -> ResultInfo {
    let data: Vec<&str> = row.split(";").take(3).collect();
    if data.len() != 3 {
        return None;
    }
    match data[2] {
        "win" => Some((data[0].to_string(), data[1].to_string(), GameResult::Win)),
        "draw" => Some((data[0].to_string(), data[1].to_string(), GameResult::Draw)),
        "loss" => Some((data[0].to_string(), data[1].to_string(), GameResult::Loss)),
        _ => None,
    }
}

fn flip_result(result: GameResult) -> GameResult {
    match result {
        GameResult::Win => GameResult::Loss,
        GameResult::Draw => GameResult::Draw,
        GameResult::Loss => GameResult::Win,
    }
}

fn register_result(mut teams: Teams, name: String, result: GameResult) -> Teams {
    teams.entry(name.clone())
        .or_insert(Team::new(name))
        .register_result(result);
    teams
}

fn sort_teams(mut teams: Vec<&Team>) -> Vec<&Team> {
    teams.sort_by(|x, y| {
        match y.points().cmp(&x.points()) {
            Ordering::Equal => x.name.cmp(&y.name),
            ordering => ordering,
        }
    });
    teams
}
