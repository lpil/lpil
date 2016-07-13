use std::char;

pub type Score = u8;

pub fn score(word: &'static str) -> Score {
    word.chars()
        .flat_map(lowercase)
        .map(char_to_score)
        .fold(0, sum)
}

fn char_to_score(c: char) -> Score {
    match c {
        'a' => 1,
        'e' => 1,
        'i' => 1,
        'o' => 1,
        'u' => 1,
        'l' => 1,
        'n' => 1,
        'r' => 1,
        's' => 1,
        't' => 1,
        'd' => 2,
        'g' => 2,
        'b' => 3,
        'c' => 3,
        'm' => 3,
        'p' => 3,
        'f' => 4,
        'h' => 4,
        'v' => 4,
        'w' => 4,
        'y' => 4,
        'k' => 5,
        'j' => 8,
        'x' => 8,
        'q' => 10,
        'z' => 10,
        _   => 0,
    }
}

fn lowercase(e: char) -> char::ToLowercase {
    e.to_lowercase()
}

fn sum(a: u8, b: u8) -> u8 {
    a + b
}
