pub fn hamming_distance(a: &'static str, b: &'static str) -> Result<u8, &'static str> {
    if a.len() != b.len() {
        Err("inputs of different length")
    } else {
        Ok(a.chars().zip(b.chars()).fold(0, inc_if_different))
    }
}

fn inc_if_different(acc: u8, chars: (char, char)) -> u8 {
    let (c1, c2) = chars;
    if c1 == c2 {
        acc
    } else {
        acc + 1
    }
}
