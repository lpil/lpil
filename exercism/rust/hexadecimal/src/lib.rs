pub fn hex_to_int(hex: &str) -> Option<usize> {
    let mut counter = 0;
    let hex_length = hex.len() - 1;
    for (i, c) in hex.chars().enumerate() {
        match hex_base_to_int(c, hex_length - i) {
            Some(value) => counter += value,

            None => return None,
        }
    }
    Some(counter)
}

fn hex_base_to_int(c: char, base_index: usize) -> Option<usize> {
    let multiplier = (16 as f64).powi(base_index as i32) as usize;

    match hex_digit_to_int(c) {
        Some(value) => Some(multiplier * value),

        None => None,
    }
}

fn hex_digit_to_int(c: char) -> Option<usize> {
    match c {
        'f' => Some(15),
        'e' => Some(14),
        'd' => Some(13),
        'c' => Some(12),
        'b' => Some(11),
        'a' => Some(10),
        '9' => Some(9),
        '8' => Some(8),
        '7' => Some(7),
        '6' => Some(6),
        '5' => Some(5),
        '4' => Some(4),
        '3' => Some(3),
        '2' => Some(2),
        '1' => Some(1),
        '0' => Some(0),
        _ => None,
    }
}
