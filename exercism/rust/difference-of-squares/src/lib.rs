pub fn square_of_sum(n: u64) -> u64 {
    (1..n + 1).fold(0, |acc, e| acc + e).pow(2)
}

pub fn sum_of_squares(n: u64) -> u64 {
    (1..n + 1).fold(0, |acc, e| acc + e.pow(2))
}

pub fn difference(n: u64) -> u64 {
    square_of_sum(n) - sum_of_squares(n)
}
