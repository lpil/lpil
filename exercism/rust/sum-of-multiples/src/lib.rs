pub fn sum_of_multiples(n: usize, factors: &Vec<usize>) -> usize {
    (1..n)
        .into_iter()
        .filter(|x| is_multiple(factors, x))
        .fold(0, |acc, x| x + acc)
}

fn is_multiple(factors: &Vec<usize>, num: &usize) -> bool {
    factors.iter().any(|factor| num % factor == 0)
}
