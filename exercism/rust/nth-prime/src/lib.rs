pub fn nth(n: usize) -> Result<usize, ()> {
    if n == 0 {
        return Err(());
    }

    let mut primes: Vec<usize> = vec![2];
    let mut candidate = 3;
    while primes.len() < n {
        let is_prime = primes.iter().all(|prime| candidate % prime != 0);
        if is_prime {
            primes.push(candidate);
        }
        candidate += 2;
    }
    Ok(primes[n - 1])
}
