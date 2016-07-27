type Primes = Vec<usize>;

pub fn primes_up_to(max: usize) -> Vec<usize> {
    let bound = (max as f64).sqrt().ceil() as usize;
    let mut primes: Primes = vec![];

    'outer: for prime in 2..(max + 1) {
        for n in 2..bound {
            if prime != n && prime % n == 0 { continue 'outer; }
        }
        primes.push(prime);
    }
    primes
}
