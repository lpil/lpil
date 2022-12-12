use std::collections::HashMap;

pub type Nucleotide = char; // This should be an enum, but these tests suck
pub type Counts = HashMap<Nucleotide, usize>;

pub fn nucleotide_counts(strand: &str) -> HashMap<Nucleotide, usize> {
    let mut counts: Counts = "ACTG".chars().map(|c| (c, 0)).collect();
    for c in strand.chars() {
        *counts.entry(c).or_insert(0) += 1
    }
    counts
}

pub fn count(base: Nucleotide, strand: &str) -> usize {
    strand.chars().filter(|e| e == &base).count()
}
