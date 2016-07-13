#[macro_use]
extern crate lazy_static;

use std::iter::FromIterator;
use std::collections::HashSet;
use std::ascii::AsciiExt;

lazy_static! {
    static ref ALPHABET: HashSet<char> =
        HashSet::from_iter("abcdefghijklmnopqrstuvwxyz".chars());
}

pub fn is_pangram(sentence: &&str) -> bool {
    let iter = sentence
        .chars()
        .filter(|e| e.is_ascii())
        .filter(|e| e.is_alphabetic())
        .flat_map(|e| e.to_lowercase());

    let letters = HashSet::from_iter(iter);
    letters == *ALPHABET
}
