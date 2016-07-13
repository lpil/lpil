use std::collections::HashMap;

pub type Word = String;
pub type WordCount = HashMap<Word, u32>;

pub fn word_count(sentence: &str) -> WordCount {
    let mut count = HashMap::new();
    let lowercase = sentence.to_lowercase();
    let words = lowercase
        .split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty());
    for word in words {
        *count.entry(String::from(word)).or_insert(0) += 1;
    }
    count
}
