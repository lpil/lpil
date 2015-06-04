pub fn sort_word(word: String) -> String {
    let mut chars: Vec<_> = word.chars().collect();
    chars.sort();
    chars.iter().cloned().collect::<String>()
}


#[cfg(test)]
mod tests {
    use super::sort_word;

    #[test]
    fn sort_word_noop() {
        assert_eq!("abc".to_string(), sort_word("abc".to_string()));
    }
    #[test]
    fn sort_word_rev() {
        assert_eq!("abc".to_string(), sort_word("cba".to_string()));
    }
    #[test]
    fn sort_word_mix() {
        assert_eq!("abc".to_string(), sort_word("bac".to_string()));
    }
}
