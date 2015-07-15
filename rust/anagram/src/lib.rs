pub fn anagrams_for<'a>(word: &str, words: &[&'a str]) -> Vec<&'a str> {
    let sorted = sort_word(lowercase(word.to_string()));
    words.iter()
         .cloned()
         .filter(|x| x.to_string() != word)
         .filter(|x| sort_word(lowercase(x.to_string())) == sorted)
         .collect::<Vec<&'a str>>()
}


fn sort_word(word: String) -> String {
    let mut chars: Vec<_> = word.chars().collect();
    chars.sort();
    chars.iter().cloned().collect::<String>()
}

fn lowercase(word: String) -> String {
    word.chars()
        .flat_map(|c| c.to_lowercase())
        .collect::<String>()
}


#[cfg(test)]
mod tests {
    use super::sort_word;
    use super::lowercase;

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


    #[test]
    fn lowercase_noop() {
        assert_eq!("abc".to_string(), lowercase("abc".to_string()));
    }
}
