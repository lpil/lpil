pub fn reply(msg: &'static str) -> &'static str {
    if msg == "" {
        return "Fine. Be that way!"
    }

    let mut all_uppercase = true;
    for c in msg.chars() {
        if all_uppercase && c.is_alphabetic() {
            all_uppercase = c.is_uppercase();
        }
    }

    if all_uppercase {
        "Whoa, chill out!"
    } else if msg.ends_with("?") {
        "Sure."
    } else {
        "Whatever."
    }
}
