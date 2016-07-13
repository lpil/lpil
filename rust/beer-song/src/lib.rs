pub fn sing(start: u8, stop: u8) -> String {
    (stop..start).rev().fold(verse(start), |acc, n| {
        acc + "\n" + &verse(n)
    })
}

pub fn verse(n: u8) -> String {
    if n == 0 {
        String::from("No more bottles of beer on the wall, no more bottles of beer.\n\
                     Go to the store and buy some more, 99 bottles of beer on the wall.\n")
    } else {
        let curr = bottles(n);
        let next = bottles(n - 1);
        format!("{0} on the wall, {0}.\n{1}, {2} on the wall.\n",
                curr, take(n), next)
    }
}

fn bottles(n: u8) -> String {
    match n {
        0 => String::from("no more bottles of beer"),
        1 => String::from("1 bottle of beer"),
        n => format!("{} bottles of beer", n),
    }
}

fn take(n: u8) -> &'static str {
    match n {
        1 => "Take it down and pass it around",
        _ => "Take one down and pass it around",
    }
}
