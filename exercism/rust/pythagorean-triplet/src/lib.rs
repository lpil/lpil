pub fn find() -> Option<u32> {
    let max = 1000;
    for a in 1..max {
        for b in (a + 1)..(max - a) {
            let c = 1000 - (a + b);
            if a * a + b * b == c * c {
                return Some(a * b * c);
            }
        }
    }
    None
}
