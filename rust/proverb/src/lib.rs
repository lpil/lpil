pub fn build_proverb(list: Vec<&str>) -> String {
    let mut lines: Vec<String> = Vec::with_capacity(list.len());
    let mut iter = list.iter();

    let mut want = iter.next().unwrap_or(&"break");

    for s in iter {
        let line = format!("For want of a {} the {} was lost.", want, s);
        lines.push(line);
        want = s;
    }

    if let Some(origin) = list.first() {
        lines.push(format!("And all for the want of a {}.", origin));
    }

    lines.join("\n")
}
