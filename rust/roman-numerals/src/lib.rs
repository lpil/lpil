#[derive(Debug)]
pub struct Roman(usize);

impl Roman {
    pub fn from(value: usize) -> Roman {
        Roman(value)
    }

    pub fn to_string(&self) -> String {
        let Roman(value) = *self;
        convert(value, &mut String::new()).clone()
    }
}

static RULES: [(usize, &'static str); 13] = [
    (1000, "M" ),
    (900,  "CM"),
    (500,  "D" ),
    (400,  "CD"),
    (100,  "C" ),
    (90,   "XC"),
    (50,   "L" ),
    (40,   "XL"),
    (10,   "X" ),
    (9,    "IX"),
    (5,    "V" ),
    (4,    "IV"),
    (1,    "I" ),
];

fn convert(n: usize, acc: &mut String) -> &mut String {
    let rule = RULES.iter().find(|&&(i, _)| n >= i);
    match rule {
        Some(&(i, s)) => {
            acc.push_str(s);
            convert(n - i, acc)
        }
        None =>
            acc
    }
}
