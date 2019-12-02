trait Error {
    fn pretty_print(&self) -> String;
}

impl<'a, E: Error + 'a> From<E> for Box<dyn Error + 'a> {
    fn from(error: E) -> Box<dyn Error + 'a> {
        Box::new(error)
    }
}

struct One {}
impl Error for One {
    fn pretty_print(&self) -> String {
        "One".to_string()
    }
}

struct Two {}
impl Error for Two {
    fn pretty_print(&self) -> String {
        "Two".to_string()
    }
}

struct Three {}
impl Error for Three {
    fn pretty_print(&self) -> String {
        "Three".to_string()
    }
}

type ProjectResult<Value> = Result<Value, Box<dyn Error>>;

fn main() {
    if let Err(e) = top() {
        println!("{}", e.pretty_print());
    }
}

fn top() -> ProjectResult<()> {
    if is_time() {
        one()
    } else if is_time() {
        two()
    } else {
        three()
    }
}

fn is_time() -> bool {
    true
}

fn one() -> ProjectResult<()> {
    Err(One {}.into())
}

fn two() -> ProjectResult<()> {
    Err(Two {}.into())
}

fn three() -> ProjectResult<()> {
    Err(Three {}.into())
}
