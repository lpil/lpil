// Make the `fmt` module available
use std::fmt;

// We shall implement the fmt::Display trait for this module
// This will allow us to print it with the {} marker
#[allow(dead_code)]
struct Displayable(i32);

impl fmt::Display for Displayable {
    // This trait requires the `fmt` function to be implemented with this signature
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write our first element into the supplied output stream `f`.
        // It returns `fmt::Result` which indicates whether it failed or not.
        write!(f, "{}", self.0)
    }
}

#[test]
fn it_works() {
    let x = Displayable(1);
    assert_eq!(format!("{}", x), "1")
}
