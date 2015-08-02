// It's hard to implement `fmt::Display` for structures where the elements
// must be handled sequentially.
// Each `write!` generates a `fmt::Result`, and we must handle them all.
//
// The `try!` macro helps with this.
// If it recieves an error, it returns, else it continues.

use std::fmt;

#[allow(dead_code)]
struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Dereference `self` to get a reference to `vec`
        let List(ref vec) = *self;

        let len = vec.len();

        // Iterate over `vec`, with index
        for (count, v) in vec.iter().enumerate() {
            // For every element except the last, format `write!` with a comma.
            // `try!` handles the errors
            if count < len - 1 {
                try!(write!(f, "{}, ", v))
            }
        }
        // Finally write the last element.
        write!(f, "{}", vec[len - 1])
    }
}

#[allow(dead_code)]
pub fn run() {
    let v = List(vec![1,2,3]);
    println!("{}", v);
    println!("It works!");
}
