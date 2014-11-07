// Many statements like if-else are expressions in Rust, so they return a value
// Rust is strongly typed, so each branch must return a value of the same type

// You can prevent an statement/expression from returning a value by appending
// a ; to the final expression.
// Expressions terminated with a ; will return the unit type ()

fn is_divisible_by(this: uint, that: uint) -> bool {
    if that == 0 {
        false
    } else {
        this % that == 0
    }
}

fn fizzbuzz(n: uint) {
  // expressions can be used as function/macro args
  println!("{}", if is_divisible_by(n, 15) {
      ~"Fizzbuzz"
    } else if is_divisible_by(n, 3) {
      ~"Fizz"
    } else if is_divisible_by(n, 5) {
      ~"Buzz"
    } else {
      // format! is like print! but returns the string
      format!("{}", n)
    })
}

fn main() {
  for n in range(1u, 101u) {
    fizzbuzz(n);
  }
}
