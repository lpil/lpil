// The function return type is specified with ->

// This function returns a bool value
fn divisible_by(x: uint, y: uint) -> bool {
  if y == 0 {
    false
  } else {
    x % y == 0
  }
}

// This function does not return a value
fn fizzbuzz(num: uint) {
  if divisible_by(num, 15) {
    println!("Fizzbuzz");
  } else if divisible_by(num, 3) {
    println!("Fizz");
  } else if divisible_by(num, 5) {
    println!("Buzz");
  } else {
    println!("{}", num);
  }
}

fn main() {
  for n in range(1u, 101u) {
    fizzbuzz(n);
  }
}
