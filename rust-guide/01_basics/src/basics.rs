// Check it. There's a main function
pub fn main() {

  // Assignment!
  let a = 1i;

  // Pattern matching!
  let (b, c) = (2i, 3i);

  // Explicit types!
  let d: int = 4;

  // Those were are all immutable assignments
  // if you want mutability you have to use the `mut` keyword
  let mut e = 4i;
  e = e + 1;

  // Hey, look. Slightly uncomfortable string interpolation syntax.
  println!("{} {} {} {} {}", a, b, c, d, e);

  // You can declare variables without assigning them
  // This will throw a warning, since the compiler cannot tell the type
  //    let x;
  //
  // This is OK
  //    let x: int;

  // Ifs are expressions, yay!
  let f: int = if true { 6 } else { 1498724398 };
  //                                              ^
  // Semicolon when it's an expression returning a value

  // let is not an expression. This won't compile
  //    let x = (let y = 5i);

  // Let's use the functions defined below
  print_number(f);
  print_number(inc(f));
  print_number(inc_if_small(f));
  print_sum(a, b);
}

// Hey look, a function that takes args
// The types for the args are mandatory
fn print_number(x: int) {
  println!("The number to be printed is {}", x);
}

fn print_sum(x: int, y: int) {
  println!("The sum of the numbers passed is {}", x + y);
}

// We can return values
// Leave off the semicolon and we get an implicit return
fn inc(x: int) -> int {
  x + 1
}

// We can still return early with the return keyword if we want
fn inc_if_small(x: int) -> int {
  if x < 100 { return x; }
  
  x + 1
}
