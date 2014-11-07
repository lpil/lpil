// Tuples are collections of values of different types.
// They look like this: (1u, "String", 2i, 3.455f32)
//
// Functions can use tuples to return multiple values

fn chimpanzees() -> (int, &str) {
  (6, "Pan troglodytes")
}

fn elephants() -> (int, &str) {
    (9, "Elephas maximus")
}

fn penguins() -> (int, &str) {
    (4, "Spheniscus demersus")
}

fn wolves() -> (int, &str) {
    (6, "Canis lupus")
}

// Tuples can be used as function args
fn show(pair: (int, &str)) {
  // Destructuring a tuple
  let (amount, species) = pair;

  println!("There are {} {}", amount, species);
}

fn main() {
  // A tuple with loads of different types in it!
  let big_tuple = (1u8, 2u16, 3u32, 4u64, -1i8, -2i16, -3i32, -4i64, 0.1f32,
                   0.2f64, 'a', "abc");

  // Values can be extracted from tuples using the .val# methods
  println!("Long tuple first value: {}",  big_tuple.val0());
  println!("Long tuple second value: {}", big_tuple.val1());

  // `let` can be used to bind a tuple members to variables
  let pair = (3, 4);
  let (x, y) = pair;
  println!("x is {}, and y is {}", x, y);

  // You can also print a tuple directly
  println!("pair is {}", pair);

  // Tuples can contain tuples
  let tuple_of_tuples = ((1u8, 2u16, 2u32), (4u64, -1i8), -2i16);
  println!("Inception! - {}", tuple_of_tuples);

  println!("Animal inventory");
  show(chimpanzees());
  show(elephants());
  show(penguins());
  show(wolves());
}
