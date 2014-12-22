// Compound data types

fn foo() {
  // This is a tuple
  //   An ordered list of fixed size
  let tup_x = (1i, "hello");

  // Here's the same declaration with the type annotated
  let tup_y: (int, &str) = (1, "hello");

  // We can access the contents with destructuring/pattern matching
  let (x, y, z) = (1i, 2i, 3i);
}

// Structs!

// Convention is for them to be CamelCase
struct Point {
  x: int,
  y: int,
}

fn struct_play() {
  // This is how we initialize them
  let mut point = Point { x: 0i, y: 1i };

  point.x = 5i;

  println!("The point is at {}, {}", point.x, point.y);
}

pub fn main() {
}


// There is also this middle ground called tuple structs

fn tuple_struct_play() {
  struct Colour(int, int, int);
  // Generally there's not much reason to use them.
}


// Enums!
enum Ordering {
  Less,
  Equal,
  Greater,
}
// Here an Ordering can be one of Less, Equal, or Greater

fn compare_ordering(a: int, b: int) -> Ordering {
  if a < b {
    Less
  } else if a > b {
    Greater
  } else {
    Equal
  }
}
