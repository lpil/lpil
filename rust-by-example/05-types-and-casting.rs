// Rust provides type safety via it's static type-checker.
//
// Variables can be type annotated using a : after the varible name in the let
// statement, however in most cases the compiler will be able to infer the type
// of the value from the constant.
//
// Type conversion (aka casting) must be explicitly stated using the as keyword

fn main() {
  let decimal = 65.4321;
  let integer = decimal as u8;
  let charact = integer as char;

  println!("Casting: {} -> {} -> {}", decimal, integer, charact);
}
