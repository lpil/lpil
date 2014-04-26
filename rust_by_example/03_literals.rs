// Primitive types in rust.
// signed ints:    i8, i16, i32, i64, and  int (machine word size)
// unsigned ints:  u8, u16, u32, u64, and uint (machine word size)
// floats:        f32, f64
//
// These will suffix a value to indicate type,
// except uint and int, which use the i an u suffixes respectively
//
// ints can be expressed in hexadecimal, octal, or binary using these prefixes
//  0x, 0o, 0b


fn main() {
  // Integer addition
  println!("1 + 2 = {}", 1u + 2u);
  
  // Float division
  println!("1.0 / 2.0 = {}", 1f32 / 2f32);

  // short-circuiting boolean logic
  println!("true AND false is {}", true && false);
  println!("true OR false is {}", true || false);
  println!("NOT true is {}", !true);

  // bitwise operations
  println!("100 XOR 001 is {:t}", 0b100 ^ 0b001);
}
