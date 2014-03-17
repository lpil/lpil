// Here I am passing a pointer into a function. It is an owned pointer to main
// and to test, so this function takes it as a borrowed pointer (&int). The
// pointer does not belong to it.
// The * before the var name references the pointer, so you have the value
fn plus_one(x: &int) -> int {
  *x + 1
}

fn main() {
  let x = ~10;
  println(plus_one(x).to_str());
}

#[test]
fn plus_one_test() {
  let x = ~10;
  assert!(plus_one(x) == 11, "~10 plus_one is not 11!");
}
