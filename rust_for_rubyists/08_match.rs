fn identify_num(i: int) -> ~str {
  match i {
    0 => ~"zero",
    1 => ~"one",
    2 => ~"two",
    3 => ~"three",
    _ => ~"I dunno!" // catch all
  }
}

fn main() {
  println(identify_num(0));
  println(identify_num(1));
  println(identify_num(2));
  println(identify_num(3));
  println(identify_num(4));
}

////////////////////
// => Testing! <= //
////////////////////

#[test]
fn test_identify_num() {
  assert!(identify_num(0) == ~"zero",     "0 should be zero");
  assert!(identify_num(1) == ~"one",      "1 should be 1");
  assert!(identify_num(2) == ~"two",      "2 should be two");
  assert!(identify_num(3) == ~"three",    "3 should be three");
  assert!(identify_num(4) == ~"I dunno!", "4 should be I dunno!");
}
