fn is_15(num: int) -> bool {
  num % 15 == 0
}

fn is_5(num: int) -> bool {
  num % 5 == 0
}

fn is_3(num: int) -> bool {
  num % 3 == 0
}

fn main() {
  for i in range(1, 101) {
    println(
      if is_15(i)     { ~"FizzBuzz" } 
      else if is_5(i) { ~"Fizz" } 
      else if is_3(i) { ~"Buzz" } 
      else            { i.to_str() }
    );
  }
}

// Testing

// is_15
#[test]
fn is_15_with_7() {
  assert!(!is_15(7), "7 is not 15!")
}

#[test]
fn is_15_with_15() {
  assert!(is_15(15), "15 is 15!")
}

// is_5
#[test]
fn is_5_with_7() {
  assert!(!is_5(7), "7 is not 5!")
}

#[test]
fn is_5_with_5() {
  assert!(is_5(5), "5 is 5!")
}
 
// is_3
#[test]
fn is_3_with_7() {
  assert!(!is_3(7), "7 is not 3!")
}

#[test]
fn is_3_with_3() {
  assert!(is_3(3), "3 is 3!")
}
