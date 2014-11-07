// The for in construct can be used to iterate over an iterator, a lazy value
// generator.
// range(a, b) will yield values from a (inclusive) to b (exclusive)

fn main() {
  for n in range(1, 101) {
    if n % 15 == 0 {
      println!("Fizzbuzz");
    } else if n % 3 == 0 {
      println!("Fizz");
    } else if n % 5 == 0 {
      println!("Buzz");
    } else { 
      println!("{}", n);
    }
  }
}
