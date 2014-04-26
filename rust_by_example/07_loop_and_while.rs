fn main() {
  let mut count = 0;

  println!("Let's count until infinity!");

  // infinite loop
  loop {
    count += 1;

    if count == 3 {
      // skip iteration
      continue
    }

    println!("{}", count);

    if count == 5 {
      println!("OK, that's enough");
      // exit the loop
      break
    }
  }


  // While loop
  while count < 50 {
    print!("{} ", count);
    count += 1;
  }
  println!("");
}
