fn main() {
  // print! is like println! but without the newline at the end
  print!("January has ");

  // {} are placeholders for args that will be stringified
  println!("{} days", 31);

  // The positional args can be reused along the string
  println!("{0}, this is {1}. {1} this is {0}", "Alice", "Bob");

  // name args can also be used
  println!("{subject} {verb} {predictate}",
          predictate="over the lazy dog",
          subject="the quick brown fox",
          verb="jumps");

  // Special formatting can be specified in the placeholder after a ':'
  println!("{} of {:t} people know binary, the other half don't", 1, 2);
}
