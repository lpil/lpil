fn channel_thing() -> ~str {
  let (port, chan): (Port<~str>, Chan<~str>) = Chan::new();

  do spawn {
    chan.send(~"Hello!"); // Send this value from this thread to the port
  }
  port.recv() // As with Ruby, the last expression is returned, providing
}             // that is does not end with a ';' character

fn main() {
  println!("The other thread says: '{}'", channel_thing());
}

// Testing!

#[test]
fn channel_thing_test() {
  assert!(channel_thing() == "Hello!", "channel_thing should return Hello!")
}
