extern mod extra;
use extra::comm::DuplexStream;

fn plus_one(channel: &DuplexStream <int, int>) {
  let mut x: int;
  loop {
    x = channel.recv();
    channel.send(x + 1);
  }
}

fn main() {
  let (from_child, to_child) = DuplexStream::new();

  do spawn {
    plus_one(&to_child);
  }

  for num in range(5, 11) {
    from_child.send(num);
  }

  for _ in range(0, 5) {
    println(from_child.recv().to_str())
  }
}

#[test]
fn plus_one_adds_one() {
  let (from_child, to_child) = DuplexStream::new();

  do spawn {
    plus_one(&to_child);
  }

  from_child.send(1);
  assert!(from_child.recv() == 2, ~"plus_one(1) isnt 2");

  from_child.send(2);
  assert!(from_child.recv() == 3, ~"plus_one(2) isnt 3");

  from_child.send(-1);
  assert!(from_child.recv() == 0, ~"plus_one(-1) isnt 0");
}
