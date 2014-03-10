fn main() {
  for i in range(0, 30) {
    do spawn {
      println!("Hello from thread {:?}", i)
    }
  }
}
