// Structs are a named data structure with multiple named fields.
struct Monster {
  health: int,
  attack: int
}

// We can impl methods to structs to get something classy/objecty
impl Monster {

  // Methods are functions that take self as first arg (ruby instance methods)
  fn attack(&self) {
    println!("The monster attacks for {:d} damage", self.attack);
  }

  // We can also make associated functions (class methods in ruby)
  fn count() {
    println("There are too many monsters to count!");
  }
}

fn main() {
  let yeti = Monster { health: 9, attack: 22 };

  println(yeti.health.to_str());
  println(yeti.attack.to_str());
  println!("{:?}", yeti);
  yeti.attack();
  Monster::count();
}
