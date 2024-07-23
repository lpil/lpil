import atomic_array
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn new_get_test() {
  let array = atomic_array.new_signed(2)

  array
  |> atomic_array.get(0)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(2)
  |> should.equal(Error(Nil))
}
