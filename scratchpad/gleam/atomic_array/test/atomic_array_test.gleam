import atomic_array
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn new_signed_get_test() {
  let array = atomic_array.new_signed(size: 2)

  array
  |> atomic_array.get(0)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(2)
  |> should.equal(Error(Nil))

  array
  |> atomic_array.get(-1)
  |> should.equal(Error(Nil))
}

pub fn invalid_new_signed_test() {
  atomic_array.new_signed(size: -1)
}

pub fn negative_new_signed_test() {
  let array = atomic_array.new_signed(size: 1)
  let assert Ok(_) = atomic_array.add(array, 0, -1)
  let assert Ok(-1) = atomic_array.get(array, 0)
}

pub fn new_unsigned_get_test() {
  let array = atomic_array.new_unsigned(size: 2)

  array
  |> atomic_array.get(0)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))

  array
  |> atomic_array.get(2)
  |> should.equal(Error(Nil))

  array
  |> atomic_array.get(-1)
  |> should.equal(Error(Nil))
}

pub fn invalid_new_unsigned_test() {
  atomic_array.new_unsigned(size: -1)
}

pub fn negative_new_unsigned_test() {
  let array = atomic_array.new_unsigned(size: 1)
  let assert Ok(_) = atomic_array.add(array, 0, -1)
  let assert Ok(n) = atomic_array.get(array, 0)
  // It has underflowed!!
  let assert True = n > 0
}

pub fn size_test() {
  atomic_array.new_signed(size: 2)
  |> atomic_array.size
  |> should.equal(2)

  atomic_array.new_signed(size: 1)
  |> atomic_array.size
  |> should.equal(1)

  atomic_array.new_signed(size: 0)
  |> atomic_array.size
  |> should.equal(1)

  atomic_array.new_signed(size: -1)
  |> atomic_array.size
  |> should.equal(1)

  atomic_array.new_signed(size: 1000)
  |> atomic_array.size
  |> should.equal(1000)
}

pub fn set_test() {
  let array = atomic_array.new_signed(2)

  // Mutate it!!!
  array
  |> atomic_array.set(0, 50)
  |> should.equal(Ok(Nil))

  // The array is mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(50))
  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))
}

pub fn set_out_of_bound_test() {
  let array = atomic_array.new_signed(2)

  // OOB
  array
  |> atomic_array.set(2, 50)
  |> should.equal(Error(Nil))

  // The array is not mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(0))
  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))
}

pub fn add_test() {
  let array = atomic_array.new_signed(2)

  // Do a mutate
  array
  |> atomic_array.add(0, 5)
  |> should.equal(Ok(Nil))

  // The array is mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(5))
  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))

  // Do a mutate
  array
  |> atomic_array.add(0, 3)
  |> should.equal(Ok(Nil))

  // The array is mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(8))
  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))
}

pub fn add_out_of_bound_test() {
  let array = atomic_array.new_signed(2)

  // OOB
  array
  |> atomic_array.add(2, 50)
  |> should.equal(Error(Nil))

  // The array is not mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(0))
  array
  |> atomic_array.get(1)
  |> should.equal(Ok(0))
}

pub fn exchange_test() {
  let array = atomic_array.new_signed(2)

  // Do a little mutation
  array
  |> atomic_array.set(0, 50)
  |> should.equal(Ok(Nil))

  // Exchange!
  array
  |> atomic_array.exchange(0, 12)
  |> should.equal(Ok(50))

  // It was mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(12))
}

pub fn exchange_oob_test() {
  let array = atomic_array.new_signed(2)
  let assert Error(Nil) = atomic_array.exchange(array, 2, 1)
}

pub fn compare_exchange_test() {
  let array = atomic_array.new_signed(2)

  // Do a little mutation
  array
  |> atomic_array.set(0, 50)
  |> should.equal(Ok(Nil))

  // Exchange!
  array
  |> atomic_array.compare_exchange(0, 50, 12)
  |> should.equal(Ok(Nil))

  // It was mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(12))
}

pub fn compare_exchange_fail_test() {
  let array = atomic_array.new_signed(2)

  // Do a little mutation
  array
  |> atomic_array.set(0, 50)
  |> should.equal(Ok(Nil))

  // Exchange!
  array
  |> atomic_array.compare_exchange(0, 51, 12)
  |> should.equal(Error(atomic_array.ComparisonFailed(50)))

  // It was not mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(50))
}

pub fn compare_exchange_oob_test() {
  let array = atomic_array.new_signed(2)
  let assert Error(atomic_array.ComparisonOutOfBounds) =
    atomic_array.compare_exchange(array, 2, 1, 1)
}

pub fn to_list_test() {
  let array = atomic_array.new_signed(7)
  let assert Ok(_) = atomic_array.set(array, 0, 10)
  let assert Ok(_) = atomic_array.set(array, 1, 9)
  let assert Ok(_) = atomic_array.set(array, 2, 8)
  let assert Ok(_) = atomic_array.set(array, 3, 7)
  let assert Ok(_) = atomic_array.set(array, 4, 6)
  let assert Ok(_) = atomic_array.set(array, 5, 5)
  let assert Ok(_) = atomic_array.set(array, 6, 4)
  atomic_array.to_list(array)
  |> should.equal([10, 9, 8, 7, 6, 5, 4])
}
