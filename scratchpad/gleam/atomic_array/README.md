# atomic_array

Atomic mutable int arrays for Gleam!

[![Package Version](https://img.shields.io/hexpm/v/atomic_array)](https://hex.pm/packages/atomic_array)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/atomic_array/)

```sh
gleam add atomic_array@1
```
```gleam
import atomic_array

pub fn main() {
  let array = atomic_array.new_signed(size: 30)

  // Mutate the array
  let assert Ok(_) = atomic_array.add(array, 0, 5)
  let assert Ok(_) = atomic_array.add(array, 0, 5)

  // The array is mutated
  array
  |> atomic_array.get(0)
  |> should.equal(Ok(10))
}
```

Further documentation can be found at <https://hexdocs.pm/atomic_array>.
