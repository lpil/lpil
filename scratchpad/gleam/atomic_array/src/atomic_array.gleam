import gleam/int

// TODO: document
pub type AtomicArray

// TODO: document
pub fn new_signed(size size: Int) -> AtomicArray {
  ffi_new_signed(int.max(size, 1))
}

// TODO: test!
// TODO: document
pub fn new_unsigned(size size: Int) -> AtomicArray {
  ffi_new_unsigned(int.max(size, 1))
}

// TODO: document
@external(erlang, "atomic_array_ffi", "get")
@external(javascript, "./atomic_array_ffi.mjs", "get")
pub fn get(array: AtomicArray, index: Int) -> Result(Int, Nil)

// TODO: document
@external(erlang, "atomic_array_ffi", "add")
@external(javascript, "./atomic_array_ffi.mjs", "add")
pub fn add(array: AtomicArray, index: Int, amount: Int) -> Result(Nil, Nil)

// TODO: document
@external(erlang, "atomic_array_ffi", "exchange")
@external(javascript, "./atomic_array_ffi.mjs", "exchange")
pub fn exchange(
  in array: AtomicArray,
  at index: Int,
  replace_with value: Int,
) -> Result(Int, Nil)

// TODO: document
@external(erlang, "atomic_array_ffi", "compare_exchange")
@external(javascript, "./atomic_array_ffi.mjs", "compare_exchange")
pub fn compare_exchange(
  in array: AtomicArray,
  at index: Int,
  expect expected: Int,
  replace_with value: Int,
) -> Result(Nil, CompareError)

pub type CompareError {
  ComparisonOutOfBounds
  ComparisonFailed(actual: Int)
}

// TODO: document
@external(erlang, "atomic_array_ffi", "size")
@external(javascript, "./atomic_array_ffi.mjs", "size")
pub fn size(array: AtomicArray) -> Int

// TODO: document
@external(erlang, "atomic_array_ffi", "set")
@external(javascript, "./atomic_array_ffi.mjs", "set")
pub fn set(array: AtomicArray, index: Int, value: Int) -> Result(Nil, Nil)

@external(erlang, "atomic_array_ffi", "new_signed")
@external(javascript, "./atomic_array_ffi.mjs", "new_signed")
fn ffi_new_signed(size size: Int) -> AtomicArray

@external(erlang, "atomic_array_ffi", "new_unsigned")
@external(javascript, "./atomic_array_ffi.mjs", "new_unsigned")
fn ffi_new_unsigned(size size: Int) -> AtomicArray
