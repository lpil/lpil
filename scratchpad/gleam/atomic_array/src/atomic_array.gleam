//// Atomic mutable arrays with these properties:
////
////
//// - Atomics are 64 bit integers.
//// - Atomics can be represented as either signed or unsigned.
//// - Atomics wrap around at overflow and underflow operations.
//// - All operations guarantee atomicity. No intermediate results can be seen.
////   The result of one mutation can only be the input to one following mutation.
//// - All atomic operations are mutually ordered. If atomic B is updated after
////   atomic A, then that is how it will appear to any concurrent readers. No one
////   can read the new value of B and then read the old value of A.
//// - Indexes into atomic arrays are zero-based. An atomic array of arity N
////   contains N atomics with index from 0 to N-1.
////
//// Be aware that JavaScript numbers (and so Gleam `Int`s) cannot represent
//// all 64bit ints, so the larger and smaller values that can be contained
//// within array will lose precision when converted to a Gleam int with
//// functions such as `get` and `to_list`.

import gleam/int

/// A mutable atomic array of 64bit ints, either signed or unsigned depending
/// on whether it was created with the `new_unsigned` or `new_signed` function.
///
pub type AtomicArray

/// Create a new signed 64bit int array.
pub fn new_signed(size size: Int) -> AtomicArray {
  ffi_new_signed(int.max(size, 1))
}

/// Create a new unsigned 64bit int array.
pub fn new_unsigned(size size: Int) -> AtomicArray {
  ffi_new_unsigned(int.max(size, 1))
}

/// Read an int from the array.
///
/// Returns an error if the index is out of bounds.
///
@external(erlang, "atomic_array_ffi", "get")
@external(javascript, "./atomic_array_ffi.mjs", "get")
pub fn get(array: AtomicArray, index: Int) -> Result(Int, Nil)

/// Add an amount to an int at the given index in the array.
///
/// Returns an error if the index is out of bounds.
///
/// Will overflow or underflow if the resulting value does not fit in the int
/// size for the array. 
///
@external(erlang, "atomic_array_ffi", "add")
@external(javascript, "./atomic_array_ffi.mjs", "add")
pub fn add(array: AtomicArray, index: Int, amount: Int) -> Result(Nil, Nil)

/// Replace an int in the array with a new one.
///
/// Returns an error if the index is out of bounds.
///
@external(erlang, "atomic_array_ffi", "exchange")
@external(javascript, "./atomic_array_ffi.mjs", "exchange")
pub fn exchange(
  in array: AtomicArray,
  at index: Int,
  replace_with value: Int,
) -> Result(Int, Nil)

/// Replace an int in the array with a new one, providing that the int in the
/// array has some expected value.
///
/// Returns an error if the index is out of bounds.
///
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

/// Get the number of ints in the array.
///
/// Atomic arrays cannot be grown or shunk, they always have the same number of
/// elements as they were created with.
///
@external(erlang, "atomic_array_ffi", "size")
@external(javascript, "./atomic_array_ffi.mjs", "size")
pub fn size(array: AtomicArray) -> Int

/// Set the int at the given index to a new value.
///
/// Returns an error if the index is out of bounds.
///
/// If the new value does not fit for the size of int that the array contains
/// then it will overflow on JavaScript, but an error will be returned on
/// Erlang. If you want to make a pull request to make this consistent then we
/// will accept your changes!
///
@external(erlang, "atomic_array_ffi", "set")
@external(javascript, "./atomic_array_ffi.mjs", "set")
pub fn set(array: AtomicArray, index: Int, value: Int) -> Result(Nil, Nil)

@external(erlang, "atomic_array_ffi", "new_signed")
@external(javascript, "./atomic_array_ffi.mjs", "new_signed")
fn ffi_new_signed(size size: Int) -> AtomicArray

@external(erlang, "atomic_array_ffi", "new_unsigned")
@external(javascript, "./atomic_array_ffi.mjs", "new_unsigned")
fn ffi_new_unsigned(size size: Int) -> AtomicArray

@external(erlang, "atomic_array_ffi", "get_or_panic")
@external(javascript, "./atomic_array_ffi.mjs", "get_or_panic")
fn get_or_panic(array: AtomicArray, index: Int) -> Int

/// Convert the array to a list of ints.
///
/// Note that this operation is not atomic, so if another thread mutates the
/// array while this function is running you may see inconsistent results.
///
pub fn to_list(array: AtomicArray) -> List(Int) {
  array_to_list(array, size(array) - 1, [])
}

fn array_to_list(
  array: AtomicArray,
  index: Int,
  accumulator: List(Int),
) -> List(Int) {
  case index {
    -1 -> accumulator
    _ -> {
      let element = get_or_panic(array, index)
      array_to_list(array, index - 1, [element, ..accumulator])
    }
  }
}
