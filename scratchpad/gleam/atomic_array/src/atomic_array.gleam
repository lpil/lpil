// TODO: document
pub type AtomicArray

@external(erlang, "atomic_array_ffi", "new_signed")
@external(javascript, "./atomic_array_ffi.mjs", "new_signed")
pub fn new_signed(size size: Int) -> AtomicArray

@external(erlang, "atomic_array_ffi", "new_unsigned")
@external(javascript, "./atomic_array_ffi.mjs", "new_unsigned")
pub fn new_unsigned(size size: Int) -> AtomicArray

@external(erlang, "atomic_array_ffi", "get")
@external(javascript, "./atomic_array_ffi.mjs", "get")
pub fn get(array: AtomicArray, index: Int) -> Result(Int, Nil)
