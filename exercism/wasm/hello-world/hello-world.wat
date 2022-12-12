(module
  (memory (export "mem") 1)

  ;; Initializes the WebAssembly Linear Memory with a UTF-8 string of 13 characters starting at offset 0
  (data (i32.const 0) "Hello, World!")
  
  ;; Returns the base offset and length of the greeting
  (func (export "hello") (result i32 i32)
    i32.const 0
    i32.const 13 ;; "Hello, World!".length
  )
)
