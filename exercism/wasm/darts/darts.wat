(module
  (func (export "score")
    (param $x f32) (param $y f32) (result i32)

    (local $z f32)

    ;; Thank you Pythagoras!
    (local.set $z
      (f32.sqrt
        (f32.add
          (f32.mul (local.get $x) (local.get $x))
          (f32.mul (local.get $y) (local.get $y))
        )
      )
    )

    (if 
      (f32.le (local.get $z) (f32.const 1))
      (then (return (i32.const 10)))
    )
    
    (if
      (f32.le (local.get $z) (f32.const 5))
      (then (return (i32.const 5)))
    )

    (if
      (f32.le (local.get $z) (f32.const 10))
      (then (return (i32.const 1)))
    )

    (return (i32.const 0))
  )
)
