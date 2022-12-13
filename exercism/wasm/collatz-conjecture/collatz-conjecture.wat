(module
  (import "console" "log_i32_u" (func $log_i32_u (param i32)))

  (func (export "steps") (param $number i32) (result i32)

    (local $steps i32)
    (local.set $steps (i32.const 0))

    ;; Numbers less than 1 are invalid
    (if 
      (i32.lt_s
        (local.get $number)
        (i32.const 1)
      )
      (then
        (return (i32.const -1))
      )
    )

    (loop $loop
      ;; If the number is not 1
      (if 
        (i32.gt_s
          (local.get $number)
          (i32.const 1)
        )
        (then
          ;; Increment the step count
          (local.set $steps
            (i32.add
              (local.get $steps)
              (i32.const 1)
            )
          )
          ;; Store the next number
          (local.set $number
            (call $next_number (local.get $number))
          )
          ;; Branch to loop again
          (br $loop)
        )
      )
    )

    (local.get $steps)
  )

  (func $next_number (param $number i32) (result i32)
    ;; If the number is even...
    (if (result i32)
      (i32.eqz
        (i32.and
          (local.get $number)
          (i32.const 1)
        )
      )
      (then
        ;; Then divide it by 2 using a bitwise shift
        (i32.shr_u
          (local.get $number)
          (i32.const 1)
        )
      )
      ;; If the number is odd then multiply it by 3 and add 1
      (else
        (i32.add 
          (i32.const 1)
          (i32.mul
            (i32.const 3)
            (local.get $number)
          )
        )
      )
    )
  )
)
