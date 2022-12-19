(module
  ;; Returns 1 if leap year, 0 otherwise
  (func (export "isLeap") (param $year i32) (result i32)
    ;; Divisible by 4
    local.get $year
    i32.const 4
    call $is_divisible_by

    ;; But not divisible by 100
    local.get $year
    i32.const 100
    call $is_divisible_by
    i32.xor

    ;; Alternatively, divisible by 400
    local.get $year
    i32.const 400
    call $is_divisible_by
    i32.or
  )  

  (func $is_divisible_by (param $n i32) (param $d i32) (result i32)
    local.get $n
    local.get $d
    i32.rem_s
    i32.eqz
  )
)
