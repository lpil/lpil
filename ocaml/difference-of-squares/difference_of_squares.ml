open Core

let square num = num * num

let sum_ints ?(f= fun x -> x) ints = Sequence.sum (module Int) ~f ints

let square_of_sum num = num + 1 |> Sequence.range 0 |> sum_ints |> square

let sum_of_squares num = num + 1 |> Sequence.range 0 |> sum_ints ~f:square

let difference_of_squares num = square_of_sum num - sum_of_squares num
