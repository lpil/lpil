open Core

let ( |> ) x f = f x

let identity x = x

let square num = num * num

let square_of_sum num =
  num + 1 |> List.range 0 |> List.fold_left ~init:0 ~f:( + ) |> square


let sum_of_squares num =
  num + 1 |> List.range 0 |> List.map ~f:square
  |> List.fold_left ~init:0 ~f:( + )


let difference_of_squares num = square_of_sum num - sum_of_squares num
