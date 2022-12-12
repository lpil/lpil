open Core

type nucleotide = A | C | G | T

let hamming_distance xs ys =
  List.zip xs ys |> Option.map ~f:(List.count ~f:(fun (x, y) -> x <> y))
