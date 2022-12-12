open Core

let transform input =
  let letter_tuple score letter = (Char.lowercase letter, score) in
  let expand (score, letters) = List.map ~f:(letter_tuple score) letters in
  let compare_tuple (a, _) (b, _) = Char.compare a b in
  input |> List.map ~f:expand |> List.concat |> List.sort ~cmp:compare_tuple
