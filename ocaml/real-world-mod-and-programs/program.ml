open Core

let x = Counter.empty

let () =
  In_channel.fold_lines stdin ~init:Counter.empty ~f:Counter.inc
  |> Counter.to_list
  |> List.sort ~cmp:(fun (_, x) (_, y) -> Int.descending x y)
  |> Fn.flip List.take 10
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
