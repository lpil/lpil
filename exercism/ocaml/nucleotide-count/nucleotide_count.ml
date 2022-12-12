open Core

let count str c =
  str |> String.to_list_rev |> List.filter ~f:(( = ) c) |> List.length


let nucleotide_counts str =
  let inc x = x |> Option.value ~default:0 |> succ in
  let inc_counter = Char.Map.update ~f:inc in
  str |> String.to_list_rev |> List.fold ~init:Char.Map.empty ~f:inc_counter
