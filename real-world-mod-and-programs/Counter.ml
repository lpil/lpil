open Core

type t = (string, int) List.Assoc.t

let empty = []

let get counter key =
  List.Assoc.find ~equal:( = ) counter key |> Option.value ~default:0


let inc counter key =
  key |> get counter |> succ |> List.Assoc.add ~equal:( = ) counter key


let to_list x = x
