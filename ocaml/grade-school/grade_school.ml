open Core

type school = string list Int.Map.t

let empty_school = Int.Map.empty

let insert value current =
  let values = Option.value ~default:[] current in
  value :: values


let add (name: string) (grade_num: int) school =
  Map.update school grade_num ~f:(insert name)


let grade grade_num school =
  Map.find school grade_num |> Option.value ~default:[]


let sort school =
  let sort = List.sort ~cmp:String.compare in
  Map.map school ~f:sort


let to_map school = school
