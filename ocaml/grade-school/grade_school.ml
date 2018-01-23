open Core

type school = string list Int.Map.t

let empty_school = Int.Map.empty

let add (name: string) (grade_num: int) school =
  let insert value current =
    let names = Option.value ~default:[] current in
    name :: names
  in
  Int.Map.update school grade_num ~f:(insert name)


let grade grade_num school =
  Int.Map.find school grade_num |> Option.value ~default:[]


let sort school =
  let sort = List.sort ~cmp:String.compare in
  Int.Map.map school ~f:sort


let to_map school = school
