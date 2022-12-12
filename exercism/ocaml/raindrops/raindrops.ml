open Core

let sound n div str = match n mod div with 0 -> str | _ -> ""

let raindrop n =
  let sounds = sound n 3 "Pling" ^ sound n 5 "Plang" ^ sound n 7 "Plong" in
  match sounds with "" -> string_of_int n | _ -> sounds
