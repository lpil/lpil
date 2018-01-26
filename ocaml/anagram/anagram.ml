open Core

let to_sorted_chars str =
  str |> String.to_list_rev |> List.sort ~cmp:Char.compare


let anagrams word candidates =
  let lowercase_word = String.lowercase word in
  let word_chars = to_sorted_chars lowercase_word in
  let is_anagram candidate =
    let lowercase_candidate = String.lowercase candidate in
    lowercase_candidate <> lowercase_word
    && word_chars = to_sorted_chars lowercase_candidate
  in
  List.filter candidates ~f:is_anagram
