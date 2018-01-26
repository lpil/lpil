open Core

let has_alpha = String.exists ~f:Char.is_alpha

let all_upper str = not (String.exists ~f:Char.is_lowercase str)

let is_silence = ( = ) ""

let is_question = String.is_suffix ~suffix:"?"

let response_for str =
  let trimmed = String.strip ~drop:Char.is_whitespace str in
  if has_alpha trimmed && all_upper trimmed then
    if is_question trimmed then "Calm down, I know what I'm doing!"
    else "Whoa, chill out!"
  else if is_question trimmed then "Sure."
  else if is_silence trimmed then "Fine. Be that way!"
  else "Whatever."
