let greet subject =
  match subject with
  | None -> "Hello, World!"
  | Some name -> "Hello, " ^ name ^ "!"
