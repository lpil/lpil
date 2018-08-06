module User = Map.Make String;

let run () => {
  let users =
    User.empty |> User.add "Fred" "sugarplums" |> User.add "Tom" "ilovelucy" |>
    User.add "mark" "ocamlrules" |>
    User.add "pete" "linux";
  users
};
