type graphql_type =
  | Int
  | String
  | Option graphql_type
  | List graphql_type
  | Object (option string) (list (string, graphql_type));

/*
    Here be testing. Arr!
 */

let test_types_list = [
  Int,
  String,
  Option Int,
  List (Option Int),
  Object [("name", String), ("number", Int)]
];

let test_schema =
