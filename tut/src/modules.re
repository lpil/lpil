let integer = Modules_a.integer;

let print_things = {
  open Printf;
  let my_data = ["a", "beautiful", "day"];
  List.iter (printf "%s\n") my_data
};

module Nested = {
  let integer = 123;
};

module type MyModuleSig = {type some_type = int; let integer: int;};

module MyModule: MyModuleSig = {
  type some_type = int;
  let private_thing = 50;
  let integer = private_thing;
};

module MyInt = {
  type t = int;
  let compare _ _ => 1;
};

module Int_set_1 = Set.Make MyInt;

module Int_set_2 =
  Set.Make {
    type t = int;
    let compare _ _ => 1;
  };
