(* A module with functions to test *)
module Mod = struct
  let capit letter = Char.uppercase_ascii letter

  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

(* The tests *)

let test_set =
  [ ( "Capitalize"
    , `Quick
    , fun () -> Alcotest.(check char) "same chars" 'A' (Mod.capit 'b') )
  ; ( "Add entries"
    , `Slow
    , fun () -> Alcotest.(check int) "same ints" 7 (Mod.plus [1; 1; 2; 3]) ) ]


(* Run it *)
let () = Alcotest.run "My first test" [("test_set", test_set)]
