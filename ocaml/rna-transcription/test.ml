open Core

let ( >:: ) = OUnit2.( >:: )

let char_of_variant = function
  | `A -> 'A'
  | `C -> 'C'
  | `G -> 'G'
  | `T -> 'T'
  | `U -> 'U'


let printer l = List.map ~f:char_of_variant l |> String.of_char_list

let assert_equal exp got _test_ctxt = OUnit2.assert_equal ~printer exp got

let tests =
  [ "transcribes empty list" >:: assert_equal [] (Rna_transcription.to_rna [])
  ; "transcribes cytidine"
    >:: assert_equal [`G] (Rna_transcription.to_rna [`C])
  ; "transcribes guanosine"
    >:: assert_equal [`C] (Rna_transcription.to_rna [`G])
  ; "transcribes adenosie"
    >:: assert_equal [`U] (Rna_transcription.to_rna [`A])
  ; "transcribes thymidine"
    >:: assert_equal [`A] (Rna_transcription.to_rna [`T])
  ; "transcribes multiple"
    >:: assert_equal [`U; `G; `C; `A; `C; `C; `A; `G; `A; `A; `U; `U]
          (Rna_transcription.to_rna
             [`A; `C; `G; `T; `G; `G; `T; `C; `T; `T; `A; `A]) ]


let () =
  OUnit2.run_test_tt_main (OUnit2.( >::: ) "rna-transcription tests" tests)
