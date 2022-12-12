open Core

type dna = [`A | `C | `G | `T]

type rna = [`A | `C | `G | `U]

let to_rna dna =
  let to_rna_base = function `G -> `C | `C -> `G | `T -> `A | `A -> `U in
  List.map ~f:to_rna_base dna
