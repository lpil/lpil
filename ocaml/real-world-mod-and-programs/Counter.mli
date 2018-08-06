(* A collection of string frequency counts *)

type t

(* The empty set of frequency counts  *)

val empty : t

(* Increment the frequency count for the given string. *)

val inc : t -> string -> t

(* Get the count for a given key *)

val get : t -> string -> int

(* Converts the set of frequency counts to an association list.  A string shows
    up at most once, and the counts are >= 1. *)

val to_list : t -> (string * int) list
