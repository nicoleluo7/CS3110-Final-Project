(** Abstract syntax tree for propositional formulas. *)

type prop =
  | Var of string
  | And of prop * prop
  | Imp of prop * prop
  | Not of prop

val prop_to_string : prop -> string
(** Convert a formula to a human-readable string. *)

(* Recursively handles negation for s *)
val simplify : prop -> prop