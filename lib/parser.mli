open Ast

exception Parse_error of string
(** Raised when a string cannot be parsed as a valid formula. *)

val parse_prop : string -> prop
(** [parse_prop s] parses [s] as a propositional formula. *)
