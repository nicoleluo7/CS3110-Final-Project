open Ast

exception Parse_error of string
(** Raised when a string cannot be parsed as a valid formula. *)

val parse_prop : string -> prop
(** [parse_prop s] parses [s] as a propositional formula.

    Preconditions:
    - [s] is a non-empty string representing a formula using variables,
      parentheses, '!', '->', '&', and '|'.

    Postconditions:
    - It returns the propositional formula represented by [s], simplified by
      [simplify].
    - It raises [Parse_error] if [s] is not a valid formula.
    
    Operator precedence (weakest to strongest): ->, &, | *)
