(** Abstract syntax tree for propositional formulas. *)

type prop =
  | Var of string
  | And of prop * prop
  | Imp of prop * prop
  | Not of prop

val prop_to_string : prop -> string
(** Convert a formula to a human-readable string.

    Preconditions:
    - It takes in [p], which is a well-formed propositional formula built from
      constructors Var, And, Imp, and Not.

    Postconditions:
    - It returns a valid text representation of [p] of string type. *)

val simplify : prop -> prop
(** Recursively handles negation for s.

    Preconditions:
    - It takes in [p], which is a well-formed propositional formula built from
      constructors Var, And, Imp, and Not.

    Postconditions:
    - It returns a formula that is logically equivalent to [p]. All occurrences
      of double negation [Not (Not p)] in [p] are eliminated. *)