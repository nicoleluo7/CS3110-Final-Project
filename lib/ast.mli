(** Abstract syntax tree for propositional formulas. *)

type prop =
  | Var of string
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Not of prop

val prop_to_string : prop -> string
(** Convert a formula to a human-readable string.

    Preconditions:
    - It takes in [p], which is a well-formed propositional formula built from
      constructors Var, And, Or, Imp, and Not.

    Postconditions:
    - It returns a valid text representation of [p] of string type. *)

val simplify : prop -> prop
(** Recursively handles negation for s.

    Preconditions:
    - It takes in [p], which is a well-formed propositional formula built from
      constructors Var, And, Or, Imp, and Not.

    Postconditions:
    - It returns a formula that is logically equivalent to [p]. All occurrences
      of double negation [Not (Not p)] in [p] are eliminated. *)

val get_variables : prop -> string list
(** [get_variables p] returns all unique variables in [p]. *)

val depth : prop -> int
(** [depth p] returns the maximum nesting depth of [p]. *)

val size : prop -> int
(** [size p] returns the total number of nodes in [p]. *)

val is_atomic : prop -> bool
(** [is_atomic p] returns true if [p] is a single variable. *)

val is_negation : prop -> bool
(** [is_negation p] returns true if [p] is a negation. *)

val is_conjunction : prop -> bool
(** [is_conjunction p] returns true if [p] is a conjunction. *)

val is_disjunction : prop -> bool
(** [is_disjunction p] returns true if [p] is a disjunction. *)

val is_implication : prop -> bool
(** [is_implication p] returns true if [p] is an implication. *)

val count_operators : prop -> int * int * int * int
(** [count_operators p] returns (and_count, or_count, imp_count, not_count). *)

val subformulas : prop -> prop list
(** [subformulas p] returns all subformulas of [p], including [p] itself. *)

val contains : prop -> prop -> bool
(** [contains p q] returns true if [q] appears as a subformula of [p]. *)

val get_left_operand : prop -> prop option
(** [get_left_operand p] returns the left operand if [p] is binary. *)

val get_right_operand : prop -> prop option
(** [get_right_operand p] returns the right operand if [p] is binary. *)

val get_negated_formula : prop -> prop option
(** [get_negated_formula p] returns the formula inside a negation. *)

val get_antecedent : prop -> prop option
(** [get_antecedent p] returns the antecedent if [p] is an implication. *)

val get_consequent : prop -> prop option
(** [get_consequent p] returns the consequent if [p] is an implication. *)