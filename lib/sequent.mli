open Ast

type t = {
  premises : prop list;
  derived : prop list;
  goal : prop option;
}
(** A proof state.
    - [premises] are the user-supplied assumptions.
    - [derived] are formulas inferred from premises and other derived facts.
    - [goal] is an optional target formula we are trying to prove. *)

val empty : t
(** The empty proof state: no premises, no derived formulas, and no goal. *)

val add_premise : t -> prop -> t
(** [add_premise st p] returns a new proof state like [st] but with [p] added to
    the list of premises. *)

val add_derived : t -> prop -> t
(** [add_derived st p] returns a new proof state like [st] but with [p] added to
    the list of derived formulas, unless [p] is already known as a premise or a
    derived formula. *)

val add_goal : t -> prop -> t
(** [add_goal st p] returns a new proof state like [st] but with the goal set to
    [Some p]. *)

val apply_modus_ponens : t -> t
(** [apply_modus_ponens st] repeatedly applies Modus Ponens to the formulas in
    [st.premises] and [st.derived], adding any newly inferred formulas to
    [derived], until no more can be added. *)

val judge_goal : t -> bool
(** [judge_goal st] is [true] iff the goal of [st] is present among its premises
    or derived formulas; otherwise [false]. *)

val print_result : t -> unit
(** [print_result st] prints a human-readable summary of the proof state,
    including premises, derived formulas, the goal, and whether the goal has
    been reached. *)

val explain_derivation : t -> prop -> (prop * prop) option
(** [explain_derivation st p] tries to explain how [p] could have been derived
    by Modus Ponens from formulas in [st]. If successful, it returns
    [Some (a, imp)] where [a] is a formula and [imp] is an implication [a -> p]
    that together justify [p]. Returns [None] if no such pair can be found. *)
